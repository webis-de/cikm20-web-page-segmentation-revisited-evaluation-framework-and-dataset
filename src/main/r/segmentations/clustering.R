library(raster) # for image masks

min.annotators.default <- 3
hclust.method.default <- "average"
hclust.disagreement.thresholds.default <- c(0.1, 0.3, 0.5, 0.7, 0.9)
image.mask.buffer.default <- 1
size.function.default <- "area"
precision.default <- 1

Clusterings <- function(task, segment.size.functions = list(AreaSegmentSizeFunction(), CannySegmentSizeFunction(task, name="edges-fine"), CannySegmentSizeFunction(task, name="edges-coarse")), xpath.node.size.functions = list(IdentityXPathNodeSizeFunction(), CharactersXPathNodeSizeFunction(task)), nodes = ReadNodes(task), precision = precision.default) {
  pixel.clusterings <- PixelBasedClusterings(task$segmentations, segment.size.functions = segment.size.functions, precision = precision)
  node.clusterings <- NodeBasedClusterings(task$segmentations, nodes = nodes, xpath.node.size.functions = xpath.node.size.functions)
  return(c(pixel.clusterings, node.clusterings))
}

Clustering.task <- function(task, size.function = size.function.default, precision = precision.default, ...) {
  if (length(grep("^canny-", size.function)) > 0) {
    canny.parameters <- as.numeric(strsplit(sub("canny-0x", "", size.function), "-")[[1]])
    size.function <- "canny"
    canny.sigma <- canny.parameters[1]
    canny.lower.threshold <- canny.parameters[2]
    canny.upper.threshold <- canny.parameters[3]
  }

  if (size.function == "pixels" || size.function == "area") {
    return(PixelBasedClusterings(task$segmentations, segment.size.functions = list(AreaSegmentSizeFunction()), precision = precision, ...)[[1]])
  } else if (size.function == "canny") {
    return(PixelBasedClusterings(task$segmentations, segment.size.functions = list(CannySegmentSizeFunction(task, canny.sigma, canny.upper.threshold)), precision = precision, ...)[[1]])
  } else if (size.function == "edges-fine") {
    return(PixelBasedClusterings(task$segmentations, segment.size.functions = list(CannySegmentSizeFunction(task, name = "edges-fine")), precision = precision, ...)[[1]])
  } else if (size.function == "edges-coarse") {
    return(PixelBasedClusterings(task$segmentations, segment.size.functions = list(CannySegmentSizeFunction(task, name = "edges-coarse")), precision = precision, ...)[[1]])
  } else if (size.function == "nodes" || size.function == "identity") {
    return(NodeBasedClustering(task$segmentations, ReadNodes(task), xpath.node.size.function = IdentityXPathNodeSizeFunction(), ...))
  } else if (size.function == "chars" || size.function == "ncharacters") {
    return(NodeBasedClustering(task$segmentations, ReadNodes(task), xpath.node.size.function = CharactersXPathNodeSizeFunction(task), ...))
  } else {
    stop(paste("Unknown size.function:", size.function))
  }
}

Clustering <- function(cluster.multipolygons, membership.matrix, membership.subsets = list(1:(dim(membership.matrix)[2])), sizes = rep(1, length(cluster.multipolygons))) {
  num.clusters <- length(cluster.multipolygons)
  if (num.clusters != dim(membership.matrix)[1]) {
    stop(paste("Got", num.clusters, "multipolygons, but", dim(membership.matrix)[1], "rows in the membership matrix (must be the same)"))
  }
  if (num.clusters != length(sizes)) {
    stop(paste("Got", num.clusters, "multipolygons, but", length(sizes), "sizes (must be the same)"))
  }

  num.segments <- dim(membership.matrix)[2]
  membership.found <- c()
  if (!is.list(membership.subsets)) {
    stop(paste("membership.subsets has to be a list of integer vectors, but was", paste(class(membership.subsets), collapse=",")))
  } else {
    for (membership.subset in membership.subsets) {
      if (!is.vector(membership.subset)) {
        stop(paste("Each membership.subset has to be a vector (of integer), but one was", paste(class(membership.subset), collapse=",")))
      }
      if (!is.integer(membership.subset)) {
        stop(paste("Each membership.subset has to be an integer (vector), but one was", paste(class(membership.subset), collapse=",")))
      }
      out.of.bounds <- membership.subset < 0 | membership.subset > num.segments
      if (any(out.of.bounds)) {
        stop(paste("Each membership value has to correspond to a segment index (1 to ", num.segments, "), but some are ", paste(membership.subset[out.of.bounds], collapse=","), sep=""))
      }
      membership.found <- c(membership.found, membership.subset)
    }
  }
  duplicates <- which(duplicated(membership.found))
  if (length(duplicates) > 0) {
    stop(paste("membership.subsets overlap at", paste(membership.found[duplicates], collapse=",")))
  }
  membership.found <- sort(membership.found)

  clustering <- list()

  MakeCluster <- function(index) {
    return(Cluster(cluster.multipolygons[[index]], membership.matrix[index,membership.found], sizes[index]))
  }
  if (num.clusters > 0) {
    clustering$clusters <- lapply(1:num.clusters, MakeCluster)
  } else {
    clustering$clusters <- list()
  }

  ConvertMembershipIndices <- function(membership.subset) {
    return(which(membership.found %in% membership.subset))
  }
  clustering$membership.subsets <- lapply(membership.subsets, ConvertMembershipIndices)

  class(clustering) <- c("clustering", "list")
  return(clustering)
}

is.clustering <- function(clustering) {
  return(inherits(clustering, "clustering"))
}

length.clustering <- function(clustering) {
  return(length(clustering$clusters))
}

plot.clustering <- function(clustering, jitter = NA, col = c("green", "blue", "purple", "orange", "red", "cyan", "salmon"), ...) {
  if (is.na(jitter)) {
    jitter <- length(clustering$clusters) > 1
  }

  clusters.count <- length(clustering$clusters)
  for (cl in 1:clusters.count) {
    col.index <- ((cl - 1) %% length(col)) + 1
    plot(clustering$clusters[[cl]], col = col[col.index], jitter = jitter, ...)
  }
}

as.raster.clustering <- function(clustering, screenshot, as.rgb = (dim(screenshot)[3] > 1)) {
  dimensions <- dim(screenshot)
  layers <- 1
  if (as.rgb) { layers <- 1:3 }
  raster.screenshot <- brick(screenshot[,,layers], xmx=dimensions[2], ymx=dimensions[1]) * 255
  raster.clustering <- raster.screenshot
  raster.clustering[] <- 255

  contained.pixels <- sapply(clustering$clusters, function(cluster) {
      multipolygon <- cluster$multipolygon
      # flip y for plotting
      plot.height <- dim(screenshot)[1]
      for (p in 1:length(multipolygon)) { # polygons
        for (r in 1:length(multipolygon[[p]])) { # rings
          multipolygon[[p]][[r]][,2] <- plot.height - multipolygon[[p]][[r]][,2]
        }
      }

      col.pixel <- 1
      contained.pixels.list <- extract(raster.screenshot, as(multipolygon, "Spatial"), cellnumbers=TRUE)
      contained.pixels <- sapply(contained.pixels.list, function(contained.pixels) {return(contained.pixels[,col.pixel])})
      return(contained.pixels)
    })
  if (is.list(contained.pixels)) {
    for (pixels in contained.pixels) {
      raster.clustering[pixels] <- raster.screenshot[pixels]
    }
  } else {
    raster.clustering[contained.pixels] <- raster.screenshot[contained.pixels]
  }

  return(raster.clustering)
}

subset.clustering <- function(clustering, indices, ...) {
  clustering$clusters <- clustering$clusters[indices]
  clustering$membership.subsets <- clustering$membership.subsets[indices]
  return(clustering)
}

PixelBasedClusterings <- function(segmentations, segment.size.functions = list(AreaSegmentSizeFunction()), precision = precision.default) {
  num.segments <- sum(GetLengths(segmentations))

  segmentation.geometries <- st_simplify(as.sfc.segmentations(segmentations))
  segmentation.geometries <- st_snap(segmentation.geometries, segmentation.geometries, 1)
  segmentation.geometries <- segmentation.geometries[st_area(st_set_precision(segmentation.geometries, precision)) >= 1]
  segmentation.geometries <- segmentation.geometries[st_is_valid(st_set_precision(segmentation.geometries, precision))]
  cluster.geometries <- st_intersection(segmentation.geometries)
  cluster.geometries.nonempty <- st_area(cluster.geometries) >= 1
  cluster.multipolygons <- lapply(cluster.geometries[cluster.geometries.nonempty], as.MULTIPOLYGON)

  membership.matrix <- t(sapply(attr(cluster.geometries, "idx")[cluster.geometries.nonempty], function(membership.vector) {
      membership.matrix.row <- rep(FALSE, num.segments)
      membership.matrix.row[membership.vector] <- TRUE
      return(membership.matrix.row)
    }))

  membership.subsets <- GetSubsets(segmentations)

  # compose
  ComposeClustering <- function(segment.size.function) {
    sizes <- sapply(cluster.multipolygons, segment.size.function)
    clustering <- Clustering(cluster.multipolygons, membership.matrix = membership.matrix, membership.subsets = membership.subsets, sizes = sizes)
    class(clustering) <- c("pixelBasedClustering", class(clustering))
    return(clustering)
  }
  clusterings <- lapply(segment.size.functions, ComposeClustering)
  names(clusterings) <- sapply(segment.size.functions, attr, "name")
  return(clusterings)
}

NodeBasedClusterings <- function(segmentations, nodes, xpath.node.size.functions = list(IdentityXPathNodeSizeFunction()), ...) {
  clusterings <- lapply(xpath.node.size.functions, function(xpath.node.size.function) {return(NodeBasedClustering(segmentations, nodes, xpath.node.size.function = xpath.node.size.function, ...))})
  names(clusterings) <- sapply(xpath.node.size.functions, attr, "name")
  return(clusterings)
}

NodeBasedClustering <- function(segmentations, nodes, xpath.node.size.function = IdentityXPathNodeSizeFunction()) {
  num.segments <- sum(GetLengths(segmentations))
  segment.multipolygons <- as.sfc.segmentations(segmentations)

  # cluster.multipolygons and sizes
  num.nodes <- length(nodes)
  xpaths <- names(nodes)
  nodes.equality <- st_equals(nodes)
  GetSizeByIndex <- function(e) {
    node.equality <- nodes.equality[[e]]
    if (node.equality[1] == e) {
      return(sum(sapply(xpaths[node.equality], xpath.node.size.function)))
    } else {
      return(0) # same as another node with smaller index
    }
  }
  sizes <- sapply(1:num.nodes, GetSizeByIndex)
  nodes.non.duplicated <- sizes > 0
  sizes <- sizes[nodes.non.duplicated]
  num.multipolygons <- length(nodes.non.duplicated)
  cluster.multipolygons <- nodes[nodes.non.duplicated]

  # membership.matrix
  membership.matrix <- t(st_contains(segment.multipolygons, cluster.multipolygons, sparse=FALSE))

  # membership.subsets
  membership.subsets <- GetSubsets(segmentations)

  # compose
  clustering <- Clustering(cluster.multipolygons, membership.matrix, membership.subsets = membership.subsets, sizes = sizes)
  class(clustering) <- c("nodeBasedClustering", class(clustering))
  return(clustering)
}

###############################################################################
## ANNOTATORS COUNT
###############################################################################

GetAnnotatorsCount <- function(x, ...) {
  UseMethod("GetAnnotatorsCount")
}

GetAnnotatorsCount.clustering <- function(clustering) {
  annotators.count <- sapply(clustering$clusters, function(cluster) {
      return(GetAnnotatorsCount.cluster(cluster, clustering$membership.subsets))
    })
  return(annotators.count)
}

GetAnnotatorsCount.cluster <- function(cluster, membership.subsets) {
  ContainsClusterASegmentByAnnotator <- function(membership.subset) {
    return(any(cluster$membership[membership.subset]))
  }
  return(sum(sapply(membership.subsets, ContainsClusterASegmentByAnnotator)))
}

FilterClustersByAnnotatorsCount <- function(clustering, min.annotators = min.annotators.default) {
  annotators.count <- GetAnnotatorsCount(clustering)
  clustering$clusters <- clustering$clusters[annotators.count >= min.annotators]
  return(clustering)
}

###############################################################################
## CLUSTER SIZE FUNCTIONS
###############################################################################

SumClusterSizes <- function(clustering) {
  sizes <- sapply(clustering$clusters, function(cluster) {return(cluster$size)})
  if (is.list(sizes) && length(sizes) == 0) {
    return(0)
  } else {
    return(sum(sizes))
  }
}


AreaSegmentSizeFunction <- function() {
  size.function <- st_area
  class(size.function) <- c("SegmentSizeFunction", "function")
  attr(size.function, "name") <- "pixel"
  return(size.function)
}

CannySegmentSizeFunction <- function(task, sigma = 1, upper.threshold = 2, name = NULL) {
  mask <- ReadCannyMask(task, sigma, upper.threshold, name)
  size.function <- ImageMaskSegmentSizeFunction(mask)
  class(size.function) <- c("SegmentSizeFunction", "function")
  if (is.null(mask)) {
    attr(size.function, "name") <- paste("canny-0x", sigma, "-1-", upper.threshold, sep="")
  } else {
    attr(size.function, "name") <- name
  }
  return(size.function)
}

ImageMaskSegmentSizeFunction <- function(x, ...) {
  UseMethod("ImageMaskSegmentSizeFunction")
}

ImageMaskSegmentSizeFunction.character <- function(file, ...) {
  return(ImageMaskSegmentSizeFunction(ReadMask(file, ...)))
}

ImageMaskSegmentSizeFunction.mask <- function(mask, buffer = image.mask.buffer.default) {
  dimensions <- dim(mask)
  class(mask) <- "matrix"
  mask <- apply(mask,2,rev) # revert y coordinates
  class(mask) <- "matrix"
  mask.raster <- raster(mask, xmx=dimensions[2], ymx=dimensions[1])
  size.function <- function(multipolygon) {
      multipolygon.enlarged <- as.MULTIPOLYGON(st_buffer(multipolygon, buffer))
      mask.values.in.multipolygon <- unlist(extract(mask.raster, as(multipolygon.enlarged, "Spatial")))
      return(sum(mask.values.in.multipolygon))
    }
  class(size.function) <- c("SegmentSizeFunction", "function")
  attr(size.function, "name") <- "mask"
  return(size.function)
}

IdentityXPathNodeSizeFunction <- function() {
  size.function <- function(xpath) { return(1) }
  class(size.function) <- c("XPathNodeSizeFunction", "function")
  attr(size.function, "name") <- "identity"
  return(size.function)
}

CharactersXPathNodeSizeFunction <- function(task) {
  node.character.counts <- ReadNodeCharacterCounts(task)
  size.function <- function(xpath) {
      size <- node.character.counts[[xpath]]
      if (is.null(size)) {
        return(0)
      } else {
        return(size)
      }
    }
  class(size.function) <- c("XPathNodeSizeFunction", "function")
  attr(size.function, "name") <- "ncharacters"
  return(size.function)
}

###############################################################################
## CLUSTER ALGORITHMS
###############################################################################

ClusterHClust <- function(x, ...) {
  UseMethod("ClusterHClust")
}

ClusterHClust.list <- function(clusterings, ...) {
  return(lapply(clusterings, function(clustering) {return(ClusterHClust(clustering, ...))}))
}

ClusterHClust.clustering <- function(clustering, ...) {
  disagreement.matrix <- ClusterAnnotatorDisagreementMatrix(clustering)
  ClusterHClust(disagreement.matrix, clustering = clustering, ...)
}

ClusterHClust.dist <- function(disagreement.matrix, clustering, disagreement.thresholds = hclust.disagreement.thresholds.default, method = hclust.method.default, ...) {
  if (length(clustering$clusters) <= 1) {
    clusterings.merged <- lapply(1:length(disagreement.thresholds), function(dt) {return(clustering)})
  } else {
    sizes <- sapply(clustering$clusters, function(cluster) {return(cluster$size)})
    tree <- hclust(disagreement.matrix, members=sizes, method=method, ...)
    tree$height <- round(tree$height, 6) # may otherwise not be sorted correctly due to rounding errors
    cuts <- matrix(cutree(tree, h=disagreement.thresholds), ncol=length(disagreement.thresholds))
    clusterings.merged <- lapply(1:length(disagreement.thresholds), function(dt) {return(MergeClusters(clustering, cuts[,dt]))})
  }
  segmentations <- lapply(clusterings.merged, as.segmentation)
  names(segmentations) <- paste("disagreement", disagreement.thresholds, sep="")
  return(segmentations)
}

MergeClusters <- function(clustering, cut) {
  clustering.merged <- clustering
  clustering.merged$clusters <- lapply(1:max(cut), function(cluster.index) {
      clusters <- clustering$clusters[cut == cluster.index]
      multipolygon <- clusters[[1]]$multipolygon
      multipolygon <- st_simplify(st_snap(multipolygon, multipolygon, 1))
      if (inherits(multipolygon, "POLYGON")) {
        multipolygon <- st_multipolygon(list(multipolygon))
      }
      membership <- clusters[[1]]$membership
      size <- clusters[[1]]$size
      if (length(clusters) > 1) {
        for (c in 2:length(clusters)) {
          multipolygon.tomerge <- clusters[[c]]$multipolygon
          multipolygon.tomerge <- st_simplify(st_snap(multipolygon.tomerge, multipolygon.tomerge, 1))
          multipolygon <- st_union(multipolygon, multipolygon.tomerge)
          if (inherits(multipolygon, "POLYGON")) {
            multipolygon <- st_multipolygon(list(multipolygon))
          }
          multipolygon <- st_simplify(st_snap(multipolygon, multipolygon, 1))
          membership <- membership | clusters[[c]]$membership
          size <- size + clusters[[c]]$size
        }
      }

      if (!inherits(multipolygon, "MULTIPOLYGON")) {
       multipolygon <- as.MULTIPOLYGON(multipolygon)
      }
      return(Cluster(multipolygon, membership, size))
    })
  return(clustering.merged)
}

# Unlike AgreementMatrix, this ignore overlapping clusters by the same annotator: distance is based only on whether there is any segment by the anotator that contains both clusters
ClusterAnnotatorDisagreementMatrix <- function(clustering) {
  AgreementMatrix(clustering)
  if (!is.clustering(clustering)) { stop(paste("clustering must be a clustering, but was", paste(class(clustering), collapse=","))) }

  clusters.count <- length(clustering$clusters)
  annotators.count <- length(clustering$membership.subsets)
  disagreement.matrix <- array(0, c(clusters.count, clusters.count))
  if (clusters.count <= 1) {
    return(disagreement.matrix)
  }

  for (c1 in 1:(clusters.count - 1)) {
    for (c2 in (c1+1):clusters.count) {
      annotators.that.group.together.count <- 0
      for (a in 1:annotators.count) {
        annotator.label.indices <- clustering$membership.subsets[[a]]
        annotator.c1 <- clustering$clusters[[c1]]$membership[annotator.label.indices]
        annotator.c2 <- clustering$clusters[[c2]]$membership[annotator.label.indices]
        if (sum(annotator.c2[annotator.c1 == 1]) >= 1) { # at least one common "1" in l1 and l2
          annotators.that.group.together.count <- annotators.that.group.together.count + 1
        }
      }
      disagreement.matrix[c1,c2] <- (annotators.count - annotators.that.group.together.count) / annotators.count
      disagreement.matrix[c2,c1] <- disagreement.matrix[c1,c2]
    }
  }

  return(as.dist(disagreement.matrix))
}

