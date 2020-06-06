merge.scale.default = 0.2
merge.distance.threshold.default = 0.5
merge.method.default = "average"
merge.use.initial.cluster.sizes.default = TRUE
merge.result.segmentation.name.default = "merged"



MergeSegmentations <- function(segmentations, cluster.frequency.calculators, segmentation.name.pattern = "", merge.scale = merge.scale.default, ...) {
  segmentations.scaled <- list()
  segmentation.names <- names(segmentations)
  segmentation.names <- segmentation.names[grep(segmentation.name.pattern, segmentation.names)]
  for (s in 1:length(segmentation.names)) {
    segmentations.scaled[[s]] <- ScaleSegmentation(segmentations[[segmentation.names[s]]], scale = merge.scale)
  }

  clustering <- ConvertSegmentationsToClustering(segmentations.scaled, width = ceiling(task$width * merge.scale), height = ceiling(task$height * merge.scale))
  if (length(clustering$assignments) > 0) {
    distances.matrix <- CalculateDistancesMatrix(segmentations.scaled, clustering)
    for (cluster.frequency.calculator.name in names(cluster.frequency.calculators)) {
      clustering.merged <- ClusterSegmentations(distances.matrix, clustering, cluster.frequency.calculators[[cluster.frequency.calculator.name]], ...)
      segmentation <- ConvertMergedClusteringToSegmentation(clustering.merged)

      segmentations[[cluster.frequency.calculator.name]] <- ScaleSegmentation(segmentation, scale = 1/merge.scale)
    }
  }
  return(segmentations)
}

CalculateDistancesMatrix <- function(segmentations, clustering) {
  sizes <- GetNumSegments(segmentations)
  workers.count <- length(sizes)
  worker.offsets <- c(0, cumsum(sizes)) + 1

  segment.label.indices <- GetSegmentLabelIndices(clustering)
  num.segment.labels <- length(segment.label.indices)
  segment.labels <- clustering$labels[segment.label.indices,]

  distances.matrix <- array(0, c(num.segment.labels, num.segment.labels))

  if (num.segment.labels == 1) {
    return(distances.matrix)
  }

  for (l1 in 1:(num.segment.labels - 1)) {
    for (l2 in (l1+1):num.segment.labels) {
      workers.that.group.together.count <- 0
      for (w in 1:workers.count) {
        worker.label.indices <- worker.offsets[w]:(worker.offsets[w + 1] - 1)
        worker.l1 <- segment.labels[l1, worker.label.indices]
        worker.l2 <- segment.labels[l2, worker.label.indices]
        if (sum(worker.l2[worker.l1 == 1]) >= 1) { # at least one common "1" in l1 and l2
          workers.that.group.together.count <- workers.that.group.together.count + 1
        }
      }
      distances.matrix[l1,l2] <- (workers.count - workers.that.group.together.count) / workers.count
      distances.matrix[l2,l1] <- distances.matrix[l1,l2]
    }
  }

  return(distances.matrix)
}

ClusterSegmentations <- function(distances.matrix, clustering, cluster.frequency.calculator, merge.distance.threshold = merge.distance.threshold.default, merge.method = merge.method.default) {
  if (dim(distances.matrix)[1] == 1) {
    return(clustering)
  }

  segment.label.indices <- GetSegmentLabelIndices(clustering)
  initial.clusters.sizes <- cluster.frequency.calculator(clustering)[segment.label.indices]

  tree <- hclust(as.dist(distances.matrix), members = initial.clusters.sizes, method = merge.method)

  tree$height <- round(tree$height, 6) # may otherwise not be sorted correctly due to rounding errors
  cut <- cutree(tree, h=merge.distance.threshold)

  assignments <- array(0, dim = dim(clustering$assignments))
  for (i in 1:length(cut)) {
    assignments[clustering$assignments == segment.label.indices[i]] <- cut[i]
  }

  clustering <- list()
  clustering$assignments <- assignments
  clustering$labels <- 0:max(cut)
  clustering$frequencies <- table(clustering$assignments)
  return(clustering)
}

###############################################################################
## CONVERSION TOWARDS SEGMENTATION
###############################################################################

# Only works for clusterings without overlapping clusters
ConvertMergedClusteringToSegmentation <- function(clustering) {
  num.segments <- max(clustering$labels)
  segmentation <- matrix(0, nrow=num.segments, ncol=4)
  for (i in 1:num.segments) {
    xs <- which(apply(clustering$assignments == i, 2, max) == 1)
    ys <- which(apply(clustering$assignments == i, 1, max) == 1)
    segmentation[i,colLeft] <- min(xs)
    segmentation[i,colTop] <- min(ys)
    segmentation[i,colRight] <- max(xs)
    segmentation[i,colBottom] <- max(ys)
  }

  return(segmentation)
}

###############################################################################
## UTILITY
###############################################################################

GetSegmentLabelIndices <- function(clustering) {
  labels <- clustering$labels
  if (is.data.frame(labels)) {
    CheckIfLabelHasSegments <- function(x) { return(!all(x == 0)) }
    segment.label.indices <- which(apply(labels, 1, CheckIfLabelHasSegments))
    return(segment.label.indices)
  } else {
    segment.label.indices <- which(labels != 0)
    return(segment.label.indices)
  }
}

