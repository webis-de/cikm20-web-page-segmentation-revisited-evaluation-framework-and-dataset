fit.containment.threshold.default <- 0.75

FitToNodes <- function(x, ...) {
  UseMethod("FitToNodes")
}

FitToNodes.task <- function(task, nodes = ReadNodes(task), node.areas = st_area(nodes), ...) {
  task$segmentations <- FitToNodes(task$segmentations, nodes, node.areas = node.areas, ...)
  return(task)
}

FitToNodes.segmentations <- function(segmentations, nodes, node.areas = st_area(nodes), write.info = FALSE, ...) {
  segmentation.names <- names(segmentations)
  for (name in segmentation.names) {
    name.new <- paste(name, "fitted", sep=".")
    if (write.info) {
      write(paste("Fitting", name, "to", name.new), file="")
    }
    segmentations[[name.new]] <- FitToNodes(segmentations[[name]], nodes, node.areas = node.areas, write.info = write.info, ...)
  }
  return(segmentations)
}

FitToNodes.segmentation <- function(segmentation, nodes, node.areas = st_area(nodes), write.info = FALSE, ...) {
  if (length(segmentation) == 0) {
    return(segmentation)
  } else {
    segmentation.old <- segmentation
    for (s in 1:length(segmentation)) {
      segmentation[[s]] <- FitToNodes(segmentation[[s]], nodes, node.areas = node.areas, write.info = write.info, ...)
    }
    empties <- GetEmptySegments(segmentation)
    segmentation <- subset(segmentation, !empties)
    if (write.info) {
      write(paste("  removed-empty-segments: ", sum(empties), sep=""), file="")
      write(paste("  kept-non-empty-segments: ", sum(!empties), sep=""), file="")
      for (empty in which(empties)) {
        write(paste("  removed-empty-segment: ", segmentation.old[[empty]], sep=""), file="")
      }
    }
  }

  return(unique(segmentation, write.info = write.info))
}

FitToNodes.segment <- function(segment, nodes, node.areas = st_area(nodes), fit.containment.threshold = fit.containment.threshold.default, tolerance = 0, write.info = FALSE) {

  segment <- as.MULTIPOLYGON.segment(segment)
  segment <- as.MULTIPOLYGON.segment(segment[st_is_valid(segment)])
  overlapping <- st_intersects(nodes, segment, sparse=FALSE)
  nodes.overlapping <- nodes[overlapping]
  node.overlapping.areas <- node.areas[overlapping]

  overlap.areas <- st_area(st_intersection(nodes.overlapping, segment, tolerance = tolerance))
  nodes.contained <- (overlap.areas / node.overlapping.areas) >= fit.containment.threshold
  segment.fitted <- as.segment(st_simplify(st_union(nodes.overlapping[nodes.contained]), dTolerance = tolerance))

  if (write.info) {
    polygon.fitted <- as.MULTIPOLYGON.segment(segment.fitted)
    overlap.area <- st_area(st_intersection(segment, polygon.fitted, tolerance = tolerance))
    write(paste("  original-area: ", st_area(segment), " contained-nodes: " , sum(nodes.contained), " fitted-area: ", st_area(polygon.fitted), " overlap-area: ", overlap.area, sep=""), file="")
  }
  return(segment.fitted)
}

