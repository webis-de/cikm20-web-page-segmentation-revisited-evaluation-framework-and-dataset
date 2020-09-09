as.segmentations <- function(x, ...) {
  UseMethod("as.segmentations")
}

as.segmentations.segmentations <- function(segmentations) {
  return(segmentations)
}
as.segmentations.list <- function(listOfSegmentations) {
  segmentations <- lapply(listOfSegmentations, as.segmentation)
  class(segmentations) <- c("segmentations", "list")
  return(segmentations)
}

asJSON.segmentations <- function(segmentations) {
  return(lapply(segmentations, asJSON.segmentation))
}

as.MULTIPOLYGON.list.segmentations <- function(segmentations) {
  multipolygons <- unlist(lapply(segmentations, as.MULTIPOLYGON.list.segmentation), recursive=FALSE)
  return(multipolygons)
}

as.sfc.segmentations <- function(segmentations) {
  return(st_as_sfc(as.MULTIPOLYGON.list.segmentations(segmentations)))
}

is.segmentations <- function(segmentations) {
  return(inherits(segmentations, "segmentations"))
}

Boundingbox.segmentations <- function(segmentations) {
  return(as.segmentations(lapply(segmentations, Boundingbox)))
}

plot.segmentations <- function(segmentations, jitter = NA, col = c("cyan", "green", "purple", "orange", "red", "blue", "salmon"), border = col, ...) {
  if (is.na(jitter)) {
    jitter <- length(segmentations) > 1
  }

  for (s in 1:length(segmentations)) {
    plot(segmentations[[s]], col = col[s], border = border[s], jitter = jitter, ...)
  }
}

subset.segmentations <- function(segmentations, indicesOrNames, ...) {
  if (inherits(indicesOrNames, "integer") | inherits(indicesOrNames, "numeric")) {
    segmentations <- segmentations[indicesOrNames]
  } else if (inherits(indicesOrNames, "character")) {
    segmentations <- segmentations[grep(indicesOrNames, names(segmentations))]
  } else {
    stop(paste("Invalid indicesOrNames (could be integer or character (array)) of type", class(indicesOrNames)))
  }
  class(segmentations) <- c("segmentations", "list")
  return(segmentations)
}

merge.segmentations <- function(segmentations1, segmentations2) {
  segmentations1 <- c(segmentations1, segmentations2)
  class(segmentations1) <- c("segmentations", "list")
  return(segmentations1)
}

GetLengths <- function(segmentations) {
  if (!is.segmentations(segmentations)) { stop(paste("can only get lengths of segmentations, but was of type", paste(class(segmentations), collapse=","))) }

  if (length(segmentations) == 0) {
    return(NULL)
  } else {
    return(sapply(segmentations, length))
  }
}

GetSubsets <- function(segmentations) {
  borders <- c(0, cumsum(GetLengths(segmentations)))
  num.segmentations <- length(borders) - 1
  subsets <- list()
  for (i in 1:num.segmentations) {
    if (borders[i] < borders[i+1]) {
      subsets[[length(subsets) + 1]] <- (borders[i]+1):(borders[i+1])
    } else {
      subsets[[length(subsets) + 1]] <- c()
    }
  }
  return(subsets)
}

