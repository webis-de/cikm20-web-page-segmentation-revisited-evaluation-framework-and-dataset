as.segmentation <- function(x, ...) {
  UseMethod("as.segmentation")
}

as.segmentation.segmentation <- function(segmentation) {
  return(segmentation)
}

as.segmentation.clustering <- function(clustering, segments) {
  segments <- lapply(clustering$clusters, function(cluster) {return(cluster$multipolygon)})
  return(as.segmentation(segments))
}

as.segmentation.sfc <- function(segments) {
  return(as.segmentation.list(segments))
}

as.segmentation.list <- function(segments) {
  if (length(segments) == 0) {
    segmentation <- list()
    class(segmentation) <- c("segmentation", "list")
    return(segmentation)
  }

  segments <- segments[!(sapply(segments, inherits, "MULTILINESTRING") | sapply(segments, inherits, "LINESTRING") | sapply(segments, inherits, "POINT") | sapply(segments, inherits, "MULTIPOINT"))]
  collections <- sapply(segments, inherits, "GEOMETRYCOLLECTION")
  segmentation <- lapply(segments[!collections], as.segment)
  class(segmentation) <- c("segmentation", "list")

  for (index in which(collections)) {
    segmentation <- c(segmentation, as.segmentation.list(segments[[index]]))
  }

  return(segmentation)
}

asJSON.segmentation <- function(segmentation) {
  return(lapply(segmentation, asJSON.segment))
}

as.MULTIPOLYGON.list.segmentation <- function(segmentation) {
  multipolygons <- lapply(segmentation, as.MULTIPOLYGON.segment)
  return(multipolygons)
}

as.sfc.segmentation <- function(segmentation) {
  return(st_as_sfc(as.MULTIPOLYGON.list.segmentation(segmentation)))
}

is.segmentation <- function(segmentation) {
  return(inherits(segmentation, "segmentation"))
}

Boundingbox.segmentation <- function(segmentation) {
  return(as.segmentation(lapply(segmentation, Boundingbox)))
}

plot.segmentation <- function(segmentation, col = c("green", "blue", "purple", "orange", "red", "cyan", "salmon"), ...) {
  index <- 0
  for (segment in segmentation) {
    col.index <- ((index - 1) %% length(col)) + 1
    plot(segment, col = col[col.index], ...)
    index <- index + 1
  }
}

subset.segmentation <- function(segmentation, indices) {
  segmentation <- segmentation[indices]
  class(segmentation) <- c("segmentation", "list")
  return(segmentation)
}

unique.segmentation <- function(segmentation, write.info = FALSE) {
  sfc <- as.sfc.segmentation(segmentation)

  s <- 1
  while (s < length(sfc)) {
    duplicates <- which(st_equals(sfc[s], sfc[(s+1):length(sfc)], sparse=FALSE)) + s
    num.duplicates <- length(duplicates)
    if (num.duplicates > 0) {
      if (write.info) {
        write(paste("  remove-duplicates: ", num.duplicates, sep=""), file="")
      }
      sfc <- sfc[-duplicates]
      segmentation <- segmentation[-duplicates]
    }
    s <- s + 1
  }

  class(segmentation) <- c("segmentation", "list")
  return(segmentation)
}

GetEmptySegments <- function(segmentation) {
  sfc <- as.sfc.segmentation(segmentation)
  empties <- st_area(sfc) == 0
  return(empties)
}

RemoveEmptySegments <- function(segmentation, write.info = FALSE) {
  empties <- GetEmptySegments(segmentation)
  segmentation <- subset(segmentation, !empties)
  if (write.info) {
    write(paste("  remove-empty-segments: ", sum(empties), sep=""), file="")
  }
  return(segmentation)
}


