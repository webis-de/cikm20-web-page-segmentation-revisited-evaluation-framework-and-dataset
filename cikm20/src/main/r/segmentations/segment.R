library(sf)

Segment <- function(xs, ys) {
  mat <- matrix(c(c(xs, xs[1]), c(ys, ys[1])), ncol=2)
  return(as.segment(mat))
}

as.segment <- function(x, ...) {
  UseMethod("as.segment")
}

as.segment.matrix <- function(mat) {
  if (dim(mat)[2] != 2) { stop(paste("only matrices with two columns can be converted to segments, but this one had", dim(mat)[2])) }
  return(as.segment(list(list(c(t(mat))))))
}

as.segment.segment <- function(segment) {
  return(segment)
}

as.segment.sfc <- function(geometry) {
  segment <- st_multipolygon(geometry)
  class(segment) <- c("segment", class(segment))
  return(segment)
}

as.segment.POLYGON <- function(polygon) {
  segment <- st_multipolygon(list(polygon))
  class(segment) <- c("segment", class(segment))
  return(segment)
}

as.segment.MULTIPOLYGON <- function(multipolygon) {
  class(multipolygon) <- c("segment", class(multipolygon))
  return(multipolygon)
}

as.segment.sfc_MULTIPOLYGON <- function(sfc) {
  multipolygon <- st_multipolygon(unlist(sfc, recursive=FALSE))
  return(as.segment(multipolygon))
}

as.segment.list <- function(listOfListOfListOfCoordinates) {
  AsRing <- function(listOfCoordinates) {
    return(matrix(unlist(listOfCoordinates), ncol=2, byrow=TRUE))
  }
  AsPolygon <- function(listOfListOfCoordinates) {
    return(lapply(listOfListOfCoordinates, AsRing))
  }
  AsMultiPolygon <- function(listOfListOfListOfCoordinates) {
    return(st_multipolygon(lapply(listOfListOfListOfCoordinates, AsPolygon)))
  }

  return(as.segment(AsMultiPolygon(listOfListOfListOfCoordinates)))
}

as.segment.cluster <- function(cluster) {
  return(as.segment(cluster$multipolygon))
}

as.MULTIPOLYGON <- function(x, ...) {
  UseMethod("as.MULTIPOLYGON")
}

as.MULTIPOLYGON.GEOMETRYCOLLECTION <- function(geometrycollection) {
  nonempty <- sapply(geometrycollection, function(geometry) {return(st_area(geometry) > 0)})
  multipolygons <- lapply(geometrycollection[nonempty], as.MULTIPOLYGON)
  return(st_multipolygon(lapply(multipolygons, unlist, recursive=FALSE)))
}

as.MULTIPOLYGON.POLYGON <- function(polygon) {
  return(st_multipolygon(list(polygon)))
}

as.MULTIPOLYGON.MULTIPOLYGON <- function(multipolygon) {
  return(multipolygon)
}

as.MULTIPOLYGON.segment <- function(segment) {
  class(segment) <- c("XY", "MULTIPOLYGON", "sfg")
  return(segment)
}

asJSON.segment <- function(segment) {
  fromRing <- function(pointMatrix) {
    lists <- split(pointMatrix, 1:(dim(pointMatrix)[1]))
    return(unname(lists))
  }
  fromPolygon <- function(listOfRings) {
    return(lapply(listOfRings, fromRing))
  }
  return(lapply(segment, fromPolygon))
}

as.character.segment <- function(segment) {
  return(toJSON(segment, force = TRUE))
}

is.segment <- function(segment) {
  return(inherits(segment, "segment"))
}

print.segment <- function(segment) {
  class(segment) <- c("XY", "MULTIPOLYGON", "sfg")
  print(segment)
}

Boundingbox <- function(x, ...) {
  UseMethod("Boundingbox")
}

Boundingbox.segment <- function(segment) {
  bbox <- st_bbox(segment)
  polygon <- st_as_sfc(bbox)
  return(as.segment(polygon))
}

plot.segment <- function(segment, plot.height = par("usr")[4], jitter = FALSE, col = "blue", border = NA, alpha = 0.5, ...) {
  jitterOffsets <- c(0, 0)
  if (jitter) { jitterOffsets <- rnorm(2) }

  for (p in 1:length(segment)) {
    polygon <- st_polygon(segment[[p]])
    # apply jitter
    polygon <- polygon + jitter

    for (r in 1:length(polygon)) { # for each ring
      # flip y for plotting
      polygon[[r]][,2] <- plot.height - polygon[[r]][,2]
    }
    # add alpha to color
    if (!is.null(col)) {
      col <- col2rgb(col)
      col <- rgb(col[1] / 255, col[2] / 255, col[3] / 255, alpha = alpha)
      # plot
      plot(polygon, add = TRUE, col = col, border = border, ...)
    } else {
      plot(polygon, add = TRUE, border = border, ...)
    }
  }
}

