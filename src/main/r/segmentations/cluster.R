library(sf)

Cluster <- function(multipolygon, membership, size = 1) {
  cluster <- list()

  # multipolygon
  if (!inherits(multipolygon, "MULTIPOLYGON")) {
    stop(paste("Given geometry was not a MULTIPOLYGON, but", paste(class(multipolygon), collapse=",")))
  }
  if (!inherits(multipolygon, "XY")) {
    stop(paste("Given geometry was not a XY, but", paste(class(multipolygon), collapse=",")))
  }
  if (is.segment(multipolygon)) {
    cluster$multipolygon <- as.MULTIPOLYGON.segment(multipolygon)
  } else {
    cluster$multipolygon <- multipolygon
  }

  # segment membership
  if (!is.vector(membership)) {
    stop(paste("Given membership was not a vector (of logicals), but", paste(class(membership), collapse=",")))
  }
  if (!is.logical(membership)) {
    stop(paste("Given membership was not a logical (vector), but", paste(class(membership), collapse=",")))
  }
  cluster$membership <- membership

  # size
  if (!is.numeric(size)) {
    stop(paste("Given size was not numeric, but", paste(class(size), collapse=",")))
  }
  cluster$size <- size

  class(cluster) <- c("cluster", "list")
  return(cluster)
}

is.cluster <- function(cluster) {
  return(inherits(cluster, "cluster"))
}

plot.cluster <- function(cluster, ...) {
  plot(as.segment(cluster), ...)
}

