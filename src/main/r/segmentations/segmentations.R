# Colors from the Integrative Genomics Viewer, https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html#igv
default.segmentations.colors <- c("#5050FFFF", "#CE3D32FF", "#749B58FF", "#F0E685FF", "#466983FF", "#BA6338FF", "#5DB1DDFF", "#802268FF", "#6BD76BFF", "#D595A7FF", "#924822FF", "#837B8DFF", "#C75127FF", "#D58F5CFF", "#7A65A5FF", "#E4AF69FF", "#3B1B53FF", "#CDDEB7FF", "#612A79FF", "#AE1F63FF", "#E7C76FFF", "#5A655EFF", "#CC9900FF", "#99CC00FF", "#A9A9A9FF", "#CC9900FF", "#99CC00FF", "#33CC00FF", "#00CC33FF", "#00CC99FF", "#0099CCFF", "#0A47FFFF", "#4775FFFF", "#FFC20AFF", "#FFD147FF", "#990033FF", "#991A00FF", "#996600FF", "#809900FF", "#339900FF", "#00991AFF", "#009966FF", "#008099FF", "#003399FF", "#1A0099FF", "#660099FF", "#990080FF", "#D60047FF", "#FF1463FF", "#00D68FFF", "#14FFB1FF")

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

plot.segmentations <- function(segmentations, jitter = NA, col = default.segmentations.colors, border = col, ...) {
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

