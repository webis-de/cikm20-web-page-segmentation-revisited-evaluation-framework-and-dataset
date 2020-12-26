#!/usr/bin/env Rscript

################################################################################
## LOADING SEGMENTATION LIBRARY
################################################################################

rscript.options <- commandArgs(trailingOnly = FALSE)
source.dir <- dirname(sub(".*=", "", rscript.options[grep("--file=", rscript.options)]))
source(paste(source.dir, "segmentations", "lib.R", sep="/"))



################################################################################
## OPTIONS
################################################################################

library("optparse")

option_list <- list(
    make_option("--input", type="character", default=NULL, help="JSON file of segmentations to flatten"),
    make_option("--output", type="character", default=NULL, help="JSON file to which the flattened segmentations should be written to"),
    make_option("--precision", type="double", default=precision.default, help=paste("Precision in pixels for intersections: decrease to 0.1 if you get non-noded intersections; default=", precision.default, sep=""))
  )

options.parser <- OptionParser(option_list=option_list)
options <- parse_args(options.parser)
if (is.null(options$input)) {
  print_help(options.parser)
  stop("Missing input file", call.=FALSE)
}
if (is.null(options$output)) {
  print_help(options.parser)
  stop("Missing output file", call.=FALSE)
}



################################################################################
## EXECUTION
################################################################################

task <- ReadTask(options$input)
for (name in names(task$segmentations)) {
  geometries <- st_simplify(as.sfc.segmentation(task$segmentations[[name]]))
  geometries <- st_snap(geometries, geometries, 0.9)
  geometries <- st_set_precision(geometries, options$precision)
  geometries <- geometries[st_area(geometries) >= 1]
  intersected.polygons <- st_simplify(st_intersection(geometries))
  intersected.polygons <- intersected.polygons[st_area(intersected.polygons) >= 1]
  segmentation <- as.segmentation(intersected.polygons)
  task$segmentations[[name]] <- as.segmentation(segmentation)
}

WriteTask(task, options$output)

