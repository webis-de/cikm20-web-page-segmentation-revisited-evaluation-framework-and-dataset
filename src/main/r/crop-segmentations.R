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
    make_option("--input", type="character", default=NULL, help="JSON file of the segmentations to crop"),
    make_option("--width", type="numeric", default=1366, help="The width to crop the segmentations to in pixels (default: 1366)"),
    make_option("--height", type="numeric", default=NULL, help="The height to crop the segmentations to in pixels"),
    make_option("--output", type="character", default=NULL, help="Output JSON file to write the cropped segmentations to")
  )

options.parser <- OptionParser(option_list=option_list)
options <- parse_args(options.parser)
if (is.null(options$input)) {
  print_help(options.parser)
  stop("Missing input file", call.=FALSE)
}
if (is.null(options$height)) {
  print_help(options.parser)
  stop("Missing height", call.=FALSE)
}
if (is.null(options$output)) {
  print_help(options.parser)
  stop("Missing output file", call.=FALSE)
}



################################################################################
## EXECUTION
################################################################################

task <- ReadTask(options$input)

width <- options$width
height <- options$height
canvas <- st_polygon(list(matrix(c(0, 0, 0, height, width, height, width, 0, 0, 0), ncol=2, byrow=TRUE)))

for (name in names(task$segmentations)) {
  cropped.polygons <- st_intersection(as.sfc.segmentation(task$segmentations[[name]]), canvas)
  task$segmentations[[name]] <- as.segmentation(cropped.polygons)
}

task$width <- min(task$width, width)
task$height <- min(task$height, height)

WriteTask(task, options$output)


