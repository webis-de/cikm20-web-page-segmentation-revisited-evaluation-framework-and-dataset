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
    make_option("--input", type="character", default=NULL, help="JSON file of segmentations to fit"),
    make_option("--output", type="character", default=NULL, help="JSON file to which segmentations should be written"),
    make_option("--nodes", type="character", default=NULL, help="The nodes.csv file that contains the nodes to fit to; default: take the nodes.csv next to the input file."),
    make_option("--fit-containment-threshold", type="double", default=fit.containment.threshold.default, help=paste("Fitted segments are the minimum axis-aligned rectangles that contain all elements that where contained to at least this percentage in the original rectangle; default=", fit.containment.threshold.default, sep=""), dest="fit.containment.threshold"),
    make_option("--segmentations", type="character", default=".*", help="Pattern that matches the names of the segmentations that should be fitted (default: .*)")
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
nodes <- NULL
if (is.null(options$nodes)) {
  nodes <- ReadNodes(task)
} else {
  nodes <- ReadNodes(options$nodes)
}
task <- FitToNodes(subset(task, options$segmentations), nodes = nodes, fit.containment.threshold = options$fit.containment.threshold, write.info = TRUE)
task <- subset(task, ".*fitted")
WriteTask(task, options$output)

