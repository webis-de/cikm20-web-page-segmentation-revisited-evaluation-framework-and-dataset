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
    make_option("--input", type="character", default=NULL, help="JSON file of segmentations to plot"),
    make_option("--width", type="numeric", default=NULL, help="The width of the page's screenshot in pixels (default: use the width of the JSON file)"),
    make_option("--height", type="numeric", default=NULL, help="The height of the page's screenshot in pixels (default: use the height of the JSON file)"),
    make_option("--output", type="character", default=NULL, help="Output PNG file to plot to"),
    make_option("--segmentations", type="character", default=".*", help="Pattern that matches the names of the segmentations that should be plotted (default: .*)")
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
task <- subset(task, options$segmentations)

width <- task$width
if (!is.null(options$width)) {
  width <- options$width
}
height <- task$height
if (!is.null(options$height)) {
  height <- options$height
}

png(options$output, width=width, height=height)
par(oma=rep(0,4),mar=rep(0,4))
plot(NULL, xlim=c(0, width), ylim=c(0, height), type='n', xlab="", ylab="", xaxs="i", yaxs="i", axes=FALSE)
for (s in (1:length(task))) {
  segmentation <- task$segmentations[[s]]
  plot(segmentation, alpha=1, col="black", border=NA)
}
dev.off()

