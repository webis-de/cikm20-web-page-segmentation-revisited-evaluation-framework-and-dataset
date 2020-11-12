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
    make_option("--input", type="character", default=NULL, help="The nodes.csv to plot"),
    make_option("--width", type="numeric", default=1366, help="The width of the page's screenshot in pixels (default: 1366)"),
    make_option("--height", type="numeric", default=NULL, help="The height of the page's screenshot in pixels"),
    make_option("--output", type="character", default=NULL, help="Output PNG file to plot to")
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

nodes <- GetTextNodes(ReadNodes(options$input))

png(options$output, width=options$width, height=options$height)
par(oma=rep(0,4),mar=rep(0,4))
plot(NULL, xlim=c(0, options$width), ylim=c(0, options$height), type='n', xlab="", ylab="", xaxs="i", yaxs="i", axes=FALSE)
PlotNodes(nodes, alpha=1, col="black", border=NA)
dev.off()

