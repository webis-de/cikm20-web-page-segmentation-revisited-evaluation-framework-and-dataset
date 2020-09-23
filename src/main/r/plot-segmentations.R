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
    make_option("--screenshot", type="character", default=NULL, help="Screenshot to use as background image (default: use the screenshot determined by the --input file)"),
    make_option("--bounding-boxes", action="store_true", default=FALSE, help="Draw the bounding box rectangle instead of the multipolygon of a segment (default: draw multipolygons)", dest="bounding.boxes"),
    make_option("--frames", action="store_true", default=FALSE, help="Draw just the frames instead of filling the segments (default: draw filled)"),
    make_option("--colors", type="character", default=NULL, help="The colors to use as a comma-separated list, one color for each segmentations  (default: the 51 colors of the Integrative Genomics Viewer, as distributed in the GGCSI package)"),
    make_option("--line-width", type="numeric", default=0, help="The line width of the drawn borders (default: 1, or 5 with --frames)", dest="line.width"),
    make_option("--color-per-segment", action="store_true", default=FALSE, help="Color each segment differently instead of each segmentation: only allowed when plotting a single segmentation (default: one color per segmentation)", dest="color.per.segment"),
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
if (options$bounding.boxes) {
  task <- Boundingbox(task)
}

screenshot <- options$screenshot
if (is.null(screenshot)) {
  screenshot = ReadScreenshot(task)
} else {
  screenshot = ReadScreenshot(screenshot)
}

col <- default.segmentations.colors
if (!is.null(options$colors)) {
  col <- unlist(strsplit(options$colors, ","))
}
border <- col
if (options$frames) {
  col <- NULL
}

lwd <- options$line.width
if (lwd == 0) {
  if (options$frames) {
    lwd <- 5
  } else {
    lwd <- 1
  }
}

if (options$color.per.segment) {
  if (length(task) != 1) {
    stop(paste("--color-per-segment is allowed just for one segmentation, but got:", paste(names(task$segmentations), collapse=", ")), call.=FALSE)
  }
  segmentation <- task$segmentations[[1]]
  plot(screenshot, file=options$output)
  plot(segmentation, col=col, border=border, lwd=lwd)
  dev.off()
} else {
  plot(task, screenshot=screenshot, file=options$output, col=col, border=border, lwd = lwd)
}
warnings()

