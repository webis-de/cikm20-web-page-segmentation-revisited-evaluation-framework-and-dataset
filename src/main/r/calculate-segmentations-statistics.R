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
    make_option("--input", type="character", default=NULL, help="JSON file of segmentations for which to calculate statistics"),
    make_option("--segmentations", type="character", default=".*", help="Pattern that matches the names of the segmentations for which statistics should be calculated (default: .*)")
  )

options.parser <- OptionParser(option_list=option_list)
options <- parse_args(options.parser)
if (is.null(options$input)) {
  print_help(options.parser)
  stop("Missing input file", call.=FALSE)
}

################################################################################
## EXECUTION
################################################################################

task <- subset(ReadTask(options$input), options$segmentations)
write("#segmentation number-of-segments covered-area-ratio", "")

task.area <- task$width * task$height
for (name in names(task$segmentations)) {
  segmentation <- lapply(task$segmentations[[name]], st_multipolygon)
  covered.area.ratio <- 0
  if (length(segmentation) > 0) {
    segmentation.area <- st_area(st_union(st_sfc(segmentation)))
    covered.area.ratio <- segmentation.area / task.area
  }
  write(paste(name, length(segmentation), covered.area.ratio), "")
}

