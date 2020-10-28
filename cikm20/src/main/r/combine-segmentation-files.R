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
  )

options.parser <- OptionParser(option_list=option_list, usage = "Usage: %prog input-segmentations1.json [...] output-segmentations.json")
options <- parse_args(options.parser, positional_arguments = TRUE)


################################################################################
## EXECUTION
################################################################################

if (length(options$args) < 3) {
  stop("Need at least two input files and one output file", call.=FALSE)
} else {
  task <- ReadTask(options$args[[1]])

  for (i in 2:(length(options$args) - 1)) {
    nextTask <- ReadTask(options$args[[i]])
    if (nextTask$id != task$id) {
      stop(paste("Task ID of", options$args[[1]], "is", task$id, "but of", options$args[[i]], "is", nextTask$id), call.=FALSE)
    }
    for (name in names(nextTask$segmentations)) {
      task$segmentations[[name]] <- nextTask$segmentations[[name]]
    }
  }

  WriteTask(task, options$args[[length(options$args)]])
}

