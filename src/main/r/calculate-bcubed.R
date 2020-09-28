#!/usr/bin/env Rscript

################################################################################
## LOADING SEGMENTATION LIBRARY
################################################################################

rscript.options <- commandArgs(trailingOnly = FALSE)
source.dir <- dirname(sub(".*=", "", rscript.options[grep("--file=", rscript.options)]))
source(paste(source.dir, "segmentations", "lib.R", sep="/"))

max.segmentations <- 5

################################################################################
## OPTIONS
################################################################################

library("optparse")

option_list <- list(
  )

options.parser <- OptionParser(option_list=option_list, usage = "Usage: %prog agreement-matrix-file1 [...]")
options <- parse_args(options.parser, positional_arguments = TRUE)


################################################################################
## EXECUTION
################################################################################

output <- options$output
write("#file precision recall f1 max", file="")
for (input in options$args) {
  bcubed.precision.matrix <- ReadBCubedPrecisionMatrix(input)
  bcubed.precision <- EvaluationMatrixMean(bcubed.precision.matrix)
  bcubed.recall <- EvaluationMatrixMean(BCubedRecallMatrix(bcubed.precision.matrix))
  bcubed.f1 <- EvaluationMatrixMean(BCubedF1Matrix(bcubed.precision.matrix))
  bcubed.max <- EvaluationMatrixMean(BCubedMaxMatrix(bcubed.precision.matrix))
  write(paste(input, bcubed.precision, bcubed.recall, bcubed.f1, bcubed.max), file="")
}

