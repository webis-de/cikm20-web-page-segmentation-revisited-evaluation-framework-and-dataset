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
    make_option("--input", type="character", default=NULL, help="JSON file of segmentations for which to calculate agreement"),
    make_option("--output", type="character", default=NULL, help="Directory to which agreement files should be written"),
    make_option("--segmentations", type="character", default=".*", help="Pattern that matches the names of the segmentations that should be merged to one (default: .*)"),
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
  stop("Missing output directory", call.=FALSE)
}



################################################################################
## EXECUTION
################################################################################

task <- subset(subset(ReadTask(options$input), options$segmentations), 1:max.segmentations)
write(paste("Task", task$id), "")
start <- Sys.time();

write("  Clusterings", file="")
clusterings <- Clusterings(task, precision = options$precision)
agreement.matrices <- list()
write("  Agreement: pixel (including edges, they are just weighted differently)", file="")
agreement.matrices[["pixel"]] <- AgreementMatrices(clusterings$pixel)
agreement.matrices[["edges-coarse"]] <- agreement.matrices$pixel # same as for pixel
agreement.matrices[["edges-fine"]] <- agreement.matrices$pixel
write("  Agreement: identity", file="")
agreement.matrices[["identity"]] <- AgreementMatrices(clusterings$identity)
if (length(clusterings$ncharacters$clusters) > 0) {
  write("  Agreement: ncharacters", file="")
  agreement.matrices[["ncharacters"]] <- AgreementMatrices(clusterings$ncharacters)
}

if (!dir.exists(options$output)) { dir.create(options$output) }
for (name in names(agreement.matrices)) {
  write(paste("  BCubed:", name), file="")
  bcubed.precision.matrix <- BCubedPrecisionMatrix(clusterings[[name]], agreement.matrices = agreement.matrices[[name]])
  WriteBCubedPrecisionMatrix(bcubed.precision.matrix, file = paste(options$output, "/", name, ".txt", sep=""))
}

print(Sys.time() - start)

