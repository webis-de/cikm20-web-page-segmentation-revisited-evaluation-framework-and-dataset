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
    make_option("--algorithm", type="character", default=NULL, help="JSON file of segmentations that contain the algorithm segmentation"),
    make_option("--algorithm-segmentation", type="character", default=".*", help="Pattern that matches the name of the segmentation from the algorithm segmentations file that should be evaluated (default: .*)", dest="algorithm.segmentation"),
    make_option("--ground-truth", type="character", default=NULL, help="JSON file of segmentations that contain the ground-truth segmentation", dest="ground.truth"),
    make_option("--ground-truth-segmentation", type="character", default=".*", help="Pattern that matches the name of the segmentations from the ground-truth segmentations file that the algorithm segmentation should be evaluated against (default: .*)", dest="ground.truth.segmentation"),
    make_option("--output", type="character", default=NULL, help="CSV file to which the evaluation should be written (default: do not write to a file)"),
    make_option("--size-function", type="character", default=size.function.default, help=paste("Function used to determine the type and sizes of the clusters. One of 'pixels' (alias 'area'), 'edges-fine', 'edges-coarse', 'canny-0x<sigma>-1-<upper.threshold>' (with '<sigma>' and '<upper.threshold>' replaced accordingly), 'nodes' (alias 'identity'), or 'chars' (alias 'ncharacters'). All resources that are needed by the selected function have to reside in the directory of the ground-truth file; default=", size.function.default, sep=""), dest="size.function"),
    make_option("--precision", type="double", default=precision.default, help=paste("Precision in pixels for intersections: decrease to 0.1 if you get non-noded intersections; default=", precision.default, sep=""))
  )

options.parser <- OptionParser(option_list=option_list)
options <- parse_args(options.parser)
if (is.null(options$algorithm)) {
  print_help(options.parser)
  stop("Missing algorithm file", call.=FALSE)
}
if (is.null(options$ground.truth)) {
  print_help(options.parser)
  stop("Missing ground truth file", call.=FALSE)
}

################################################################################
## EXECUTION
################################################################################

task.algorithm <- subset(ReadTask(options$algorithm), options$algorithm.segmentation)
if (length(task.algorithm) != 1) {
  stop(paste("--algorithm and --algorithm-segmentation have to specify a single segmentation, but specified", length(task.algorithm)), call.=FALSE)
}
task.ground.truth <- subset(ReadTask(options$ground.truth), options$ground.truth.segmentation)
if (length(task.ground.truth) == 0) {
  stop(paste("--ground-truth and --ground-truth-segmentation have to specify at least one segmentation, but specified", length(task.ground.truth)), call.=FALSE)
}

write(paste("Task", task.algorithm$id), file="")

bcubed <- matrix(0, nrow=length(task.ground.truth), ncol=3)
colnames(bcubed) <- c("bcubed.precision", "bcubed.recall", "bcubed.f1")
row.names <- rep("NAME", length(task.ground.truth))

write(paste("Algorithm:", names(task.algorithm$segmentations), "with", length(task.algorithm$segmentations[[1]]), "segments"), file="")
for (gt in 1:length(task.ground.truth)) {
  write(paste("Ground-truth:", names(task.ground.truth$segmentations)[gt], "with", length(task.ground.truth$segmentations[[gt]]), "segments"), file="")

  task <- merge(subset(task.ground.truth, gt), task.algorithm)
  clustering <- Clustering.task(task, size.function = options$size.function, precision = options$precision)
  bcubed.matrix <- BCubedPrecisionMatrix(clustering)

  bcubed.precision <- bcubed.matrix[2,1]
  bcubed.recall <- bcubed.matrix[1,2]
  bcubed.f1 <- F1(bcubed.precision, bcubed.recall)

  bcubed[gt,1] <- bcubed.precision
  bcubed[gt,2] <- bcubed.recall
  bcubed[gt,3] <- bcubed.f1

  row.names[gt] <- names(task.ground.truth$segmentations)[gt]
}
rownames(bcubed) <- row.names

write.csv(bcubed, file="")
if (!is.null(options$output)) {
  write.csv(bcubed, file=options$output)
}

