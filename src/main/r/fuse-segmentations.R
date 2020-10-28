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
    make_option("--input", type="character", default=NULL, help="JSON file of segmentations to fit"),
    make_option("--output", type="character", default=NULL, help="JSON file to which segmentations should be written"),
    make_option("--segmentations", type="character", default=".*", help="Pattern that matches the names of the segmentations that should be merged to one (default: .*)"),
    make_option("--segments-min-annotators", type="integer", default=min.annotators.default, help=paste("All segments of the image that have an annotation by less than this number of annotators are discarded; default=", min.annotators.default, sep=""), dest="min.annotators"),
    make_option("--size-function", type="character", default=size.function.default, help=paste("Function used to determine the type and sizes of the clusters. One of 'pixels' (alias 'area'), 'edges-fine', 'edges-coarse', 'canny-0x<sigma>-1-<upper.threshold>' (with '<sigma>' and '<upper.threshold>' replaced accordingly), 'nodes' (alias 'identity'), or 'chars' (alias 'ncharacters'). All resources that are needed by the selected function have to reside in the directory of the ground-truth file; default=", size.function.default, sep=""), dest="size.function"),
    make_option("--method", type="character", default=hclust.method.default, help=paste("Method used by the hierarchical clustering algorithm for determining the new disagreements after merging clusters; default=", hclust.method.default, sep=""), dest="method"),
    make_option("--disagreement-threshold", type="character", default=paste(hclust.disagreement.thresholds.default, collapse=","), help=paste("Distance threshold for the hierarchical clustering algorithm; default=", paste(hclust.disagreement.thresholds.default, collapse=","), sep=""), dest="disagreement.threshold"),
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
  stop("Missing output file", call.=FALSE)
}

################################################################################
## EXECUTION
################################################################################

task <- subset(ReadTask(options$input), options$segmentations)
write(paste("Task", task$id), "")
start <- Sys.time();

write(paste("  Clustering for", options$size.function), file="")
clustering <- Clustering.task(task, size.function = options$size.function, precision = options$precision)
write(paste("  Filtering clusters with less than", options$min.annotators, "annotators"), file="")
clustering <- FilterClustersByAnnotatorsCount(clustering, options$min.annotators)
write("  Disagreement matrix", file="")
disagreement.matrix <- ClusterAnnotatorDisagreementMatrix(clustering)
write(paste("  Segmentation with", options$method, "at", paste(options$disagreement.threshold, collapse=",")), file="")
disagreement.thresholds <- as.numeric(unlist(strsplit(options$disagreement.threshold, ",")))
segmentations <- ClusterHClust.dist(disagreement.matrix, clustering, disagreement.thresholds = disagreement.thresholds, method=options$method)

task$segmentations <- subset.segmentations(segmentations, "^disagreement")
WriteTask(task, options$output)

