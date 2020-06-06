AgreementMatrices <- function(clustering) {
  agreement.matrices <- lapply(clustering$membership.subsets, function(membership.subset) {return(AgreementMatrix(clustering, membership.subset))})
  class(agreement.matrices) <- c("AgreementMatrices", "list")
  return(agreement.matrices)
}

AgreementMatrix <- function(clustering, membership.subset) {
  GetAgreement <- function(cluster.indices) {
    cluster1.membership <- clustering$clusters[[cluster.indices[1]]]$membership[membership.subset]
    cluster2.membership <- clustering$clusters[[cluster.indices[2]]]$membership[membership.subset]
    return(sum(cluster1.membership & cluster2.membership))
  }

  num.clusters <- length(clustering)
  common.segment.counts <- matrix(NA, num.clusters, num.clusters)
  # fill lower triangle
  triangle <- lower.tri(common.segment.counts, diag=TRUE)
  triangle.indices <- which(triangle, arr.ind=TRUE)
  common.segment.counts[triangle] <- apply(triangle.indices, 1, GetAgreement)
  # make symmetric
  upper.triangle.indices <- upper.tri(common.segment.counts)
  common.segment.counts[upper.triangle.indices] <- t(common.segment.counts)[upper.triangle.indices]

  class(common.segment.counts) <- c("AgreementMatrix", "matrix")
  return(common.segment.counts)
}

BCubedPrecisionMatrices <- function(clusterings) {
  matrices <- lapply(clusterings, BCubedPrecisionMatrix)
  names(matrices) <- names(clusterings)
  return(matrices)
}

# Based on "A comparison of Extrinsic Clustering EvaluationMetrics based on Formal Constraints" by Amigo et al, 2009
# http://nlp.uned.es/docs/amigo2007a.pdf, 6.2, page 20 onwards
#
# Result:
# bcubed.precision.matrix[row,column]
#   -> algorithm segments are those of clustering$membership.subsets[[row]]
#   -> truth     segments are those of clustering$membership.subsets[[column]]
BCubedPrecisionMatrix <- function(clustering, agreement.matrices = AgreementMatrices(clustering), cluster.sizes = sapply(clustering$clusters, function(cluster) {return(cluster$size)})) {
  num.clusters <- length(clustering)

  MultiplicityPrecision <- function(l1, l2, agreement.matrix.algorithm, agreement.matrix.truth) {
    num.segments.algorithm <- agreement.matrix.algorithm[l1,l2]
    num.segments.truth <- agreement.matrix.truth[l1,l2]
    if (num.clusters == 0) {
      return(NaN)
    } else {
      return(min(num.segments.algorithm, num.segments.truth) / num.segments.algorithm)
    }
  }

  AverageMultiplicityPrecision <- function(l1, ...) {
    multiplicity.precision <- sapply(1:num.clusters, function(l2, ...) { return(MultiplicityPrecision(l1, l2, ...)) }, ...)
    common <- !is.nan(multiplicity.precision)
    total <- sum(cluster.sizes[common])
    if (total == 0) {
      return(NaN)
    } else {
      average.multiplicity.precision <- sum(multiplicity.precision[common] * cluster.sizes[common]) / total
      return(average.multiplicity.precision)
    }
  }

  BCubedPrecision <- function(agreement.matrix.algorithm.index, agreement.matrix.truth.index) {
    if (agreement.matrix.algorithm.index == agreement.matrix.truth.index) {
      return(1)
    }

    average.multiplicity.precision <- sapply(1:num.clusters, AverageMultiplicityPrecision, agreement.matrix.algorithm = agreement.matrices[[agreement.matrix.algorithm.index]], agreement.matrix.truth = agreement.matrices[[agreement.matrix.truth.index]])
    common <- !is.nan(average.multiplicity.precision)
    total <- sum(cluster.sizes[common])
    bcubed.precision <- sum(average.multiplicity.precision[common] * cluster.sizes[common]) / total
    return(bcubed.precision)
  }

  num.matrices <- length(agreement.matrices)
  bcubed.precision.matrix <- matrix(mapply(BCubedPrecision, rep(1:num.matrices, num.matrices), rep(1:num.matrices, each=num.matrices)), num.matrices, num.matrices)
  class(bcubed.precision.matrix) <- c("BCubedPrecisionMatrix", "EvaluationMatrix", "matrix")
  return(bcubed.precision.matrix)
}

CombineBCubedPrecisionAndRecall <- function(bcubed.precision.matrix, combination.function) {
  bcubed.combined.matrix <- bcubed.precision.matrix
  triangle <- lower.tri(bcubed.combined.matrix)
  triangle.indices <- which(triangle, arr.ind=TRUE)
  bcubed.combined.matrix[triangle] <- apply(triangle.indices, 1, function(indices) {
      recall <- bcubed.combined.matrix[indices[1],indices[2]]
      precision <- bcubed.combined.matrix[indices[2],indices[1]]
      return(combination.function(precision, recall))
    })
  # make symmetric
  bcubed.combined.matrix[upper.tri(bcubed.combined.matrix)] <- t(bcubed.combined.matrix)[upper.tri(bcubed.combined.matrix)]
  return(bcubed.combined.matrix)
}

BCubedMaxMatrix <- function(bcubed.precision.matrix) {
  if (!inherits(bcubed.precision.matrix, "BCubedPrecisionMatrix")) { stop(paste("Method requires a BCubedPrecisionMatrix, but got a", paste(class(bcubed.precision.matrix), collapse=","))) }
  bcubed.max.matrix <- CombineBCubedPrecisionAndRecall(bcubed.precision.matrix, max)
  class(bcubed.max.matrix) <- c("BCubedMaxMatrix", "EvaluationMatrix", "matrix")
  return(bcubed.max.matrix)
}

BCubedRecallMatrix <- function(bcubed.precision.matrix) {
  if (!inherits(bcubed.precision.matrix, "BCubedPrecisionMatrix")) { stop(paste("Method requires a BCubedPrecisionMatrix, but got a", paste(class(bcubed.precision.matrix), collapse=","))) }
  bcubed.recall.matrix <- t(bcubed.precision.matrix)
  class(bcubed.recall.matrix) <- c("BCubedRecallMatrix", "EvaluationMatrix", "matrix")
  return(bcubed.recall.matrix)
}

F1 <- function(precision, recall) {
  return(2 * precision * recall / (precision + recall))
}

BCubedF1Matrix <- function(bcubed.precision.matrix) {
  if (!inherits(bcubed.precision.matrix, "BCubedPrecisionMatrix")) { stop(paste("Method requires a BCubedPrecisionMatrix, but got a", paste(class(bcubed.precision.matrix), collapse=","))) }
  bcubed.f1.matrix <- CombineBCubedPrecisionAndRecall(bcubed.precision.matrix, F1)
  class(bcubed.f1.matrix) <- c("BCubedF1Matrix", "EvaluationMatrix", "matrix")
  return(bcubed.f1.matrix)
}

EvaluationMatrixMean <- function(evaluation.matrix) {
  if (!inherits(evaluation.matrix, "EvaluationMatrix")) { stop(paste("Method requires an EvaluationMatrix, but got a", paste(class(evaluation.matrix), collapse=","))) }
  non.self.evaluations <- diag(dim(evaluation.matrix)[1]) != 1
  return(mean(evaluation.matrix[non.self.evaluations]))
}

WriteBCubedPrecisionMatrix <- function(bcubed.precision.matrix, file="") {
  write.table(bcubed.precision.matrix, file=file, row.names=FALSE, col.names=FALSE, quote=FALSE)
}

ReadBCubedPrecisionMatrix <- function(file) {
  bcubed.precision.matrix <- unname(as.matrix(read.table(file)))
  class(bcubed.precision.matrix) <- c("BCubedPrecisionMatrix", "EvaluationMatrix", "matrix")
  return(bcubed.precision.matrix)
}
