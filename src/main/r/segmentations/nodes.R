ReadNodes <- function(x, ...) {
  UseMethod("ReadNodes")
}

ReadNodes.matrix <- function(nodes.matrix) {
  return(st_as_sfc(apply(nodes.matrix, 1, as.MULTIPOLYGON.rectangle)))
}

ReadNodes.character <- function(nodes.file) {
  nodes.data <- read.csv(nodes.file)
  nodes.matrix <- as.matrix(nodes.data[,1:4])
  rownames(nodes.matrix) <- nodes.data[,5]
  return(ReadNodes(nodes.matrix))
}

ReadNodes.task <- function(task) {
  nodes.file <- paste(task$directory, "nodes.csv", sep="/")
  return(ReadNodes(nodes.file))
}

ReadNodeCharacterCounts <- function(x, ...) {
  UseMethod("ReadNodeCharacterCounts")
}

ReadNodeCharacterCounts.character <- function(node.texts.file) {
  return(tryCatch({
      node.texts <- read.csv(node.texts.file, row.names=1)
      node.character.counts <- as.list(node.texts$ncharacter)
      names(node.character.counts) <- rownames(node.texts)
      node.character.counts
    }, error = function(cond) {
      message("No text found on the page")
      return(list())
    }))
}

ReadNodeCharacterCounts.task <- function(task) {
  nodes.texts.file <- paste(task$directory, "nodes-texts.csv", sep="/")
  return(ReadNodeCharacterCounts(nodes.texts.file))
}

as.MULTIPOLYGON.rectangle <- function(rectangle) {
  # polygon
  #
  #  4 <- 3
  #  |    ^
  #  v    |
  # 1=5-> 2
  #
  left <- rectangle[colLeft]
  right <- rectangle[colRight]
  xs <- c(left, right, right, left, left)
  bottom <- rectangle[colBottom]
  top <- rectangle[colTop]
  ys <- c(bottom, bottom, top, top, bottom)
  return(st_multipolygon(list(list(cbind(xs, ys)))))
}

subset.sfc <- function(nodes, indicesOrXpaths, ...) {
  if (inherits(indicesOrXpaths, "integer") | inherits(indicesOrXpaths, "numeric")) {
    nodes <- nodes[indicesOrXpaths]
  } else if (inherits(indicesOrXpaths, "character")) {
    nodes <- nodes[grep(indicesOrXpaths, names(nodes))]
  } else {
    stop(paste("Invalid indicesOrXpaths (could be integer or character (array)) of type", class(indicesOrXpaths)))
  }
  return(nodes)
}

PlotNodes <- function(nodes, border = "black", alpha = 0.0, ...) {
  for (node in nodes) {
    plot(as.segment(node), border = border, alpha = alpha, ...)
  }
}

GetTextNodes <- function(nodes) {
  if (!inherits(nodes, "sfc")) { stop(paste("can only get text nodes from object of type 'sfc', but was", class(nodes))) }
  return(subset(nodes, "text[^/]*$"))
}

