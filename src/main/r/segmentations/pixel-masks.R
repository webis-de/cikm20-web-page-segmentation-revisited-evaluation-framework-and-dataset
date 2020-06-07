library("png")

ReadMask <- function(file) {
  mask <- readPNG(file)
  class(mask) <- c("mask", class(mask))
  return(mask)
}

ReadCannyMask <- function(x, ...) {
  UseMethod("ReadCannyMask")
}


ReadCannyMask.character <- function(file) {
  return(ReadMask(file))
}

ReadCannyMask.task <- function(task, sigma = 1, upper.threshold = 2, name = NULL) {
  file <- paste(task$directory, "/screenshot-canny-0x", sigma, "-1-", upper.threshold, ".png", sep="")
  if (!is.null(name)) {
    file <- paste(task$directory, "/screenshot-", name, ".png", sep="")
  }
  return(ReadCannyMask(file))
}

