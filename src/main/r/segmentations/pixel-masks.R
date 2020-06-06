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

ReadCannyMask.task <- function(task, sigma, upperThreshold) {
  file <- paste(task$directory, "/screenshot-canny-0x", sigma, "-1-", upperThreshold, ".png", sep="")
  return(ReadCannyMask(file))
}

