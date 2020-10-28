library(png)

ReadScreenshot <- function(x, ...) {
  UseMethod("ReadScreenshot")
}

ReadScreenshot.character <- function(screenshot.file) {
  screenshot <- readPNG(screenshot.file)
  class(screenshot) <- c("screenshot", class(screenshot))
  return(screenshot)
}

ReadScreenshot.task <- function(task) {
  screenshot.file <- paste(task$directory, "screenshot.png", sep="/")
  return(ReadScreenshot(screenshot.file))
}

plot.screenshot <- function(screenshot, file = NULL, ...) {
  dimensions <- dim(screenshot)
  height <- dimensions[1]
  width <- dimensions[2]

  if (!is.null(file)) {
    png(file, width=width, height=height)
  }

  par(oma=rep(0,4),mar=rep(0,4))
  plot(NULL, xlim=c(0, width), ylim=c(0, height), type='n', xlab="", ylab="", xaxs="i", yaxs="i", axes=FALSE)
  rasterImage(screenshot, 0, 0, width, height)
}

