library("jsonlite")

#' Creates a new segmentation task object without segmentations.
#'
#' \code{Task(id, width, height)}             # A task without a directory to read task data from
#' \code{Task(id, width, height, directory)}  # A task with a directory to read task data from
#' \code{Task(screenshotFile)}                # A task with ID, width, height, and directory determined through the screenshot file
#' \code{Task(id, screenshotFile)}            # A task with width, height, and directory determined through the screenshot file
#'
#' @param id The ID of the task
#' @param width The width of the task's page/screenshot in pixels; only used if no screenshot file is given
#' @param height The height of the task's page/screenshot in pixels; only used if no screenshot file is given
#' @param directory The directory to read task data (screenshots, nodes.csv) from; only used if no screenshot file is given
#' @param screenshotFile The file of the screenshot to use to determine the ID (optional, as the directory name), width, height, and the task directory
Task <- function(id = NULL, width = 0, height = 0, directory = NULL, screenshotFile = NULL) {
  task <- list()
  task$segmentations <- list()
  if (is.null(screenshotFile)) {
    task$id <- id
    task$height <- height
    task$width <- width
    if (is.null(directory)) {
      task$directory <- ""
    } else {
      task$directory <- directory
    }
  } else {
    screenshot <- readPNG(screenshotFile)
    if (is.null(id)) {
      task$id <- basename(dirname(screenshotFile))
    } else {
      task$id <- id
    }
    task$height <- dim(screenshot)[1]
    task$width <- dim(screenshot)[2]
    task$directory <- dirname(screenshotFile)
  }
  return(as.task(task))
}

as.task <- function(x, ...) {
  UseMethod("as.task")
}

as.task.default <- function(task) {
  if (is.task(task)) { return(task) }

  if (!is.list(task)) { stop(paste("task must be a list, but was", class(task))) }

  if (is.null(task$id)) { stop("task misses required attribute 'id'") }
  if (!is.character(task$id)) { stop(paste("task id must be of type character, but was", class(task$id))) }

  if (is.null(task$directory)) { stop(paste("task", task$id, "misses required attribute 'directory'")) }
  if (!is.character(task$directory)) { stop(paste("task directory must be of type character, but was", class(task$id), "for task", task$id)) }

  if (is.null(task$width)) { stop(paste("task", task$id, "misses required attribute 'width'")) }
  if (!is.integer(task$width)) { stop(paste("task width must be of type integer, but was", class(task$width), "for task", task$id)) }
  if (task$width < 1) { stop(paste("task width must positive, but was", task$width, "for task", task$id)) }
  task$width <- unbox(task$width)

  if (is.null(task$height)) { stop(paste("task", task$id, "misses required attribute 'height'")) }
  if (!is.integer(task$height)) { stop(paste("task height must be of type integer, but was", class(task$height), "for task", task$id)) }
  if (task$height < 1) { stop(paste("task height must positive, but was", task$height, "for task", task$id)) }
  task$height <- unbox(task$height)

  if (is.null(task$segmentations)) { stop(paste("task", task$id, "misses required attribute 'segmentations'")) }
  task$segmentations <- as.segmentations(task$segmentations)

  class(task) <- c("task", "list")
  return(task)
}

is.task <- function(task) {
  return(inherits(task, "task"))
}

length.task <- function(task) {
  return(length(task$segmentations))
}

merge.task <- function(task1, task2) {
  if (task1$id != task2$id) { stop(paste("can merge tasks with same ids only, but got:", task1$id, "and", task2$id)) }
  if (task1$width != task2$width) { stop(paste("can merge tasks with same widths only, but got:", task1$width, "and", task2$width)) }
  if (task1$height != task2$height) { stop(paste("can merge tasks with same heights only, but got:", task1$height, "and", task2$height)) }

  # actual merge:
  task1$segmentations <- merge(task1$segmentations, task2$segmentations)

  return(task1)
}

ReadTask <- function(file) {
  json <- paste(readLines(file, warn=FALSE))
  task <- fromJSON(json, simplifyMatrix=FALSE)
  task$directory <- dirname(file)
  return(as.task(task))
}

WriteTask <- function(task, file) {
  task$id <- unbox(task$id)
  task$width <- unbox(task$width)
  task$height <- unbox(task$height)
  task$directory <- NULL
  task$segmentations <- asJSON.segmentations(task$segmentations)
  json <- toJSON(task)
  cat(json, file = file, sep = "\n")
}

subset.task <- function(task, ...) {
  task$segmentations <- subset(task$segmentations, ...)
  return(task)
}

Boundingbox.task <- function(task) {
  task$segmentations <- Boundingbox(task$segmentations)
  return(task)
}

plot.task <- function(task, screenshot = ReadScreenshot(task), file = NULL, ...) {
  plot(screenshot, file = file)
  plot(task$segmentations, ...)
  if (!is.null(file)) { dev.off() }
}

