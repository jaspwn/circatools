#' @export
#'
runpeakPlottR <- function() {
  appDir <- system.file("shinypeakPlottR", package = "circatools")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `circatools`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
