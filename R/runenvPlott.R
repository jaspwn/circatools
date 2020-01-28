#' @export
#'
runenvPlottR <- function() {
  appDir <- system.file("shinyenvPlottR", package = "circatools")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `circatools`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
