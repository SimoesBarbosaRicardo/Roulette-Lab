#' Run the shiny application inside a web browser window
#' @export
launch <- function() {
  shiny::runApp(system.file("shiny", package = "RouletteLab"),
                display.mode = "normal",
                launch.browser = TRUE)
}


