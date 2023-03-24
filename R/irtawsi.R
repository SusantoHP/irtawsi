#'
#' @title Items Response Theory Analysis with Steps and Interpretation
#' @return Nothing
#' @description Analysis of Dichotomous and polytomous data using unidimensional Item Response Theory model (Chalmers (2012) <doi:10.18637/jss.v048.i06>) with user friendly Graphical User Interface. Suitable for  beginners who are learning Item Response Theory.
#' @details launches 'shiny' interface
#' @keywords IRT
#'
#' @export

irtawsi <- function() {
  shiny::runApp(appDir = system.file("irtawsi", package="irtawsi"))
}
