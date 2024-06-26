#' @title Items Response Theory Analysis with Steps and Interpretation
#' @return Nothing
#' @description Dichotomous and polytomous data analysis and their scoring using the unidimensional Item Response Theory model (Chalmers (2012) <doi:10.18637/jss.v048.i06>) with user-friendly graphic User Interface. Suitable for beginners who are learning item response theory.
#' @details launches 'shiny' interface
#' @keywords IRT
#'
#' @export
irtawsi <- function() {
  shiny::runApp(appDir = system.file("irtawsi", package="irtawsi"))
}
