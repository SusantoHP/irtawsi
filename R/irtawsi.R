#'
#' @title Items Response Theory Analysis with Steps and Interpretation
#' @return Nothing
#' @description Analysis of Dichotomous and polytomous data using unidimensional Item Response Theory model (Chalmers (2012) <doi:10.18637/jss.v048.i06>) with user friendly Graphical User Interface. When Items Respons Theory assumptions are not met, this package tries to make wise decisions by providing suggestions for solutions. The unidimensional assumption refers to (DeMars (2010, ISBN:978-0-19-537703-3)), which is analyzed using the 'psych' package. The suggestions when the local independence assumption is not met refer to (Nguyen, et al (2014) <doi:10.1007/s40271-013-0041-0>, Paek & Cole (2019) <doi:10.4324/9781351008167>, Petersen (2005) <doi:10.1007/s11136-005-1259-7>, Toland (2014) <doi:10.1177/0272431613511332>).The suggestions used on the invariance assumption parameter refer to (Xu, et al (2020) <doi:10.3758/s13428-020-01426-z>, Guenole & Brown (2014) <doi:10.3389/fpsyg.2014.00980>). Suitable if used by beginners who are learning Item Response Theory. The features offered by this package are: (1) Instructions for conducting an IRT analysis. (2) Provide the best IRT model recommendations. (3) Automatically provides interpretation of the results. (4) Results can be downloaded in the form of a report with an html extension. (5) Can be used in English or Indonesian.
#' @details launches 'shiny' interface
#' @keywords IRT
#'
#' @export

irtawsi <- function() {
  shiny::runApp(appDir = system.file("irtawsi", package="irtawsi"))
}
