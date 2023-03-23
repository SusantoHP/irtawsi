\name{irtawsi}
\alias{irtawsi}
\title{Items Response Theory Analysis with Steps and Interpretation}
\usage{
irtawsi()
}
\value{No values are returned, launches 'shiny' interface}
\description{
Analysis of Dichotomous and polytomous data using unidimensional Item Response Theory model (Chalmers (2012) <doi:10.18637/jss.v048.i06>) with user friendly Graphical User Interface. When Items Respons Theory assumptions are not met, this package tries to make wise decisions by providing suggestions for solutions. The unidimensional assumption refers to(DeMars (2010) <doi:https://doi.org/10.1093/acprof:oso/9780195377033.001.0001>), which is analyzed using the 'psych' package. The suggestions when the local independence assumption is not met refer to (Nguyen, et al (2014) <doi:10.1007/s40271-013-0041-0>, Paek & Cole (2019) <doi:10.4324/9781351008167>, Petersen (2005) <doi:10.1007/s11136-005-1259-7>, Toland (2014) <doi:10.1177/0272431613511332>).The suggestions used on the invariance assumption parameter refer to (Xu, et al (2020) <doi:10.3758/s13428-020-01426-z>, Guenole & Brown (2014) <doi:10.3389/fpsyg.2014.00980>). Suitable if used by beginners who are learning Item Response Theory. The features offered by this package are: (1) Instructions for conducting an IRT analysis. (2) Provide the best IRT model recommendations. (3) Automatically provides interpretation of the results. (4) Results can be downloaded in the form of a report with an html extension. (5) Can be used in English or Indonesian.
}
\examples{
if(interactive()){
## Run this code for launching the Graphic User Interface
irtawsi()
}
}
\references{


Cai, L. & Monro, S. (2014). \emph{A new statistic for evaluating item response theory
models for ordinal data}. National Center for Research on Evaluation, Standards,
& Student Testing. Technical Report.

Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory
Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
\doi{10.18637/jss.v048.i06}

DeMars, C. (2010). \emph{Item Response Theory}. \doi{https://doi.org/10.1093/acprof:oso/9780195377033.001.0001}

Guenole, N., & Brown, A. (2014). The consequences of ignoring measurement invariance for path coefficients in structural equation models. \emph{Frontiers in Psychology, 5}. \doi{https://doi.org/10.3389/fpsyg.2014.00980}

Maydeu-Olivares, A. (2013). Goodness-of-Fit Assessment of Item Response Theory Models. \emph{Measurement: Interdisciplinary Research & Perspective, 11} (3), 71–101. \doi{https://doi.org/10.1080/15366367.2013.831680}

Maydeu-Olivares, A. (2014). Evaluating the Fit of IRT Models. In S. P. Reise & D. A. Revicki (Eds.), \emph{Handbook of Item Response Theory Modeling} 129–145. Routledge. \doi{https://doi.org/10.4324/9781315736013-15}

Nguyen, T. H., Han, H.-R., Kim, M. T., & Chan, K. S. (2014). An Introduction to Item Response Theory for Patient-Reported Outcome Measurement. \emph{The Patient - Patient-Centered Outcomes Research, 7} (1), 23–35. \doi{https://doi.org/10.1007/s40271-013-0041-0}

Paek, I., & Cole, K. (2019). Using R for Item Response Theory Model Applications. \emph{Routledge}. \doi{https://doi.org/10.4324/9781351008167}

Petersen, M. A. (2005). Introduction to Nonparametric Item Response Theory. \emph{Quality of Life Research, 14} (4), 1201–1202. \doi{https://doi.org/10.1007/s11136-005-1259-7}

Retnawati, H. (2015). Karakteristik Butir Tes dan Analisisnya. Uny, 53(5).

Toland, M. D. (2014). Practical Guide to Conducting an Item Response Theory Analysis. \emph{The Journal of Early Adolescence, 34} (1), 120–151. \doi{https://doi.org/10.1177/0272431613511332}

Xu, J., Zhang, Q., & Yang, Y. (2020). Impact of violations of measurement invariance in cross-lagged panel mediation models. \emph{Behavior Research Methods, 52} (6), 2623–2645. \doi{https://doi.org/10.3758/s13428-020-01426-z}



}
