% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_important_medvals.r
\name{pull_important_medvals}
\alias{pull_important_medvals}
\title{Pull mediation values needed for mediation diagram}
\usage{
pull_important_medvals(a, medi, mdres)
}
\arguments{
\item{a}{this is the initial model (i.e., a to b in the diagram; i.e., main predictor to mediator)}

\item{medi}{this is the mediation model (i.e., the full model; i.e., a to c controlling for b)}

\item{mdres}{this is the output from the mediation model you ran with the mediation package}
}
\description{
This function takes the output from a mediation model via the mediation package and returns a2b, b2c, and a2c values so that you can make a digestible diagram!
}
\keyword{mediation}
\keyword{pull_important_medvals,}
