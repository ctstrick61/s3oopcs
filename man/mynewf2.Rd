% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myttest.R
\name{mynewf2}
\alias{mynewf2}
\title{Print Environment of Function Data}
\usage{
mynewf2(x, y, Var.equal = True, paired = True)
}
\arguments{
\item{x}{vector of data}

\item{y}{vector of data}
}
\value{
Environment of data
}
\description{
shows the parent of the environment and returns the bindings of all the functions.
}
\details{
Need RLANG package to operate. Calls the function and looks inside its enclosure with the active time of evaluation
}
\examples{
mynewf2(x=rnorm(30,10,12), y=rnorm(40, 7, 10))
}
