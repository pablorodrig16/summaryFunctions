% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pCalc.R
\name{p.Wilcox}
\alias{p.Wilcox}
\title{Wilcoxon Rank Sum test of data x group}
\usage{
p.Wilcox(data, group)
}
\arguments{
\item{data}{is a numeric or integer vector with data}

\item{group}{is a factor vector for grouping}
}
\description{
Calculates Wilcox P (two tails) of data by group (or Mann-Whitney test)
}
\examples{
p.Wilcox (mtcars$mpg, mtcars$cyl)
}
