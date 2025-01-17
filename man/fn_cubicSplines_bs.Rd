% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fns_splines.R
\name{fn_cubicSplines_bs}
\alias{fn_cubicSplines_bs}
\title{Create a basis for a cubic spline}
\usage{
fn_cubicSplines_bs(x, knots, varName, Boundary.knots = NULL)
}
\arguments{
\item{x}{the vector of data}

\item{knots}{the inner knots}

\item{varName}{the prefix to be used for the column names}

\item{Boundary.knots}{the boundary knots.  Can be NULL in which cases the
0.001 and 0.999 percentiles are used.}
}
\value{
a list: bx_ the basis as a sparse matrix; cnames the column names
}
\description{
Create a basis for a cubic spline.
}
