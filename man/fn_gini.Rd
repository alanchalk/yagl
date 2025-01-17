% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fns_performance.R
\name{fn_gini}
\alias{fn_gini}
\title{Calculate gini}
\usage{
fn_gini(actual, pred, weight = NULL)
}
\arguments{
\item{actual}{vector of actual responses}

\item{pred}{vector of predicted responses}

\item{weight}{}
}
\description{
Calculate gini, can allow for weights.  First calculates AUC using trapezium rule
    and then sets gini = 2 * (0.5 - auc) - where auc was defined based on
    predictions sorted in increasing value.  Actual and predictions are expected
    to be numeric and neither of them are limited to being 0-1.
}
