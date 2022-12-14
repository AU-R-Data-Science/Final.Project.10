% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BascFunction.R
\name{Beta.init}
\alias{Beta.init}
\title{Calculate the initial beta value}
\usage{
Beta.init(X, Y)
}
\arguments{
\item{X}{An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors(Not including intercept).}

\item{Y}{A factor vector indicating the category of response}
}
\value{
The initial beta vector for loss function
}
\description{
{Initial values for optimization obtained from the least-squares formula \deqn{(X^{T}X)^{−1}X^{T}Y}\\}
}
\author{
Fangjian Yang
}
