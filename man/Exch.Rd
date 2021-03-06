% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exch.R
\name{Exch}
\alias{Exch}
\title{Generate a normal distributed sample X with dimension n *p with mean 0
and an exchangable covariance matrix where a_{ij} = rho^{i-j}}
\usage{
Exch(rho, P, N)
}
\arguments{
\item{rho}{auto correlation structure}

\item{P}{dimension of the data}

\item{N}{sample size of the data}
}
\value{
X generated data
}
\description{
Generate a normal distributed sample X with dimension n *p with mean 0
and an exchangable covariance matrix where a_{ij} = rho^{i-j}
}
\examples{
 Exch(rho = 0.2, P = 10, N = 300)

}
