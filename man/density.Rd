% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density.R
\name{density}
\alias{density}
\alias{logitnormal}
\alias{lognormal}
\alias{make_gamma}
\alias{multinom}
\alias{normal}
\alias{prop}
\title{Probability Density Functions for 
Probabilistic Uncertainty Analysis}
\usage{
normal(mean, sd)

lognormal(mean, sd, meanlog, sdlog)

make_gamma(mean, sd)

prop(prob, size)

multinom(...)

logitnormal(mu, sigma)
}
\arguments{
\item{mean}{Distribution mean.}

\item{sd}{Distribution standard deviation.}

\item{meanlog}{Mean on the log scale.}

\item{sdlog}{SD on the log scale.}

\item{prob}{Proportion.}

\item{size}{Size of sample used to estimate proportion.}

\item{...}{Dirichlet distribution parameters.}

\item{mu}{Mean on the lgit scale.}

\item{sigma}{SD on the logit scale.}
}
\description{
Probability Density Functions for 
Probabilistic Uncertainty Analysis
}

