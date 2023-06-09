% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doHaulSampling.R
\name{doHaulSampling}
\alias{doHaulSampling}
\title{Do haul sampling based on a point process simulation}
\usage{
doHaulSampling(res, type, xrng, yrng, sel = 1)
}
\arguments{
\item{res}{\itemize{
\item ppplist point process simulation object
}}

\item{type}{\itemize{
\item character string indicating gear type
}}

\item{xrng}{\itemize{
\item coordinates indicating haul start, end  (i.e., extent of area swept along x axis)
}}

\item{yrng}{\itemize{
\item coordinates indicating net extent (i.e., extent of area swept along y axis)
}}

\item{sel}{\itemize{
\item gear selectivity (probability of capturing individual in area swept)
}}
}
\value{
tibble with number of individuals caught, area swept, and cpue
}
\description{
Function to do haul sampling based on a point process simulation.
}
