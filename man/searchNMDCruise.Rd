% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rstox_NMD.R
\name{searchNMDCruise}
\alias{searchNMDCruise}
\title{Search for a cruise given the cruise number and ship name.}
\usage{
searchNMDCruise(cruisenrANDshipname, datatype, ver = 1,
  API = "http://tomcat7.imr.no:8080/apis/nmdapi")
}
\arguments{
\item{cruisenrANDshipname}{A vector of two elements, the first being the cruise number and the second the ship name.}

\item{datatype}{The type of data requested. Currently implemented are "echosunder" and "biotic", while "landing" and "ctd" are in the pipeline. datatype=NULL (default) returns all possible data.}

\item{ver}{The version of the API. As of 2015-05 only version 1 is available. Version 2 will include the possibility to return a list of all cruises.}

\item{API}{The path to the API.}
}
\description{
The NMD API enables searching for a cruise identifyer given the cruise number and ship name.
}
\keyword{internal}