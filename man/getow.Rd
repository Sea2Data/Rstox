% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rstox_base.r
\name{getow}
\alias{getow}
\title{Function for controling overwriting of prjects.}
\usage{
getow(ow, projectPath, onlyone = TRUE, msg = TRUE)
}
\arguments{
\item{ow}{Specifies whether to ovewrite existing project: If TRUE, overwrite; if FALSE, do not overwrite; if NULL (default), aks the user to confitm overwriting.}

\item{projectPath}{The full path to the project folder.}

\item{onlyone}{Logical: If TRUE, only one project is checked (no for loop).}

\item{msg}{Logical: If TRUE, print messages to the console.}
}
\description{
Function for controling overwriting of prjects.
}
\keyword{internal}
