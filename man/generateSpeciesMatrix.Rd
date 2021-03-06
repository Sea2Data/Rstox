% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rstox_reports.r
\name{generateSpeciesMatrix}
\alias{generateSpeciesMatrix}
\alias{mergeSpeciesMatrix}
\alias{aggregateBySpeciesCategory}
\title{Generate species matrix with biotic stations as rows and station information and the species present in the \code{ref} file as columns. One matrix generated per variable \code{var}.}
\usage{
generateSpeciesMatrix(projectName, ref, years, check = TRUE,
  downloadProjects = FALSE, timeout = 600,
  model = list("ReadBioticXML", FilterBiotic = list(BioticData =
  "ReadBioticXML")), specVar = "noname", catVar = "Speccat",
  stationVar = c("cruise", "serialno"), bioticProc = "FilterBiotic",
  var = c("weight", "count"), max.sampletype = 49, list.out = TRUE,
  ...)

mergeSpeciesMatrix(...)

aggregateBySpeciesCategory(projectName, ref, specVar = "noname",
  specVar.bio = specVar, specVar.ref = specVar, catVar = "SpecCat",
  bioticProc = "FilterBiotic", stationVar = c("cruise", "serialno"),
  var = c("weight", "count"), na.as = 0, drop.out = TRUE,
  close = TRUE, msg = TRUE, ...)
}
\arguments{
\item{projectName}{A vector of cruise series names providing the data.}

\item{ref}{Either a data frame with the reference information about the species, containing the variables specied in "specVar", "catVar", or the path to a csv file holding this data frame.}

\item{years}{A vector of the years to process.}

\item{downloadProjects}{Logical: If TRUE download the projects, which must be done the first time, or if one needs to re-download.}

\item{timeout}{Used on Windows if problems with incompletely downloded data occurs (server problems).}

\item{model}{The model to use in the projects. Per default only two processes are needed: ReadBioticXML reads the biotic data, and FilterBiotic filters away stations, gear, species and so on. Set FilterBiotic to filter data from ReadBioticXML.}

\item{specVar}{The name of the variable identifying the species the data. This variable must be present also in 'ref'.}

\item{catVar}{The name of the variable identifying the species categories to which the data should be grouped.}

\item{stationVar}{The variables in the data defining biotic stations (the combination of these variables is used).}

\item{bioticProc}{The StoX process from which the data are extracted (one of "ReadBioticXML" and "FilterBiotic" (the default, useful for filtering out e.g. gears)).}

\item{var}{The variables to return data of.}

\item{...}{Parameters passed on to getBaseline(), e.g. for changing the baseline parameters (adding filters).}

\item{specVar.bio, specVar.ref}{Names of the species columns in the data and in the \code{ref} file. When only specifying \code{specVar}, a common column name is assumed.}

\item{na.as}{The value to use for missing data (e.g., species categories that are not present in a station).}

\item{drop.out}{Logical: If TRUE (default) unlist if only one variable is given in \code{var}.}

\item{close}{Logical: If TRUE (default) close the project after reading the data.}

\item{model}{Parameters such as FilterBiotic <- list(BioticData="ReadBioticXML",  FishStationExpr = "gear =~['3270','3271', '3293', '']  and gearcondition < 3 and trawlquality =~['1','3']  and fishstationtype != ['2','C']", SampleExpr = "genetics != '7'").}

\item{projectName}{The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).}
}
\value{
A list of data frames with stations as rows and station information and the species present in the \code{ref} file as columns.
}
\description{
Generate species matrix with biotic stations as rows and station information and the species present in the \code{ref} file as columns. One matrix generated per variable \code{var}.
}
\keyword{internal}
