#*********************************************
#*********************************************
#' Extending the StoX survey estimation software into R
#' 
#' This package contains most of the functionality of the StoX software, which is used for assessment of fish and other marine resources based on biotic and acoustic survey and landings data, among other uses. Rstox is intended for further analyses of such data, facilitating iterations over an arbitrary number of parameter values and data sets.
#'
#' The core funciton of the package is \code{\link{getBaseline}} which runs a StoX project and retrieves the output and input parameters. The functions \code{\link{runBootstrap}} and \code{\link{imputeByAge}} are used by StoX for estimating the variance of the survey estimate. The functions \code{\link{getReports}} and \code{\link{getPlots}} are used to run report and plot funcitons relevant for the type of StoX project.
#' 
#' Rstox supports a variety of other uses, such as downloading biotic and acoustic data from the Norwegian Marine Data Center through \code{\link{getNMDinfo}} and \code{\link{getNMDdata}}. The data are placed in StoX projects, enabling the data to be read using \code{\link{getBaseline}}. The function \code{\link{readXMLfiles}} can be used to simply read an acoustic or biotic xml file into R memory (via a temporary StoX project). The simpler function \code{\link{downloadXML}} reads an arbitrary XML file into a list. It is also possible to write acoustic and biotic XML file in the MND echosounder (version 1.0) and biotic (version 1.4 and 3.0) format.
#' 
#' Rstox also contains functions for generating and reporting parallel or zigzag transect lines for use in a survey through \code{\link{surveyPlanner}}. 
#' 
#' Soon to be implemented is running the Estimated Catch at Age (ECA) model develped by the Norwegian Computing Center and the Norwegian Institute of Marine Research. 
#'
"_PACKAGE"
#> [1] "_PACKAGE"
