#*********************************************
#*********************************************
#' Get polygon area and convert to or from geographic and Cartesian coordinates.
#'
#' \code{polyArea} calculates the area in nautical mile squared of a multipolygon. \cr \cr
#' \code{geo2xy} converts from geographic to or from Cartesian coordinates or the inverse. \cr \cr
#' \code{polyAreaOld} to be deleted. \cr \cr
#' \code{getMatrixList} converts the input to a list of matrices. \cr \cr
#' \code{getMultipolygon} converts the input to a multipolygon wkt string. \cr \cr
#' \code{getSpatial} converts the input to a Spatial object. \cr \cr
#' \code{matrix2multipolygon} identical to \code{getMultipolygon}. \cr \cr
#' \code{multipolygon2matrix} identical to \code{getMatrixList}. \cr \cr
#' 
#' @param x					Either a two column matrix of x and y coordinates, indicating only one polygon, or a list of such matrices, indicating several polygons. If a list of lists of two column matrices are given, the first matrix of each list is the polygon, and the following are subtractions. \code{wkt} cal also be a wkt string such as "MULTIPOLYGON(((4 55, 15 56, 15 59, 4 59, 4 55)))".
#' @param multipolygon		A multipolygon string.
#' @param drop				Logical: If TRUE drop the list if only one multipolygon or only one polygon is given.
#' @param data.frame.out	Logical: If TRUE convert the matrices to data frames with columns x and y.
#' 
#' @return \code{polyArea} returns area in nmi squared, and \code{matrix2multipolygon} returns a MULTIPOLYGON wkt.
#'
#' @examples
#' projectName <- "Test_Rstox"
#' g <- getBaseline(projectName, input="proc", proc=NULL, drop=FALSE)
#' # Get the stratum polygons:
#' multipolygon <- g$processData$stratumpolygon$Polygon
#' # Get stratum area in square nautical miles:
#' lapply(g$processData$stratumpolygon$Polygon, polyArea)
#' # Get cartesian locations using Azimuthal Equidistant projection (preserving distance):
#' ###proj <- getProjString(multipolygon)
#' ###xy <- lapply(g$processData$stratumpolygon$Polygon, geo2xy)
#' ###xlim=range(unlist(lapply(xy, "[", , "x")))
#' ###ylim=range(unlist(lapply(xy, "[", , "y")))
#' ###plot(NULL, xlim=xlim, ylim=ylim)
#' ###lapply(xy, lines, col='black', pbg='white')
#' ###lapply(xy, polyArea, input="xy")
#'
#' @export
#' @importFrom rgeos gArea
#' @rdname polyArea
#' 
polyArea <- function(x, par=list(proj="laea", units="kmi", lon_0=NA, lat_0=NA, x_0=0, y_0=0, ellps="WGS84", datum="WGS84"), input="longlat", ...) {
	# Convert to xy if given in longlat:
	if(input=="longlat"){
		x <- geo2xy(x, par=par, inv=FALSE, ...)
	}
	# Get the spatial object and use gArea() to get the area:
	out <- getSpatial(x)
	out <- sum(rgeos::gArea(out, byid=T))
	return(out)
}
#' 
#' @export
#' @rdname polyArea
#' 
geo2xy <- function(x, par=list(proj="aeqd", units="kmi", lon_0=NA, lat_0=NA, x_0=0, y_0=0, ellps="WGS84", datum="WGS84"), inv=FALSE, data.frame.out=FALSE, add=FALSE, ...){
	# Get projection string:
	par <- getProjString(par=par, ..., x=x)
	
	# Convert to a list of matrices and run the project() function on all elements:
	#out <- x[ ,c("x", "y"), drop=FALSE]
	out <- getMatrixList(x, data.frame.out=data.frame.out)
	#x <- rapply(x, rgdal::project, proj=par, inv=inv, how="replace")
	out <- projectMatrixList(out, par=par, inv=inv, data.frame.out=data.frame.out)
	#if(!inv){
		colnames(out) <- if(inv) c("Longitude", "Latitude") else c("x", "y")
		#}
	# Add the other columns:
	#if(ncol(x)>2){
	#	out <- cbind(out, x[, -(1:2)])
	#}
	if(add){
		out <- cbind(out, x)
	}
	
	attr(out, "proj") <- par
	out
}
#' 
#' @export
#' @importFrom rgeos readWKT gArea
#' @importFrom sp CRS spTransform proj4string
#' @rdname polyArea
#' 
polyAreaOld <- function(x) {
	# We need rgdal when AreaMethod=Acurate in StratumArea!!!!
	###if(is.numeric(x)){
	###	x <- paste0("MULTIPOLYGON(((", paste(apply(x, 1, paste, collapse=" "), collapse=", "), ")))")
	###}
	x <- matrix2multipolygon(x)
	p <- rgeos::readWKT(x)
	# Define projection for the wkt
	sp::proj4string(p) <- sp::CRS("+proj=longlat +ellps=WGS84")	
	# define the proj4 definition of Lambert Azimuthal Equal Area (laea) CRS with origo in wkt center:
	# Units: international nautical miles:
	laea.CRS<-CRS(paste0("+proj=laea +lat_0=",p@polygons[[1]]@labpt[2]," +lon_0=",p@polygons[[1]]@labpt[1],
		" +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=kmi +no_defs"))
	# project data points from longlat to given laea
	p1 <- sp::spTransform(p, laea.CRS)
	sum(rgeos::gArea(p1, byid=T)) # Returns area
	# The result is very near compared known online geodesic planimeters (+- 0.001 naut.m)
}
#' 
#' @export
#' @keywords internal
#' @rdname polyArea
#' 
getMatrixList <- function(x, drop=TRUE, data.frame.out=FALSE){
	if(isSpatial(x)){
		x <- spatial2matrixList(x, drop=drop, data.frame.out=data.frame.out)
	}
	else if(isMultipolygon(x)){
		x <- multipolygon2spatial(x)
		x <- spatial2matrixList(x, drop=drop, data.frame.out=data.frame.out)
	}
	else if(isMatrixList(x) && data.frame.out){
		x <- rapplyKeepDataFrames(x, as.data.frame)
	}
	else if(!isMatrixList(x)){
		warning("Unrecognized input by isSpatial(), isMatrixList() or isMultipolygon(). Returned unaltered")
	}
	x
}
#'
#' @export
#' @keywords internal
#' @rdname polyArea
#' 
getMultipolygon <- function(x, drop=TRUE, data.frame.out=FALSE){
	if(isSpatial(x)){
		x <- spatial2matrixList(x, drop=drop, data.frame.out=data.frame.out)
		x <- matrixList2multipolygon(x)
	}
	else if(isMatrixList(x)){
		x <- matrixList2multipolygon(x)
	}
	else if(!isMultipolygon(x)){
		warning("Unrecognized input by isSpatial(), isMatrixList() or isMultipolygon(). Returned unaltered")
	}
	x
}
#'
#' @export
#' @importFrom rgeos readWKT
#' @importFrom sp SpatialPolygons Polygons Polygon
#' @keywords internal
#' @rdname polyArea
#' 
getSpatial <- function(x){
	if(isMatrixList(x)){
		#x <- matrixList2multipolygon(x)
		#x <- rgeos::readWKT(x)
		if(matrixListLevel(x)==1){
			x <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(x)), ID=1)))
		}
		else{
			x <- lapply(seq_along(x), function(i) sp::Polygons(list(sp::Polygon(x[[i]])), ID=i))
			x <- sp::SpatialPolygons(x)
		}
	}
	else if(isMultipolygon(x)){
		x <- rgeos::readWKT(x)
	}
	else if(!isSpatial(x)){
		warning("Unrecognized input by isSpatial(), isMatrixList() or isMultipolygon(). Returned unaltered")
	}
	x
}
#' 
#' @export
#' @rdname polyArea
#' 
matrix2multipolygon <- getMultipolygon
#' 
#' @export
#' @rdname polyArea
#' 
multipolygon2matrix <- getMatrixList


#*********************************************
#*********************************************
#' Utility functions for coordinate transformation between geographic and Cartesian coordinates, and calculation of multipolygon area.
#'
#' \code{getProjString} generates a projection string in the proj4 format. \cr \cr
#' \code{projectMatrixList} projects a list of matrices. \cr \cr
#' \code{spatial2matrixList} converts a Spatial object to a list of matrices. \cr \cr
#' \code{matrixList2multipolygon} converts a list of matrices to a multipolygon wkt string. \cr \cr
#' \code{multipolygon2spatial} converts a multipolygon wkt string to a Spatial object. \cr \cr
#' \code{isSpatial} determines whether the input is of Spatial class. \cr \cr
#' \code{isMatrixList} determines whether the input is a list of matrices or data frames. \cr \cr
#' \code{isMultipolygon} determines whether the input is a multipolygon wkt (well known text) string. \cr \cr
#' \code{matrixListLevel} determines the number of levels in a list of matrices, where 1 denotes a matrix, 2 denotes a list of matrices, and 3 denotes a list of lists of matrices. \cr \cr
#' \code{rapplyKeepDataFrames} lapplies the funciton \code{FUN} throughout the first two levels of a list but not into any data frames. \cr \cr
#' 
#' @param x					One of three onjects depending on the funciton: (1) a two column matrix of x and y coordinates, indicating only one polygon, or a list (of lists) of such matrices, indicating several polygons in a multipolygon. If a list of lists of two column matrices are given, the first matrix of each list is the polygon, and the following are subtractions. (2) A wkt string such as "MULTIPOLYGON(((4 55, 15 56, 15 59, 4 59, 4 55)))". (3) A spatial object.
#' @param drop				Logical: If TRUE drop the list if only one multipolygon or only one polygon is given.
#' @param data.frame.out	Logical: If TRUE convert the matrices to data frames with columns x and y.
#' @param par				A list of proj4 parameters.
#' @param ...				Further proj4 parameters overriding those in \code{par}.
#' @param inv				Logical: If TRUE, do the inverse conversion in rgdal::project().
#' @param FUN				The funciton to apply to the elements in \code{rapplyKeepDataFrames}.
#'
#' @export
#' @importFrom rgeos readWKT
#' @keywords internal
#' @rdname getProjString
#' 
getProjString <- function(par=list(proj="laea", units="kmi", lon_0=NA, lat_0=NA, x_0=0, y_0=0, ellps="WGS84", datum="WGS84"), ..., x=NULL, list.out=FALSE){
	#if(length(x) && is.character(attr(x, "proj"))){
	#	return(attr(x, "proj"))
	#}
	
	if(is.character(par)){
		par <- lapply(strsplit(par, "+", fixed=TRUE)[[1]][-1], strsplit, "=", fixed=TRUE)
		par <- lapply(par, unlist, recursive=FALSE)
		parNames <- sapply(par, head, 1)
		par <- lapply(par, "[", 2)
		names(par) <- parNames
	}
	
	# Include the freely specified args, set to override existing definitions:
	par <- c(list(...), par)
	# If lon_0 or lat_0 are given as NA, use the center of the polygon:
	# SHOULD WE USE rgeos::gCentroid() INSTEAD HERE, SINCE THIS CELECTS A POINT WHICH MAY BE LOCATED BETWEEN POSSIBLE SUB POLYGONS????:
	if(is.na(par$lon_0)){
		#getCoordsPolygon <- function(x){
		#	do.call(rbind, lapply(p, function(x) x@polygons[[1]]@Polygons[[1]]@coords))
		#}
		p <- getMultipolygon(x)
		#if(is.list(p)){
		#	p <- getCoordsPolygon(p)
		#}
		p <- rgeos::readWKT(p)
		#p <- lapply(p, rgeos::readWKT)
		#p[[1]]@polygons[[1]]@Polygons[[1]]@coords <- getCoordsPolygon(p)
		#p <- p[[1]]
		temp <- rgeos::gCentroid(p)@coords
		par$lon_0 <- temp[1]
		par$lat_0 <- temp[2]
	}
	# Get the CRS, using only the first of duplicatedly specified parameters:
	par <- par[!duplicated(names(par))]
	# Convert the args to a vector of strings:
	if(!list.out){
		par <- paste(paste0("+", names(par), "=", unlist(par, recursive=FALSE)), collapse=" ")
	}
	#par <- sp::CRS(par)
	par
}
#'
#' @export
#' @importFrom rgdal project
#' @keywords internal
#' @rdname getProjString
#' 
projectMatrixList <- function(x, par=list(proj="laea", units="kmi", lon_0=NA, lat_0=NA, x_0=0, y_0=0, ellps="WGS84", datum="WGS84"), inv=FALSE, data.frame.out=FALSE){
	out <- rapplyKeepDataFrames(x, function(y) rgdal::project(data.matrix(y), proj=par, inv=inv))
	if(data.frame.out){
		out <- rapplyKeepDataFrames(out, as.data.frame)
	}
	out
}
#'
#' @export
#' @keywords internal
#' @rdname getProjString
#' 
spatial2matrixList <- function(x, drop=TRUE, data.frame.out=FALSE){
	
	applyDataFrame <- function(x, data.frame.out=FALSE){
		if(data.frame.out){
			if(is.list(x)){
				x <- lapply(x, as.data.frame)
			}
			else{
				x <- as.data.frame(x)
			}
		}
		x
	}
	
	# Function for extracting the coordinates of spatialPolygon:
	getCoordsMultipolygon <- function(y, data.frame.out){
		out <- lapply(y@Polygons, slot, "coords")
		applyDataFrame(out, data.frame.out)
		#######out <- y@Polygons[[1]]@coords
		###if(data.frame.out){
		###	out <- lapply(out, as.data.frame)
		###}
		###out
	}
	# Function for extracting the coordinates of spatialPoints:
	getCoordsSpatialPoints <- function(y, data.frame.out){
		out <- y@coords
		applyDataFrame(out, data.frame.out)
		###if(data.frame.out){
		###	out <- lapply(out, as.data.frame)
		###}
		###out
	}
	# Function for extracting the coordinates of spatialLines:
	getCoordsSpatialLines <- function(y, data.frame.out){
		out <- lapply(y@Lines, slot, "coords")
		applyDataFrame(out, data.frame.out)
		#######out <- y@Polygons[[1]]@coords
		###if(data.frame.out){
		###	if(is.list(out)){
		###		out <- lapply(out, as.data.frame)
		###	}
		###	else{
		###		out <- as.data.frame(out)
		###	}
		###}
		###out
	}
	
	#' @importFrom sp disaggregate
	### # Disaggregate the polygons:
	### out <- sp::disaggregate(x)
	# Extract the coordinates:
	if("polygons" %in% slotNames(x)){
		out <- sp::disaggregate(x)
		out <- lapply(out@polygons, getCoordsMultipolygon, data.frame.out=data.frame.out)
	}
	else if("coords" %in% slotNames(x)){
		out <- getCoordsSpatialPoints(x, data.frame.out=data.frame.out)
	}
	else if("lines" %in% slotNames(x)){
		out <- lapply(x@lines, getCoordsSpatialLines, data.frame.out=data.frame.out)
	}
	if(drop){
		# Drop when only one multipolygon:
		out <- lapply(out, function(x) if(length(x)==1) x[[1]] else x)
		# Drop when only one polygon:
		if(length(out)==1){
			out <- out[[1]]
		}
	}
	
	return(out)
}
#'
#' @export
#' @keywords internal
#' @importFrom utils head tail
#' @rdname getProjString
#' 
matrixList2multipolygon <- function(x){
	# Merge to pairs of x, y:
	mergeToPairs <- function(x){
		x <- apply(x, 1, paste, collapse=" ")
		x <- paste(x, collapse=", ")
		x <- paste0("(", x, ")")
		return(x)
	}
	addParantheseis <- function(x){
		paste0("(", paste0(x, collapse=", "), ")")
	}
	isConnected <- all(utils::head(x, 1) == utils::tail(x, 1))
	x <- rapplyKeepDataFrames(x, mergeToPairs)
	if(isConnected){
		x <- lapply(x, addParantheseis)
		x <- addParantheseis(x)
		x <- paste0("MULTIPOLYGON", x)
	}
	else{
		x <- paste0("LINESTRING", x)
	}
	return(x)
}
#'
#' @export
#' @importFrom rgeos readWKT
#' @keywords internal
#' @rdname getProjString
#' 
multipolygon2spatial <- function(x){
	rgeos::readWKT(x)
}
#'
#' @export
#' @keywords internal
#' @rdname getProjString
#' 
isSpatial <- function(x){
	isS4(x) && any(c("lines", "coords", "polygons") %in% slotNames(x))
}
#'
#' @export
#' @keywords internal
#' @rdname getProjString
#' 
isMatrixList <- function(x){
	length(matrixListLevel(x)) > 0
}
#'
#' @export
#' @keywords internal
#' @rdname getProjString
#' 
isMultipolygon <- function(x){
	is.character(x) && length(grep("MULTIPOLYGON", x))>0
}
#'
#' @export
#' @keywords internal
#' @rdname getProjString
#' 
matrixListLevel <- function(x){
	isM <- function(y){
		is.matrix(y) || is.data.frame(y)
	}
	isM1 <- isM(x)
	isM2 <- is.list(x) && isM(x[[1]])
	isM3 <- is.list(x) && isM(x[[1]][[1]])
	isM <- c(isM1, isM2, isM3)
	if(any(isM)){
		return(which(isM)[1])
	}
	else{
		return(NULL)
	}
}
#'
#' @export
#' @keywords internal
#' @rdname getProjString
#' 
rapplyKeepDataFrames <- function(x, FUN, ...){
	level <- matrixListLevel(x)
	FUN0 <- function(x, ...){
		#do.call(FUN, c(list(data.matrix(x)), ...))
		do.call(FUN, c(list(x), ...))
	}
	FUN1 <- function(x, ...){
		lapply(x, FUN0, ...)
	}
	FUN2 <- function(x, ...){
		lapply(x, FUN1, ...)
	}
	if(level==1){
		FUN0(x, ...)
	}
	else if(level==2){
		FUN1(x, ...)
	}
	else if(level==3){
		FUN2(x, ...)
	}
}



#*********************************************
#*********************************************
#' Get polygon area and convert to or from geographic and Cartesian coordinates. The strata are converted to Cartesian coordinates using a common projection that preserves distance (azimuthal equal distance projection, aeqd in the proj4 library). 
#'
#' @param projectName   	The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param type				The type of the transects, repeated to the number of stratums. See details for possible values. Case insensitive.
#' @param bearing			The survey bearing of each transect, given in one of 4 ways: (1) by codes "N", "NW", "W", "WS", "S", "SE", "E", "NE", (2) as angles counter clockwise from EAST on degrees, (3) as a string "along" or "across" the stratum orientation as obtained by the principal components of the stratum borders after populating with 1000 points with nearly equal separation along the border in geographic corrdinates, or (4) as a data frame with columns "lon_start", "lon_stop", "lat_start", "lat_stop" defining the angles between a start and end point for each stratum.
#' @param retour			Logical: If TRUE continue with the transects back to the start point of the stratum.
#' @param toursFirst		Logical: If TRUE do all tours first followed by the retours (only effective if retour is TRUE).
#' @param hours				The time to spend in the stratum, given in hours.
#' @param knots				The speed to use in the stratum, given in knots.
#' @param nmi				The distance to travel in the stratum in nautical miles.
#' @param seed				The seed(s) to use for the random transects. If not given as a vector of length equal to the number of strata, random seed are drawn using \code{\link{getSeedV}}.
#' @param dt				The density of points populating the stratum polygons. The first element denotes the density in degrees to use for the geographical coordinates (Longitude, Latitude), whilst the second element if present denotes the density in nautical miles to use for the Cartesian coordinates before converting back to geographical coordinates. The first element preserves the stratum borders when converting to Cartesian coordinates, whilst the second elements preserves the shortest travel distance of the transects when converting back to geographical coordinates (inducing curved lines in geographical coordinates). 
#' @param plot				Deprecated: Plots the transects in Cartesian coordintes. Use \code{\link{plotStratum}} instead().
#' @param t0				The start time of the survey, set to Sys.time() by default.
#' @param shapenames		A list of length 3 giving the names of the longitude, latitude and stratum column in the shape files.
#' @param equalEffort		Logical: If TRUE, assign effort proportional to the area of each stratum.
#' @param x					The output from \code{\link{surveyPlanner}}.
#' @param transect			Logical: If TRUE, plot the transects.
#' @param centroid			Logical: If TRUE, plot the centroid of the strata.
#' @param transport_alpha	The transparency to use for the transport paths.
#' @param margin			The margin to accept for deviation between input and output total travelled distance. Setting this may result in unequal coverage, since the forcing the total traveled distance towards a fixed value may force specific paths due to the shape of the stratum. Rather, increase the input \code{hours}, og accept the "noise" in the output total traveled distance. For \code{plotStratum} margin is the margin around the borders of the survey area in units of its dimensions.
#' @param google			Logical: If TRUE download Google maps.
#' @param zoom				The zoom to use for google maps
#' @param aspectratio		The aspect ratio of the plot, adjusted automatically by default.
#' @param xlab				The x label for the plot.
#' @param ylab				The y label for the plot.
#' 
#' @details Strindberg and Buckland (2004): Strindberg, S., & Buckland, S. T. (2004). Zigzag survey designs in line transect sampling. Journal of Agricultural, Biological, and Environmental Statistics, 9(4), 443
#' The following types are implemented:
#' @return \code{surveyPlanner} returns a list of the following elements: 
#' \describe{
#'	\item{"Parallel"}{"Parallel transects"}
#'	\item{"EqAngZZ"}{"Equal angle zigzag sampler (not implemented), Strindberg and Buckland (2004)"}
#'	\item{"EqSpZZ"}{"Equal space zigzag sampler, Strindberg and Buckland (2004)"}
#'	\item{"AdjAngZZ"}{"Adjusted angle zigzag sampler (not implemented), Strindberg and Buckland (2004)"}
#'	\item{"RectEnclZZ"}{"Rectangular enclosure zigzag sampler, Harbitz and Holmin (2018)"}
#'	\item{"CurvEnclZZ"}{"Curved enclosure zigzag sampler (not implemented), Harbitz and Holmin (2018)"}
#' }
#' 
#' @return \code{surveyPlanner} returns a list of the following elements: 
#' \describe{
#'	\item{"transects"}{"A data frame with the geographic and Cartesian transect coordinates for all strata, along with start, mid and stop time and sailed distance"}
#'	\item{"totalSailedDist"}{"A data fram with total, transect, transport and input (nmi) sailed distance, also given in percent of 'nmi'."}
#'	\item{"lonlat"}{"The input stratum polygons (Longitude, Latitude) in a list with one matrix per stratum"}
#'	\item{"parameters"}{"A list of parameters for each stratum: type, bearing, retour, hours, knots, nmi, seed"}
#'	\item{"proj"}{"The projection string used to convert from (Longitude, Latitude) to (x, y)"}
#'	\item{"area"}{"The area og each polygon in square nautical miles"}
#'	\item{"centroid"}{"The centroid of the strata, used in the projection"}
#'	\item{"strata"}{"The stratum names"}
#' }
#'
#' @export
#' @importFrom sp Lines Line SpatialLines
#' @importFrom rgeos gIntersection
#' @import ggplot2
#' @import data.table
#' @importFrom tools file_path_sans_ext
#' @importFrom utils head tail
#' @importFrom rgdal readOGR
#' @rdname surveyPlanner
#' 
surveyPlanner <- function(projectName, type="Parallel", bearing="N", rev=FALSE, retour=FALSE, toursFirst=FALSE, hours=240, t0=NULL, knots=10, nmi=NULL, seed=0, dt=1/60, plot=FALSE, margin=NULL, shapenames=list(Longitude="long", Latitude="lat", Stratum="id"), equalEffort=FALSE, byStratum=TRUE) {
#surveyPlanner <- function(projectName, shapefiles=NULL, type="Parallel", bearing="N", rev=FALSE, retour=FALSE, toursFirst=FALSE, hours=240, t0=NULL, knots=10, nmi=NULL, seed=0, dt=1/60, plot=FALSE, margin=NULL, shapenames=list(Longitude="long", Latitude="lat", Stratum="id"), equalEffort=FALSE, byStratum=TRUE) {
	
	############################################################
	######################## Functions: ########################
	############################################################
	# Function used for populating a path with points of constant distance 'dt':
	populatePath <- function(xy, N=100, dt=NULL, list.out=FALSE, addInfo=TRUE){
		# Function for getting the time sequence in one stretch, as 
		tseqOne <- function(i, t, dt){
			tnew <- seq(t[i], t[i+1], dt)
			tnew
		}
	
		# Save the projection attribute:
		temp <- attributes(xy)
		if(!is.data.frame(xy)){
			xy <- as.data.frame(xy)
		}
		# Get cummulative traveled distance, which we interpret as time (for constant speed):
		difft <- sqrt(rowSums(do.call(cbind, lapply(xy, diff))^2))
		# Return immediately if these is no difference between the points:
		if(max(difft)==0){
			if(addInfo){
				return(cbind(xy, Populated=0))
			}
			else{
				return(xy)
			}
		}
		
		# Get the cummulative time:
		cumtime <- c(0, cumsum(difft))
		# Get the increment in time:
		if(length(dt)==0){
			dt <- cumtime[length(cumtime)] / N
		}
	
		# Get the new time vector and the new x and y values:
		tnew <- unlist(lapply(seq_len(length(difft)), tseqOne, t=cumtime, dt=dt))
		xynew <- apply(xy, 2, function(z) approx(x=cumtime, y=z, xout=tnew)$y)
		# Special care taken if only one point was populated (the last original point is added below):
		if(length(dim(xynew)) < 2){
			xynew <- t(xynew)
		}
		xynew <- as.data.frame(xynew)
		
		# Add the last point to close the path:
		xynew <- rbind(xynew, xy[nrow(xy), ])
		if(addInfo){
			# Add a column denoting which rows are populated:
			Populated <- double(nrow(xynew)) + 1
			Populated[c(1, length(Populated))] <- 0
			xynew <- cbind(xynew, Populated=Populated)
		}
		
		# Include the projection in the output:
		attr(xynew, "proj") <- temp$proj
		if(list.out){
			list(xy=xynew, t=tnew, dt=dt)
		}
		else{
			xynew
		}
	}
	
	# Function for rotating 2-D points by an angle:
	rotate2d <- function(x, ang, paired=FALSE, data.frame.out=FALSE){
		# Create a rotation matrix and apply it to the input data:
		A = matrix(c(cos(ang), sin(ang), -sin(ang), cos(ang)), 2, 2, byrow=TRUE)
		out <- t(A %*% t(x))
		if(data.frame.out){
			out <- as.data.frame(out)
		}
		colnames(out) <- c("x", "y")
		out
	}
	
	# Function for calculating the bearing of a survey:
	getBearing <- function(bearing, deg=TRUE, data=NULL, proj=NULL, rev=FALSE){
		if(is.character(bearing)){
			# If the data is given as a list of stratum polygons, and bearing="along" or "across", populate the polygon with points and get the angles from the first or second PCA, respectively:
			if(tolower(bearing[1]) %in% c("along", "across")){
				ind <- which(tolower(bearing) == c("along", "across"))
				# Get the PCAs in xy (not lonlat):
				data <- lapply(data, geo2xy, data.frame.out=TRUE, par=proj)
				ev <- lapply(data, function(x) eigen(cov(populatePath(x, N=1e3))))
				# Get the angles:
				angles <- sapply(ev, function(x) atan(x$vectors[2,ind] / x$vectors[1,ind]))
			}
			else{
				getPredefined <- function(bearing){
					# Interpret strings as degrees:
					strings <- c("N", "NW", "W", "WS", "S", "SE", "E", "NE")
					angles <- c(90, 135, 180, 225, 270, 315, 0, 45) * pi/180
					hit <- which(tolower(strings) == tolower(bearing))
					if(length(hit)){
						return(angles[hit])
					}
					else{
						warning(paste0("'bearing not matching any of'", paste(strings, collapse=", ")))
						return(bearing)
					}
				}
			
				angles <- unlist(lapply(bearing, getPredefined))
			}
		}
		else if(is.list(bearing)){
			if(!all(c("lon_start", "lon_stop", "lat_start", "lat_stop") %in% names(bearing))){
				bearing <- as.data.frame(bearing)
				names(bearing) <- c("lon_start", "lon_stop", "lat_start", "lat_stop")
			}
			# Get the start and stop point in Cartesian coordinates:
			start <- geo2xy(bearing[, c("lon_start", "lat_start")], data.frame.out=TRUE, par=proj)
			stop <- geo2xy(bearing[, c("lon_stop", "lat_stop")], data.frame.out=TRUE, par=proj)
			# Get the angles from start to stop:
			dx <- stop[, 2] - start[, 2]
			dy <- stop[, 1] - start[, 1]
			angles <- atan(dy / dx)
		}
		else{
			if(deg){
				angles <- bearing*pi/180
			}
			else{
				angles <- bearing
			}
		}
		if(rev){
			angles <- angles + pi
		}
		angles
	}
	
	# Order tours first and then retours reversed, i.e., forth and back along the entire survey instead of in each stratum:
	orderTourRetour <- function(x){
		x <- split(x, interaction(x$Stratum, x$Retour))
		# The indices to use for the ordering:
		ind <- c(seq_len(length(x)/2))
		ind <- c(ind, length(x) + 1 - ind)
		# Reorder to have all tour first followed by all retours in the reversed order:
		x <- x[ind]
		# Return data frame:
		as.data.frame(data.table::rbindlist(x, idcol=FALSE))
	}
	
	# Small function for reversing order of the 
	revMatrix <- function(x, byrow=TRUE){
		if(byrow){
			x[rev(seq_len(nrow(x))), ]
		}
		else{
			x[, rev(seq_len(ncol(x)))]
		}
	}
	
	# Function for adding stratum ends:
	addEndTransects <- function(intersects, poly){
		# Function for getting a transect at the end with the inverse angle of the first/last transect:
		getEndTransectOne <- function(poly, intersects, last=FALSE){
			# If we are actually appending to the last and not the first transect, reverse the transects here for convenience:
			if(last){
				intersects@coords <- revMatrix(intersects@coords)
			}
			# Get the first point and the line segments:
			first <- intersects@coords[1,]
			firstLine <- rbind(intersects@coords[1,], intersects@coords[1,] + diff(intersects@coords[1:2,]) * c(-1,1) * 2)
			# Create a spatial lines object:
			spatialLinesEnd <- sp::Lines(list(sp::Line(firstLine)), ID = 1)
			spatialLinesEnd <- sp::SpatialLines(list(spatialLinesEnd))
	
			# Find intersection point between the line and the polygon:
			intersectsFirst <- rgeos::gIntersection(poly, spatialLinesEnd)
			if(length(intersectsFirst)<3){
				intersects@coords <- intersects@coords[FALSE,]
				return(intersects)
			}
			# Pick out the point farthes from the intersection point:
			dist <- rowSums((intersectsFirst@coords - matrix(first, nrow=nrow(intersectsFirst@coords), ncol=2, byrow=TRUE))^2)
			intersectsFirst@coords <- intersectsFirst@coords[which.max(dist), ,drop=FALSE]
			# Add the first point to obtain a valid transect of two points:
			intersectsFirst@coords <- if(last) rbind(first, intersectsFirst@coords) else rbind(intersectsFirst@coords, first)
			# Return the transect:
			intersectsFirst
		}
		
		# Get the end transects at the start and end:
		first <- getEndTransectOne(poly, intersects)
		last <- getEndTransectOne(poly, intersects, last=TRUE)
		intersects@coords <- rbind(first@coords, intersects@coords, last@coords)
		intersects
	}
	
	## Function for ordering points in each element of a list to be increasing by x and then y:
	orderTransectsByXY <- function(x, down=FALSE){
		orderTransectsByXYOne <- function(x, down=FALSE){
			x[order(x$x, x$y, decreasing=down), ]
		}
		lapply(x, orderTransectsByXYOne, down=down)
	}
	
	# Function for selecting the first point of each list element, and generating zigzag grid by merging consecutive points:
	parallel2zigzag <- function(x){
		oldNames <- names(x)
		# Get the first element of each line, requiring that the data have been linked by alternate direction using linkClosest() first:
		start <- data.table::rbindlist(lapply(x, utils::head, 1))
		# Generate the indices used to split the data into line segments:
		tempSeq <- seq_len(nrow(start) - 1)
		transecind <- rep(tempSeq, each=2)
		# Generate the indices used to access the line segments in 'start':
		ind <- c(outer(0:1, tempSeq, "+"))
		# Select the line segments and split to one list per segment:
		start <- start[ind,]
		start <- split(start, transecind)
		names(start) <- oldNames[-length(oldNames)]
		start
	}
	
	# Function for extracting the start stop and mid Cartesian positions:
	getXY_StartMidStop <- function(coords){
		coords$x_start <- coords$x
		coords$x_stop  <- c(coords$x_start[-1], NA)
		coords$x_mid   <- (coords$x_start + coords$x_stop) / 2
		coords$y_start <- coords$y
		coords$y_stop  <- c(coords$y_start[-1], NA)
		coords$y_mid   <- (coords$y_start + coords$y_stop) / 2
		coords
	}
	
	# Function for getting start and stop time and mid time of track segments (NA for the last node):
	getDistTime <- function(x, t0, byStratum=TRUE){
		# Funciton for getting the distance and time for one stratum:
		getDistTime_OneStratum <- function(coords, t0){
			insertIfMissing <- function(x, value){
				if(length(x)==0){
					return(value)
				}
				else{
					areNA <- is.na(x)
					x[areNA] <- value[areNA]
					x
				}
			}
		
			# Get start, mid and stop time and dist:
			segmentLengths <- sqrt( (coords$x_stop - coords$x_start)^2 + (coords$y_stop - coords$y_start)^2 )
			segmentLengths <- segmentLengths[-length(segmentLengths)]
			CsegmentLengths <- cumsum(segmentLengths)
			coords$dist_start <- insertIfMissing(coords$dist_start, c(0, CsegmentLengths) )
			coords$dist_stop  <- insertIfMissing(coords$dist_stop,  c(CsegmentLengths, NA) )
			coords$dist_mid   <- insertIfMissing(coords$dist_mid,   (coords$dist_start + coords$dist_stop) / 2 )
		
			# Add the survey start time:
			t0 <- unclass(as.POSIXct(t0))
			coords$time_start <- insertIfMissing(coords$time_start, as.POSIXct(coords$dist_start / coords$knots * 3600 + t0, origin="1970-01-01"))
			coords$time_stop  <- insertIfMissing(coords$time_stop,  as.POSIXct(coords$dist_stop / coords$knots * 3600 + t0, origin="1970-01-01"))
			coords$time_mid   <- insertIfMissing(coords$time_mid,   as.POSIXct(coords$dist_mid / coords$knots * 3600 + t0, origin="1970-01-01"))
		
			coords$segmentLengths <- insertIfMissing(coords$segmentLengths, c(segmentLengths, NA))
		
			coords <- coords[setdiff(names(coords), c("x", "y"))]
			coords
		}
		
		# Split into strata if byStratum:
		x <- split(x, if(byStratum) x$Stratum else 1)
		# Repeat the start time:
		t0 <- rep(t0, length.out=length(x))
		
		# Get the distance and time and re-merge to a data frame:
		out <- lapply(seq_along(x), function(i) getDistTime_OneStratum(x[[i]], t0=t0[i]))
		out <- as.data.frame(data.table::rbindlist(out))
		
	}
	
	# Function for extracting the coords from an intersect object returned from rgeos::gIntersection(), and splitting into a data frame per transect:
	getIntersectsCoordsList <- function(intersects){
		# Get the transect IDs as the first characters of the rownames:
		transectID <- sapply(strsplit(rownames(intersects@coords), " "), utils::tail, 1)
		intersectsCoordsList <- split(as.data.frame(intersects@coords), transectID)
		# Order the transects by names, corresponding to x value:
		if(length(names(intersectsCoordsList))){
			intersectsCoordsList <- intersectsCoordsList[order(as.numeric(names(intersectsCoordsList)))]
		}
		intersectsCoordsList
	}
	
	# Function for splitting pairs of points, represented by pairs of lines in the 'coords', into a list of these paris as data frames:
	coords2coordsList <- function(coords){
		ind <- rep(seq_len(nrow(coords)/2), each=2)
		split(as.data.frame(coords), ind)
	}
	
	# Function for linking consecutive transects stored in a list of data frames, in a way so that if the previous transect is uppwards in y, the next will be downwards, and vice versa:
	linkClosest <- function(x){
		# Assume ordered data frames by x and then y, e.g., obtained by orderTransectsByXY():
		getUp <- function(x){
			up <- unlist(x[c(1, nrow(x)), "y"])
			up <- diff(up) > 0
			up
		}
		if(length(x)==1){
			return(x)
		}
		
		# Get first up:
		up <- getUp(x[[1]])
		
		# Then move through the list and order according to the previous line segment:
		for(i in 1 + seq_along(x[-1])){
			npoints <- nrow(x[[i]])
			if(up == getUp(x[[i]])){
				x[[i]] <- x[[i]][rev(seq_len(npoints)),]
			}
			up <- !up
		}
		return(x)
	}
	
	# Expand the transects beyond each end, and intersect again with these lines:
	expand_transect_ends <- function(x, fact=1000){
		firstToLast <- apply(x, 2 , function(y) y[c(1, nrow(x))])
		mid <- matrix(colMeans(firstToLast), byrow=TRUE, nrow=2, ncol=2)
		out <- (firstToLast - mid) * fact + mid
		out
	}
	
	# Various internal functions for spatial data which are in use now or in the future.
	splitByCol <- function(x, col, prefix=""){
		if(isTRUE(prefix)){
			prefix <- col
		}
		out <- split(x, x[[col]])
		names(out) <- paste0(prefix, names(out))
		out
	}
	splitByTwoCols <- function(x, cols, prefix=""){
		prefix <- rep(prefix, length.out=2)
		if(isTRUE(prefix)){
			prefix <- cols
		}
		out <- splitByCol(x, cols[1], prefix=prefix[1])
		out <- lapply(out, splitByCol, cols[2], prefix=prefix[2])
		out
	}
	splitByThreeCols <- function(x, cols, prefix=""){
		prefix <- rep(prefix, length.out=3)
		if(isTRUE(prefix)){
			prefix <- cols
		}
		out <- splitByCol(x, cols[1], prefix=prefix[1])
		out <- lapply(out, splitByTwoCols, cols[2:3], prefix=prefix[2:3])
		out
	}
	matrixList2Lines <- function(x, ID="ID", coordNames=c("x", "y")){
		sp::Lines(lapply(x, function(y) sp::Line(data.matrix(y[,coordNames]))), ID=ID)
	}
	Transect2SpatialLines <- function(x, coordNames=c("x", "y"), prefix="ID"){
		IDs <- names(x)
		if(length(IDs)==0){
			IDs <- paste0(prefix, seq_along(x))
		}
		sp::SpatialLines(lapply(seq_along(x), function(i) matrixList2Lines(x[[i]], ID=IDs[i], coordNames=coordNames)))
	}
	Stratum2ListOfSpatialLines <- function(x, coordNames=c("x", "y")){
		out <- lapply(x, Transect2SpatialLines, coordNames=coordNames)
		names(out) <- names(x)
		out
	}
	TransectMatrix2ListOfSpatialLines <- function(x, coordNames=c("x", "y")){
		out <- splitByThreeCols(x, c("Stratum", "Transect", "Segment"), prefix=TRUE)
		out <- Stratum2ListOfSpatialLines(out, coordNames=coordNames)
		out
	}
	
	# Function used for linking strata, by incerting the start positions of the strata to the end position of the previous:
	linkStrata <- function(x){
		etEnd <- which(is.na(x$x_mid))
		stratumOrderStart <- etEnd[-length(etEnd)]
		atCols <- c("x_start", "y_start")
		replaceCols <- c("x_stop", "y_stop")
		x[stratumOrderStart, replaceCols] <- x[stratumOrderStart + 1, atCols]
	
		x
	}
	############################################################
	############################################################
	
	
	############################################################
	########## Function for generating the transects ###########
	############ in one direction (tour or retour): ############
	############################################################
	getTransectsOneDirection <- function(xGrid, corners, spatialLinesPolygon, seed=0, type="Parallel", retour=FALSE){
		# If we are on a retour, reverse the order of the grid points:
		downRandom <- seed %% 2 == 0
		if(retour){
			downRandom <- !downRandom 
			}
			
		# For parallel transects we need to shift by the half transectSpacing on the retour:
		if(type == "Parallel" && retour){
			halfTransectSpacing <- diff(xGrid[1:2]/2)
			xGrid <- xGrid + halfTransectSpacing
		}
		
		##### Create the grid to intersect with the stratum polygon: #####
		# Get the grid of lines which are used to find intersection points between transects and polygon borders:
		grid <- data.frame(
			x = rep(xGrid, each=2), 
			y = rep(unlist(corners[c("ymin", "ymax")]), length(xGrid)), 
			Ind = rep(seq_along(xGrid), each=2))
		grid <- split(grid, grid$Ind)
		###grid <- lapply(grid, data.matrix)
	
		# For zigzag transects with equal coverage, convert the grid to zigzag lines, by selecting every other point:
		if(type == "RectEnclZZ"){
			# Order alternately, and if on a retour order oppositely from the default:
			#grid <- orderTransectsByXY(grid, down=downRandom)
			#grid <- orderAlternateByY(grid, decreasing=downRandom)
			# Applyt the ordering of the grid prior to linking consecutive high-high and low-low points:
			grid <- orderTransectsByXY(grid, down=downRandom)
			grid <- linkClosest(grid)
			# Select the first end point of each grid line, and generate zigzag grid by merging consecutive points:
			grid <- parallel2zigzag(grid)
		}
		spatialLinesGrid <- lapply(seq_along(grid), function(Ind) sp::Lines(list(sp::Line(data.matrix(grid[[Ind]][,1:2]))), ID=Ind))
		spatialLinesGrid <- sp::SpatialLines(spatialLinesGrid)
		
		# Here we put the EqualAngleZigzag sampler and the AdjustedAngleZigzag sampler, and in the future the CurvedEnclosureZigzag sampler and the FlexibleHeadCurveZigzag sampler? These are all iterative, in the sence that we need to intersect one transect with the stratum polygon in order to move to the next transect:
		##### Intersect the grid with the stratum polygon: #####
		# Get intersection points between the grid and the polygon borders:
		intersects <- rgeos::gIntersection(spatialLinesPolygon, spatialLinesGrid, byid=TRUE)
		intersectsCoordsList <- getIntersectsCoordsList(intersects)
		#ind_xGrid <- as.numeric(names(intersectsCoordsList))
	
	   	intersectsCoordsList <- orderTransectsByXY(intersectsCoordsList, down=downRandom)
		intersectsCoordsList <- linkClosest(intersectsCoordsList)
		
		# For zigzag transects, set the end point of each transect to the start point of the next, and remove the last transect:
		if(type == "EqSpZZ"){
			intersectsCoordsList <- parallel2zigzag(intersectsCoordsList)
			# Add end points to the zigzag transects. For this we need a spatial object:
			temp <- intersects
			# Use the idcol="Transect" to split the transects afterwards:
			temp@coords <- data.matrix(data.table::rbindlist(intersectsCoordsList, idcol=FALSE))
			temp <- addEndTransects(temp, spatialLinesPolygon)
			# Split the transects into a list again:
			intersectsCoordsList <- coords2coordsList(temp@coords)
			# remove the last value since the last element of intersectsCoordsList was removed in the 
			#ind_xGrid <- ind_xGrid[-length(ind_xGrid)]
			xGrid_EqSpZZ <- sapply(sapply(intersectsCoordsList, "[", 1, 1), function(x) which.min(abs(x-xGrid)))
			#xGrid_EqSpZZ <- findInterval(sapply(intersectsCoordsList, "[", 1, 1), xGrid)
		}
		
		# The transects may intersect with the stratum polygon borders more than once, so we need to intersect again and split transects into subtransects when intersecting more than twice (two intersectiins at the borders):
		#intersectsCoordsList <- lapply(intersectsCoordsList, function(x) )
		intersectsCoordsList <- lapply(intersectsCoordsList, expand_transect_ends)
		
		spatialLinesTransects <- lapply(intersectsCoordsList, list)
		spatialLinesTransects <- Transect2SpatialLines(spatialLinesTransects, prefix="")
		
		
		intersects <- rgeos::gIntersection(spatialLinesPolygon, spatialLinesTransects, byid=TRUE)
		intersectsCoordsList <- getIntersectsCoordsList(intersects)
		
		# Assure that the new intersects are between the relevant grid lines:
		if(type == "EqSpZZ"){
			selectInsidexGrid <- function(ind, xGrid, margin=0.01){
				bin <- xGrid_EqSpZZ[ind]
				width <- xGrid[bin + 1] - xGrid[bin]
				valid <- xGrid[bin] - margin * width <= intersectsCoordsList[[ind]][,1] & intersectsCoordsList[[ind]][,1] <= xGrid[bin + 1] + margin * width
				intersectsCoordsList[[ind]][valid, , drop=FALSE]
			}
			intersectsCoordsList <- lapply(seq_along(intersectsCoordsList), selectInsidexGrid, xGrid=xGrid)
		}
		
		# Do not use dornrandom here, since we have achieved the randomness in the direction of the first line using this variable above:
		#intersectsCoordsList <- orderTransectsByXY(intersectsCoordsList, down = if(type == "RectEnclZZ") downRandom else FALSE)
		intersectsCoordsList <- orderTransectsByXY(intersectsCoordsList, down = FALSE)
		intersectsCoordsList <- linkClosest(intersectsCoordsList)
		
		# Split transects into sub transects, but keep the transect ID:
		nsegmentsPerTransect <- sapply(intersectsCoordsList, nrow) / 2
		#if(any(nsegmentsPerTransect != 1)){
			nsubtransects <- sum(nsegmentsPerTransect)
			temp <- vector("list", nsubtransects)
		
			ind <- 0
			for(i in seq_along(intersectsCoordsList)){
				intersectsCoordsList[[i]] <- cbind(intersectsCoordsList[[i]], Transect=i)
				#temp[ind + seq_len(nsegmentsPerTransect[i])] <- split(intersectsCoordsList[[i]], rep(seq_len(nsegmentsPerTransect[i]), each=2))
				temp[ind + seq_len(nsegmentsPerTransect[i])] <- coords2coordsList(intersectsCoordsList[[i]])
				ind <- ind + nsegmentsPerTransect[i]
			}
			intersectsCoordsList <- temp
		#}
		
		
		# If on a retour, reverse order of the transects and within all transects:
		if(retour){
			intersectsCoordsList <- rev(intersectsCoordsList)
			#if(type != "RectEnclZZ"){
				intersectsCoordsList <- lapply(intersectsCoordsList, function(x) x[seq(nrow(x), 1), ])
				#}
		}
		# Add a column denoting tour or retour:
		intersectsCoordsList <- lapply(intersectsCoordsList, cbind, Retour=as.numeric(retour))
		
		intersectsCoordsList
	}
	############################################################
	############################################################
	
	
	############################################################
	#### Function for generating one set of transects given ####
	#### the 'area', the tracklength minus the width of the ####
	### stratum 'nmi_rest', the seed factor 'fac', the range ###
	#### of x values in the rectangle that has been rotated ####
	### to have x along the 'bearing' (the direction in which ##
	#### to propagate through the stratum) ('xmin', 'xmax'), ###
	############ and the x,y positions 'xyRotated': ############
	############################################################
	#getTransectsByArea <- function(nmi_rest, area, fac, corners, xyRotated, type="Parallel", bearing="N", t0=NULL, knots=10, seed=0, retour=FALSE, plot=FALSE){
	getTransectsByArea <- function(nmi_rest, area, fac, corners, xyRotated, type="Parallel", bearing="N", knots=10, seed=0, retour=FALSE, plot=FALSE){
		# Get the number of transects:
		
		transectSpacing <- area / nmi_rest
		# If the transect sould go tour-retour, use half spacing for parallel andtransects, and for zigzag simply go back with opposite order:
		#if(type == "Parallel" && retour){
		if(retour){
			transectSpacing <- transectSpacing * 2
		}
		# Set the leftmost position of the grid lines:
		firstTransectPos <- 2 * transectSpacing * fac
		# Get x positions of the grid:
		xGrid <- seq(corners$xmin - 2 * firstTransectPos, corners$xmax + 2 * transectSpacing, by=transectSpacing)
		
		# Convert the polygon to a SpatialLines object for use in the intersection between polygon borders and transect lines:
	   	spatialLinesPolygon <- lapply(seq_len(nrow(xyRotated)-1), function(ind) sp::Lines(list(sp::Line(xyRotated[ind + 0:1, ])), ID=ind))
		spatialLinesPolygon <- sp::SpatialLines(spatialLinesPolygon)
	
		# Generate the transects in one direction:
		intersectsCoordsList <- getTransectsOneDirection(xGrid=xGrid, corners=corners, spatialLinesPolygon=spatialLinesPolygon, seed=seed, type=type, retour=FALSE)
		if(retour){
			intersectsCoordsList <- c(intersectsCoordsList, getTransectsOneDirection(xGrid=xGrid, corners=corners, spatialLinesPolygon=spatialLinesPolygon, seed=seed, type=type, retour=TRUE))
		}
		
		# If 'dt' is given as a two element vector, the second element indicates populating the transects with points of distance dt[2]:
		if(length(dt)>1){
			for(i in seq_along(intersectsCoordsList)){
				intersectsCoordsList[[i]] <- populatePath(intersectsCoordsList[[i]], dt=dt[2], addInfo=FALSE)
			}
		}
		
		# Add Transect and Transport columns:
		for(i in seq_along(intersectsCoordsList)){
			# Add also whether the segments are transports (the last line is transport):
			Transport <- c(double(nrow(intersectsCoordsList[[i]]) - 1), 1)
			Segment <- seq_len(nrow(intersectsCoordsList[[i]]))
			#intersectsCoordsList[[i]] <- cbind(intersectsCoordsList[[i]], Segment=Segment, Transect=i, Transport=Transport)
			intersectsCoordsList[[i]] <- cbind(intersectsCoordsList[[i]], Segment=Segment, Transport=Transport, knots=knots)
		}
		
		### # Add a column to each transect denoting start and end points
		# Combine to a matrix:
		coords <- as.data.frame(data.table::rbindlist(intersectsCoordsList, idcol=FALSE))
		# Update the segment IDs to account for multiple intersections:
		coords$Segment <- unlist(by(coords$Segment, coords$Transect, seq_along))
		
		if(plot){
			plot(xyRotated, type="o")
			lines(coords[,c("x", "y")], type="o", col=rainbow(nrow(coords)), lwd=2)
			abline(v=xGrid, col=4)
		}
	
		# Expand the data to contain start, mid and stop position, time and sailed distance, as well as segment length:
		coords <- getXY_StartMidStop(coords)
		
		# Get the time of the segments
		#time <- getTrackTime(coords=coords, knots=knots, t0=t0)
		return(c(list(coords=coords), time))
	}
	############################################################
	############################################################
	
	
	############################################################
	#### Function for generating transects for one stratum: ####
	############################################################
	transectsOneStratum <- function(stratum, xy, area, parameters, plot=FALSE, margin=NULL){
		# Get the parameters of the current stratum:
		parameters <- lapply(parameters, "[", stratum)
		
		# Rotate into a cartesian coordinate system having x axis aloing this bearing:
		xyRotated <- rotate2d(xy[[stratum]], parameters$bearing, data.frame.out=TRUE)
		
		# Get corners of the bounding box of the polygon (a slight value added to the y to ensure intersection with the polygon):
		dy <- diff(range(xyRotated$y))
		corners <- list(xmin=min(xyRotated$x), xmax=max(xyRotated$x), ymin=min(xyRotated$y) - dy*1e-9, ymax=max(xyRotated$y) + dy*1e-9)
		# Get the length of the stratum along the bearing:
		lengthOfStratum <- corners$xmax - corners$xmin
		
		# Subtract the length of the stratum, and return NULL if the traveled distance is shorter than this:
		if(parameters$nmi < lengthOfStratum){
			warning("The traveled distance specified by nmi or duration and knots is shorter than the length of the stratum")
		}
		nmi_rest <- parameters$nmi - lengthOfStratum
		
		# Get the random seed point for the transects:
		set.seed(parameters$seed)
		fac <- runif(1)
		
		# If margin is given, iterate to obtain transects with total track length deviating at most by 'margin' relative to the input track length (margin = 0.05 implies between 19  and 21 hours, say):
		temp <- getTransectsByArea(nmi_rest=nmi_rest, area=area[stratum], fac=fac, corners=corners, xyRotated=xyRotated, type=parameters$type, bearing=parameters$bearing, seed=parameters$seed, knots=parameters$knots, retour=parameters$retour, plot=plot)
		
		if(length(margin) && is.numeric(margin)){
			# Set the totalSailedDist, margin to use, and the last value for 'rest' and 'nmi_rest':
			totalSailedDist <- 0
			margin <- parameters$nmi * margin
			last_rest <- Inf
			last_nmi_rest <- Inf
			lastTemp <- NULL
			# Iterate to get a calculated tracklength within the margins
			while(abs(parameters$nmi - totalSailedDist) > margin){
				temp <- getTransectsByArea(nmi_rest=nmi_rest, area=area[stratum], fac=fac, corners=corners, xyRotated=xyRotated, type=parameters$type, bearing=parameters$bearing, seed=parameters$seed, knots=parameters$knots, retour=parameters$retour, plot=plot)
				# Update the tracklength and rest tracklength:
				totalSailedDist <- temp$totalSailedDist
				rest <- parameters$nmi - totalSailedDist
				# If increasing in rest value, break the loop and rerun with the previous settings:
				if(abs(last_rest) < abs(rest)){
					temp <- lastTemp
					warning(paste0("Sailed distance in stratum ", stratum, " did not converge to the desired sailed distance (",  parameters$nmi, " nmi). The closest used."))
					break
				}
				# Set the values of the last run:
				#nmi_rest <- nmi_rest + if(rest > 0) rest else 2 * rest
				last_nmi_rest <- nmi_rest
				last_rest <- rest
				lastTemp <- temp
				# Set new nmi_rest to use:
				nmi_rest <- nmi_rest + rest
			}
		}
		
		# Get x,y coordinates of the transects:
		coords <- temp$coords
		xcols <- c("x_start", "x_stop", "x_mid")
		ycols <- c("y_start", "y_stop", "y_mid")
		xy <- cbind(
			unlist(coords[, xcols]),
			unlist(coords[, ycols])
			)
		# Rotate back:
		xy <- rotate2d(xy, -parameters$bearing)
		coords[,c(xcols, ycols)] <- c(xy)
			
		# Add transect type:
		coords$Type <- parameters$type
		
		#return(c(list(geo=geo, xy=xy), temp[names(temp) != "coords"]))
		return(coords)
	}
	############################################################
	############################################################
	
	
	# Read the the stratum polygons from a folder of shape files or from a StoX project:
	
	
	if(all(isProject(projectName))){
		# Get the baseline output and number of strata:
		g <- getBaseline(projectName, endProcess="ReadProcessData", endProcess="ReadProcessData", input="proc", proc=NULL, drop=FALSE)
		strata <- g$processData$stratumpolygon$Stratum
		nstrata <- length(strata)
	
		# Get the strata polygons in geographic coordinates (longitude, latitude) in a list named with the strata names:
		lonlat <- lapply(g$processData$stratumpolygon$Polygon, getMatrixList, data.frame.out=TRUE)
		names(lonlat) <- strata
		lonlat <- lapply(lonlat, "colnames<-", c("Longitude", "Latitude"))
		# Test of southern hemisphere:
		#lonlat <- lapply(lonlat, function(x) {x$Latitude <- -x$Latitude; x})
		
		# Create a single data frame version of the strata polygons, with stratum as the third column, and get a common projection definition using the centroid of the system:
		lonlatAll <- data.table::rbindlist(lonlat, idcol="Stratum")
	}
	else if(all(file.exists(projectName))){
		if(length(projectName) == 1 && isTRUE(file.info(projectName)$isdir)){
			projectName <- list.files(projectName, full.names=TRUE)
		}
		dsn <- dirname(path.expand(projectName[1]))
		layer <- tools::file_path_sans_ext(basename(projectName[1]))
		shape <- rgdal::readOGR(dsn=dsn, layer=layer)
		shape <- ggplot2::fortify(shape)
		#lonlatAll <- data.frame(Longitude=shape$long, Latitude=shape$lat, Stratum=shape$id)
		lonlatAll <- data.frame(Longitude=shape[[shapenames$Longitude]], Latitude=shape[[shapenames$Latitude]], Stratum=shape[[shapenames$Stratum]])
		lonlat <- split(lonlatAll, lonlatAll$Stratum)
		lonlat <- lapply(lonlat, "[", c("Longitude", "Latitude"))
		strata <- unique(lonlatAll$Stratum)
		nstrata <- length(strata)
	}
	
	
	
	
	
	#
	#
	#
	#
	#if(length(shapefiles)){
	#	if(length(shapefiles) == 1 && isTRUE(file.info(shapefiles)$isdir)){
	#		shapefiles <- list.files(shapefiles, full.names=TRUE)
	#	}
	#	dsn <- dirname(path.expand(shapefiles[1]))
	#	layer <- tools::file_path_sans_ext(basename(shapefiles[1]))
	#	shape <- rgdal::readOGR(dsn=dsn, layer=layer)
	#	shape <- ggplot2::fortify(shape)
	#	#lonlatAll <- data.frame(Longitude=shape$long, Latitude=shape$lat, Stratum=shape$id)
	#	lonlatAll <- data.frame(Longitude=shape[[shapenames$Longitude]], Latitude=shape[[shapenames$Latitude]], Stratum=shape[[shapenames$Stratum]])
	#	lonlat <- split(lonlatAll, lonlatAll$Stratum)
	#	lonlat <- lapply(lonlat, "[", c("Longitude", "Latitude"))
	#	strata <- unique(lonlatAll$Stratum)
	#	nstrata <- length(strata)
	#}
	#else{
	#	# Get the baseline output and number of strata:
	#	g <- getBaseline(projectName, endProcess=1, input="proc", proc=NULL, drop=FALSE)
	#	strata <- g$processData$stratumpolygon$Stratum
	#	nstrata <- length(strata)
	#
	#	# Get the strata polygons in geographic coordinates (longitude, latitude) in a list named with the strata names:
	#	lonlat <- lapply(g$processData$stratumpolygon$Polygon, getMatrixList, data.frame.out=TRUE)
	#	names(lonlat) <- strata
	#	lonlat <- lapply(lonlat, "colnames<-", c("Longitude", "Latitude"))
	#	# Test of southern hemisphere:
	#	#lonlat <- lapply(lonlat, function(x) {x$Latitude <- -x$Latitude; x})
	#	
	#	# Create a single data frame version of the strata polygons, with stratum as the third column, and get a common projection definition using the centroid of the system:
	#	lonlatAll <- data.table::rbindlist(lonlat, idcol="Stratum")
	#}
	
	# Get the projection to use, centered at the centroid of the total survey area (using rgeos::gCentroid()):
	proj <- getProjString(proj="aeqd", x=lonlatAll[,c("Longitude", "Latitude")])
	
	# Draw seeds for the transects:
	if(length(seed)!=nstrata){
		set.seed(seed)
		seed <- getSeedV(seed, nstrata)
	}
	
	# Gather potential stratum dependent parameters in a list for convenience:
	if(length(t0)==0){
		t0 <- format(Sys.time(), tz="UTC")
	}
	# Get the total traveled length in nautical miles:
	#if(length(speed)){
	#	knots <- speed * 3600/1852
	#}
	if(length(nmi)==0){
		nmi <- hours * knots
	}
	# Get bearing of the stratum:
	bearing <- getBearing(bearing, data=lonlat, proj=proj, rev=rev)
	
	# Check for valid type:
	implementedTypes <- c("Parallel", "RectEnclZZ", "EqSpZZ")
	type <- implementedTypes[match(tolower(type), tolower(implementedTypes))]
	if(is.na(type)){
		warning(paste0("type not matching any of the implemented types (", implementedTypes, "). Parallel chosen"))
		type <- "Parallel"
	}
	#parameters <- list(type=type, bearing=bearing, retour=retour, hours=hours, knots=knots, nmi=nmi, speed=speed, seed=seed, t0=t0)
	parameters <- list(type=type, bearing=bearing, retour=retour, hours=hours, knots=knots, nmi=nmi, seed=seed, t0=t0)
	suppressWarnings(parameters <- lapply(parameters, rep, length.out=nstrata))
	
	# Extract the centroid:
	projList <- getProjString(proj="aeqd", x=lonlatAll[,c("Longitude", "Latitude")], list.out=TRUE)
	centroid <- data.frame(Longitude=projList$lon_0, Latitude=projList$lat_0)
	
	# Get the stratum areas:
	area <- unlist(lapply(lonlat, polyArea))
	if(equalEffort){
		parameters$nmi <- sum(parameters$nmi) * area / sum(area)
	}
	
	# Populate the stratum polygon borders with denser points, in order to preserve the geographic coordinate definition when converting to Cartesian coordinates (i.e., follow a latitude if two points are on the same latitude. If given only by two points, the azimuthal equal distance projection will follow the great circle, which in general will not coincide with the intended equal latitude path):
	lonlatPopulated <- lapply(lonlat, populatePath, dt=dt[1], addInfo=FALSE)
	
	# Convert to Cartesian:
	xy <- lapply(lonlatPopulated, geo2xy, data.frame.out=TRUE, par=proj)
	
	#########################################
	##### Get transects for all strata: #####
	#########################################
	out <- lapply(seq_along(xy), transectsOneStratum, xy=xy, area=area, parameters=parameters, plot=plot, margin=margin)
	outAll <- as.data.frame(data.table::rbindlist(out, idcol="Stratum"))
	
	# If requested, reorder the strata so that the tours are first, followed by the retours reversed:
	if(toursFirst && length(unique(outAll$Retour))==2){
		outAll <- orderTourRetour(outAll)
	}
	# Insert the next position as the stop position of the end of all strata except the last:
	if(!byStratum){
		outAll <- linkStrata(outAll)
	}
	# Get distance and time variables:
	outAll <- getDistTime(outAll, t0=parameters$t0, byStratum=byStratum)
	
	# Convert back to (longitude, latitude):
	# Create a data frame in which to put the geographic coordinates, and cbind this to the output:
	xcols <- c("x_start", "x_stop", "x_mid")
	ycols <- c("y_start", "y_stop", "y_mid")
	loncols <- c("lon_start", "lon_stop", "lon_mid")
	latcols <- c("lat_start", "lat_stop", "lat_mid")
	lonlat <- outAll[,c(xcols, ycols)]
	colnames(lonlat) <- c(loncols, latcols)
	outAll <- cbind(lonlat, outAll)
	
	
	xy <- cbind(unlist(outAll[, xcols]), unlist(outAll[, ycols]))
	geo <- geo2xy(xy, par=proj, inv=TRUE)
	outAll[,c(loncols, latcols)] <- c(geo)
	#outAll <- cbind(lonlat, outAll)
	#########################################
	
	
	# Reorder the columns to have Stratum, Transect, Segment first:
	firstcols <- c("Stratum", "Transect", "Segment", "Transport", "Retour", "Type")
	#outAll <- outAll[, c(firstcols, setdiff(colnames(outAll), firstcols)), with=FALSE]
	outAll <- outAll[, c(firstcols, setdiff(colnames(outAll), firstcols))]
	
	# Get the total sailed distance by stratum and after linking strata:
	totalSailedDist <- getSailedDist(outAll, nmi=parameters$nmi, byStratum=TRUE)
	totalSailedDist1 <- getSailedDist(outAll, nmi=sum(parameters$nmi), byStratum=FALSE)
	
	
	
	
	
	# Output the following objects:
	# 1. Data frame with the transects (Stratum, Transect, Segment, Transport, Retour, Type, start, stop and mid of lon, lat, x, y, dist and time, and segmentLengths)
	# 2. Data frame strata (Stratum, Area, type, bearing, retour, hours, knots, nmi, t0, seed, )
	# 3. Data frame with a single row for the entire survey (projectName, proj, longitude_centroid, latitude_centroid)
	# 4. A list of the input parameters
	
	
	
	
	#projectName, type="Parallel", bearing="N", rev=FALSE, retour=FALSE, toursFirst=FALSE, hours=240, t0=NULL, knots=10, nmi=NULL, seed=0, dt=1/60, plot=FALSE, margin=NULL, shapenames=list(Longitude="long", Latitude="lat", Stratum="id"), equalEffort=FALSE, byStratum=TRUE
	
	
	
	# Return a list of geographic coordinates for the stratum polygons and transects:
	list(
		transects = outAll, 
		totalSailedDist = totalSailedDist, 
		totalSailedDist1 = totalSailedDist1, 
		lonlat = lonlatAll, 
		parameters = parameters, 
		proj = proj, area = area, centroid = centroid, strata = strata)
}
#' 
#' @export
#' @import data.table
#' @rdname surveyPlanner
#' 
getSailedDist <- function(x, nmi=NULL, byStratum=TRUE){
	getSailedDist_OneStratum <- function(x){
		data.frame(
			total=sum(x$segmentLengths, na.rm=TRUE), 
			transect=sum(x$segmentLengths[x$Transport==0], na.rm=TRUE), 
			transport=sum(x$segmentLengths[x$Transport==1], na.rm=TRUE))
	}
	
	# If a data frame, split into strata (if byStratum):
	if(is.data.frame(x)){
		x <- split(x, if(byStratum) x$Stratum else 1)
	}
	# Otherwise merge to one stratum if !byStratum:
	else if(length(x)>1 && !byStratum){
		x <- list(as.data.frame(data.table::rbindlist(x)))
	}
	
	# Get sailed distances from each stratum
	out <- lapply(x, getSailedDist_OneStratum)
	out <- as.data.frame(data.table::rbindlist(out))
	
	# Add in percent if reference sailed distance nmi is given:
	if(length(nmi)){
		out <- cbind(out, nmi=nmi)
		outPercent <- round(out / out$nmi * 100, digits=2)
		colnames(outPercent) <- paste0(colnames(outPercent), "_percent")
		out <- cbind(out, outPercent)
	}
	if(nrow(out)==1){}
	rownames(out) <- if(nrow(out)==1) "Survey" else paste0("Stratum ", rownames(out))
	
	out
}
#' 
#' @export
#' @import ggplot2
#' @rdname surveyPlanner
#' 
plotStratum <- function(x, zoom=4, transect=TRUE, centroid=NULL, transport_alpha=0.1, google=FALSE, margin=0.5, aspectratio=NULL, xlab="Longitude", ylab="Latitude"){
	# Get the range in geographic coordinates:
	rangelonlat <- cbind(range(x$transects$lon_start), range(x$transects$lat_start))
	if(length(centroid)==0){
		# centroid <- unlist(x$centroid)
		centroid <- apply(rangelonlat, 2, mean)
	}
	#location <- colMeans(x[, c("Longitude", "Latitude")])
	
	# Get the data from the map package or alternatively from Google:
	if(google){
		gmap <- ggmap::get_map(location=centroid, zoom=zoom, maptype="terrain", source="google", col="bw")
		# Initiate the plot:
		p <- ggmap::ggmap(gmap)
	}
	else{
		# get the map and set the limits and aspect ratio:
		gmap <- map_data("world")
		spanlonlat <- apply(rangelonlat, 2, diff)
		fact <- c(-1, 1) * (1 + margin)
		xlim <- centroid[1] + fact * spanlonlat[1] / 2
		ylim <- centroid[2] + fact * spanlonlat[2] / 2
		# Adjust the aspect ratio by latitude:
		aspectratio <- 1 / cos(centroid[2] * pi/180)
		# Initiate the plot:
		p <- ggplot() + geom_polygon(data=gmap, aes(x=long, y=lat, group=group)) + coord_fixed(aspectratio, xlim=xlim, ylim=ylim)
	}
	
	# Add the strata:
	p <- p + geom_polygon(data=x$lonlat, aes(x=Longitude, y=Latitude, fill=Stratum, group=Stratum), colour="black", alpha=0.3, inherit.aes=FALSE)
	
	
	# Add transects:
	if(length(transect)){
		p <- p + 
			# Use the Retour as line type:
			hasRetour <- length(unique(x$transects$Retour))
			geom_segment(data=x$transects, aes(x=lon_start, y=lat_start, xend=lon_stop, yend=lat_stop, group=Stratum, colour=as.factor(Stratum), alpha = 1 - Transport, linetype=as.factor(Retour)), show.legend=TRUE) + 
			scale_alpha(range = c(transport_alpha, 1), guide=FALSE) + 
			scale_colour_discrete(guide=FALSE) + 
			if(hasRetour) scale_linetype(name="Retour") else scale_linetype(guide=FALSE) 
	}
	
	# Add labels:
	p <- p + xlab(xlab) + ylab(ylab)
	
	# Run the plot
	print(p)
}


#*********************************************
#*********************************************
#' Write transects generated by \code{\link{surveyPlanner}} to various report files.
#'
#' \code{writeTransectsToMAxSea} writes the transects of all strata to separate .asc files for import in MaxSea. \cr \cr
#' \code{writeTransectsINFO} writes the transects of all strata to separate hunam readable .txt files. \cr \cr
#' \code{writeTransectsCSV} writes the transects of all strata to separate csv files files. \cr \cr
#' \code{writeTransects} writes the data to CSV or NetCDF files. Only a selection of the columns are saved by default (override this by \code{cols}): The transect secifications ("Stratum", "Transect", "Segment", "Transport", "Retour"), the start and stop position, time and distance ("lon_start", "lon_stop", "lat_start", "lat_stop", "time_start", "time_stop", "dist_start", "dist_stop", "segmentLengths"). By default only transects and not transport stretches are saved (override this by \code{keepTransport}). \cr \cr
#' 
#' @param x								The output from \code{\link{surveyPlanner}}.
#' @param projectName   				The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param dir   						The path to the directory in which to put the files. If given, it takes precidence over the report directory of the project given by \code{projectName}.
#' @param item.type,item.id,item.col	The type (default 257), id (default 20) and color (default 1) to pass to MaxSea.
#' @param north,east					Strings denoting North and East to pass to MaxSea.
#' @param digits						Precision to use in the reports.
#' @param byStratum						Logical: If TRUE, create one file per stratum.
#' @param cols							The columns to write to file for \code{writeTransects}.
#' @param keepTransport					Logical: If TRUE, save also the transport stretches to the file.
#' @param text							A text to add to the files before the file extension.
#' @param ext							The file extension. For \code{writeTransects} \code{ext} = "nc" implies writing NetCDF files, and otherwise a CSV files are written.
#' @param ...							Parameters passed to \code{\link[utils]{write.table}}, \code{\link[utils]{write.csv}} and \code{\link[data.table]{fwrite}}. For \code{writeTransects} thie \code{...} contains the important variables \code{sep} specifying the separator used in CSV files, and \code{dateTimeAs} which defines how to save the time in \code{\link[data.table]{fwrite}}.
#' 
#' @return The file names.
#'
#' @export
#' @rdname writeTransects
#' 
writeTransectsMaxSea <- function(x, projectName, dir=NULL, item.type=257, item.id=20, item.col=1, north="N", east="E", digits=3, byStratum=TRUE, ...){
	
	# Function for writing one stratum:
	writeTransectsToMAxSea_OneStratum <- function(stratumInd, x, item.type=257, item.id=20, item.col=1, north="N", east="E", digits=3, ...){
		# Extract the file name from the names of the input:
		filename <- names(x$transects)[stratumInd]
		
		# Select the current stratum:
		this <- x$transects[[stratumInd]]
		
		# Create a matrix suited for loading in MaxSea:
		numrow <- nrow(this)
		out <- cbind(
			rep(item.type, numrow),
			rep(item.id, numrow),
			rep(item.col, numrow),
			round(this$abs_lat_start, digits=digits),
			this$NorthSouth, 
			#rep(north, numrow),
			round(this$abs_lon_start, digits=digits),
			this$EastWest)
			
		# Write to the .asc file:
	    write.table(out, file=filename, row.names=FALSE, col.names=FALSE, sep=",", quote=FALSE, ...)
	}
	
	# Split into strata (if byStratum==TRUE), and set the files names as names of the list:
	filenames <- getTransectFileName(x=x, projectName=projectName, text="", ext="asc", dir=dir, byStratum=byStratum)
	
	# Add columns NorthSouth and EastWest, and abs_lat_start and abs_lon_start to the transects, for writing:
	x$transects$NorthSouth <- c("N", "S")[as.numeric(x$transects$lat_start > 0) + 1]
	x$transects$EastWest <- c("E", "W")[as.numeric(x$transects$lon_start > 0) + 1]
	x$transects$abs_lat_start <- x$transects$lat_start
	x$transects$abs_lon_start <- x$transects$lon_start
	
	x$transects <- split(x$transects, if(byStratum) x$transects$Stratum else 1)
	names(x$transects) <- filenames
	# Write the files:
	lapply(seq_along(x$transects), writeTransectsToMAxSea_OneStratum, x=x, item.type=item.type, item.id=item.id, item.col=item.col, north=north, east=east, digits=digits, ...)
	
	# Return the file names
	filenames
}
#'
#' @export
#' @rdname writeTransects
#' 
writeTransectsINFO <- function(x, projectName, dir=NULL, digits=2, byStratum=TRUE){
	
	# Function for writing one stratum:
	writeTransectsINFO_OneStratum <- function(stratumInd, x, digits=2){
		# Extract the file name from the names of the input:
		filename <- names(x$transects)[stratumInd]
		
		# Get the total travelled distance and the survey coverage:
		dist <- x$totalSailedDist$total[stratumInd]
	    area.nm2 <- x$area[stratumInd]
	    Sur.cov <- dist / sqrt(area.nm2)
		
		# Select the current stratum:
		this <- x$transects[[stratumInd]]
		LatDeg <- floor(this$lat_start)
		LonDeg <- floor(this$lon_start)
		LatMin <- round(60*(this$lat_start  - LatDeg), digits=digits)
		LonMin <- round(60*(this$lon_start - LonDeg), digits=digits)
		lonlat <- data.frame(LatDeg=LatDeg, LatMin=LatMin, LonDeg=LonDeg, LonMin=LonMin)
		
		# wWrite to the file:
	    capture.output( cat("\n", format(c(date()), width=20, justify = "left")), file=filename)
	    capture.output( cat("\n", "Stratum (Cruise region) ", format(x$strata[stratumInd], width=7, justify="right")), file=filename, append=TRUE)
	    capture.output( cat("\n", "Speed and time available", format(c(paste(x$parameters$knots[stratumInd], "knots"), paste(x$parameters$hours[stratumInd], "h")), width=7, justify="right")), file=filename, append=TRUE)
	    capture.output( cat("\n", "Stratum area (n.mi2)    ", format(area.nm2, width=7, justify="right")), file=filename, append=TRUE)
	    capture.output( cat("\n", "Sailing distance (n.mi) ", format(dist, width=7, justify="right")), file=filename, append=TRUE)
	    capture.output( cat("\n", "Survey coverage         ", format(Sur.cov, width=7, justify="right")), file=filename, append=TRUE)
	    capture.output( cat("\n", " "), file=filename, append=TRUE)
	    capture.output( cat("\n", "Transect positions      "), file=filename, append=TRUE)
	    capture.output( cat("\n", " "), file=filename, append=TRUE)
	    capture.output( cat("\n", " "), lonlat, file=filename, append=TRUE) 
	}
	
	# Split into strata (if byStratum==TRUE), and set the files names as names of the list:
	filenames <- getTransectFileName(x=x, projectName=projectName, text="INFO", ext="txt", dir=dir, byStratum=byStratum)
	
	x$transects <- split(x$transects, if(byStratum) x$transects$Stratum else 1)
	names(x$transects) <- filenames
	# Write the files:
	lapply(seq_along(x$transects), writeTransectsINFO_OneStratum, x=x, digits=digits)
	
	# Return the file names
	filenames
}
#'
#' @export
#' @rdname writeTransects
#' 
writeTransectsTRACK <- function(x, projectName, dir=NULL, digits=5, byStratum=TRUE, ...){
	
	# Function for writing one stratum:
	writeTransectsTRACK_OneStratum <- function(stratumInd, x, ...){
		# Extract the file name from the names of the input:
		filename <- names(x$transects)[stratumInd]
		
		# Select the current stratum:
		this <- x$transects[[stratumInd]]
		
		out <- data.frame(Line=1, Longitude=round(this$lon_start, digits=digits), Latitude=round(this$lat_start, digits=digits))
		write.csv(out, file=filename, row.names=FALSE, ...)
	}
	
	# Split into strata (if byStratum==TRUE), and set the files names as names of the list:
	filenames <- getTransectFileName(x=x, projectName=projectName, text="TRACK", ext="txt", dir=dir, byStratum=byStratum)
	
	x$transects <- split(x$transects, if(byStratum) x$transects$Stratum else 1)
	names(x$transects) <- filenames
	# Write the files:
	lapply(seq_along(x$transects), writeTransectsTRACK_OneStratum, x=x, ...)
	
	# Return the file names
	filenames
}
#'
#' @export
#' @importFrom data.table fwrite
#' @rdname writeTransects
#' 
writeTransectsGPX <- function(x, projectName, dir=NULL, digits=5, byStratum=TRUE){
	
	writeTransectsGPX__OneStratum <- function(stratumInd, x){
		# Extract the file name from the names of the input:
		filename <- names(x$transects)[stratumInd]
		
		# Select the current stratum:
		this <- x$transects[[stratumInd]]
		
		out <- data.frame(wp=seq_len(nrow(this)), Long=round(this$lon_start, digits=digits), Lat=round(this$lat_start, digits=digits))
		writeGPX(out, file=filename)
	}

	# Split into strata (if byStratum==TRUE), and set the files names as names of the list:
	filenames <- getTransectFileName(x=x, projectName=projectName, text="", ext="gpx", dir=dir, byStratum=byStratum)
	
	x$transects <- split(x$transects, if(byStratum) x$transects$Stratum else 1)
	names(x$transects) <- filenames
	# Write the files:
	lapply(seq_along(x$transects), writeTransectsGPX__OneStratum, x=x)
	
	# Return the file names
	filenames
}
#'
#' @export
#' @importFrom data.table fwrite
#' @rdname writeTransects
#' 
writeTransects <- function(x, projectName, dir=NULL, digits=5, byStratum=TRUE, cols=NULL, keepTransport=TRUE, text="", ext="txt", ...){
	
	# Function for writing one stratum:
	writeTransects_OneStratum <- function(stratumInd, x, cols, keepTransport, ext, units, ...){
		# Extract the file name from the names of the input:
		filename <- names(x$transects)[stratumInd]
		# Select the current stratum:
		this <- x$transects[[stratumInd]]
		
		# Select the appropriate columns, and only the transect rows (not transport):
		if(!keepTransport){
			this <- this[this$Transport==0, , drop=FALSE]
		}
		this <- this[, cols, drop=FALSE]
		numericCols <- sapply(this, is.numeric)
		this[,numericCols] <- round(this[,numericCols], digits=digits)
		
		# Write the data:
		if(ext=="nc"){
			library(ncdf4)
			# Define the variables with Length and dimension state:
			L <- nrow(this)
			dimState <- ncdim_def(name="Row", units="count", vals=seq_len(L))
			ncvars <- lapply(seq_along(cols), function(i) ncvar_def(cols[i], units=units[i], dim=dimState))
	      	# Create the NetCDF file:
			ncnew <- nc_create(filename, ncvars)
			# Write the variables to the file:
			lapply(seq_along(cols), function(i) ncvar_put(nc=ncnew, varid=cols[i], vals=this[[cols[i]]], start=1, count=L))
			# Close the file:
			nc_close(ncnew)
		}
		else{
			data.table::fwrite(this, filename, ...)
		}
	}
	
	# Define units of all present columns_
	allcols <- c(
		"Stratum", "Transect", "Segment", "Transport", "Retour", 
		"lon_start", "lon_mid", "lon_stop", "lat_start", "lat_mid", "lat_stop", 
		"x_start", "y_start", "x_mid", "y_mid", "x_stop", "y_stop", 
		"time_start", "time_stop", "time_mid", 
		"dist_start", "dist_stop", "dist_mid", 
		"segmentLengths")
	allunits <- c(
		"String", "Integer", "Integer", "Integer", "Logical", 
		rep("Decimal degrees", 6), 
		rep("Nautical miles", 6), 
		rep("UNIX time", 3), 
		rep("Nautical miles", 3), 
		"Nautical miles")
	
	# Define the columns to keep:
	if(!length(cols)){
		cols <- c(
			"Stratum", "Transect", "Segment", "Transport", "Retour", 
			"lon_start", "lon_stop", "lat_start", "lat_stop", 
			"time_start", "time_stop", 
			"dist_start", "dist_stop", 
			"segmentLengths"
		)
	}
	else if(tolower(cols)=="all"){
		cols <- colnames(x$transects)
	}
	if(!keepTransport){
		cols <- cols[cols != "Transport"]
	}
	
	units <- allunits[match(allcols, cols)]
				 
	# Split into strata (if byStratum==TRUE), and set the files names as names of the list:
	filenames <- getTransectFileName(x=x, projectName=projectName, text=text, ext=ext, dir=dir, byStratum=byStratum)
	
	x$transects <- split(x$transects, if(byStratum) x$transects$Stratum else 1)
	names(x$transects) <- filenames
	# Write the files:
	lapply(seq_along(x$transects), writeTransects_OneStratum, x=x, cols=cols, keepTransport=keepTransport, ext=ext, units=units, ...)
	
	# Return the file names
	filenames
}
#'
#' @export
#' @rdname writeTransects
#' 
getTransectFileName <- function(x, projectName, text="", ext="txt", dir=NULL, byStratum=TRUE){
	# If 'dir' is not given, use the report dir of the project:
	if(length(dir)==0){
		dir <- getProjectPaths(projectName)$RReportDir
	}
	# Use the first type if byStratum==FALSE:
	if(byStratum){
		type <- unique(x$parameters$type)
	}
	else{
		type <- unique(x$parameters$type)
		if(length(type)>1){
			warning("File name only reflects the transect type of the first stratum")
		}
		type <- type[1]
	}
	# Set the path(s) containing transect type and possibly stratum:
	Transects <- paste0("Transects_", type)
	Stratum <- paste0("_Stratum_", unique(x$strata))
	if(nchar(text)){
		text <- paste0("_", text)
	}
	file.path(dir, paste0(Transects, if(byStratum) Stratum, text, ".", ext))
}
