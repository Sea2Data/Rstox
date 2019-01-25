#*********************************************
#*********************************************
#' Get polygon area and convert to or from geographic and Cartesian coordinates.
#'
#' \code{polyArea} calculates the area in nautical mile squared of a multipolygon (for one stratum). \cr \cr
#' \code{geo2xy} converts from geographic to or from Cartesian coordinates or the inverse. \cr \cr
#' \code{getMatrixList} converts the input to a list of matrices. \cr \cr
#' \code{getMultipolygon} converts the input to a multipolygon wkt string. \cr \cr
#' \code{getSpatial} converts the input to a Spatial object. \cr \cr
#' \code{matrix2multipolygon} identical to \code{getMultipolygon}. \cr \cr
#' \code{multipolygon2matrix} identical to \code{getMatrixList}. \cr \cr
#' 
#' @param x					Either a two column matrix of x and y coordinates, indicating only one polygon, or a list of such matrices, indicating several polygons. If a list of lists of two column matrices are given, the first matrix of each list is the polygon, and the following are subtractions. \code{wkt} cal also be a wkt string such as "MULTIPOLYGON(((4 55, 15 56, 15 59, 4 59, 4 55)))".
#' @param requireClosed		Logical: If TRUE (default) require polygons to be closed in the sense that the last point should equal the first. Set this to FALSE to allow adding the first point as the last point.
#' @param par				A list of proj4 parameters.
#' @param ...				Further proj4 parameters overriding those in \code{par}.
#' @param longlat			Logical: If TRUE (default) the input to polyArea_test is longitude, latitude.
#' @param inv				Logical: If TRUE, do the inverse conversion in rgdal::project().
#' @param data.frame.out	Logical: If TRUE convert the matrices to data frames with columns x and y.
#' @param drop				Logical: If TRUE drop the list if only one multipolygon or only one polygon is given.
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
#' @importFrom rgeos readWKT
#' @rdname polyArea
#' 
readWKTSplit <- function(x, ...){
	x <- paste(strwrap(x, ...), collapse='\r')
	rgeos::readWKT(x)
}
#' 
#' @export
#' @importFrom rgeos gArea
#' @importFrom sp CRS spTransform proj4string
#' @rdname polyArea
#' 
polyArea <- function(x, requireClosed=TRUE) {
	# We need rgdal when AreaMethod=Acurate in StratumArea!!!!
	###if(is.numeric(x)){
	###	x <- paste0("MULTIPOLYGON(((", paste(apply(x, 1, paste, collapse=" "), collapse=", "), ")))")
	###}
	#write(x, "~/Desktop/Aktuelt/test1.txt")
	x <- matrix2multipolygon(x, requireClosed=requireClosed)
	#write(x, "~/Desktop/Aktuelt/test3.txt")
	
	p <- readWKTSplit(x)
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
#' @importFrom rgeos gArea
#' @rdname polyArea
#' 
polyArea_test <- function(x, par=list(proj="laea", units="kmi", lon_0=NA, lat_0=NA, x_0=0, y_0=0, ellps="WGS84", datum="WGS84"), longlat=TRUE, ...) {
	# Convert to xy if given in longlat:
	if(longlat){
		x <- geo2xy(x, par=par, inv=FALSE, ...)
	}
	# Get the spatial object and use gArea() to get the area:
	out <- getSpatial(x)
	out <- sum(rgeos::gArea(out, byid=T))
	return(out)
}
#' 
#' @export
#' @keywords internal
#' @rdname polyArea
#' 
geo2xy <- function(x, par=list(proj="aeqd", units="kmi", lon_0=NA, lat_0=NA, x_0=0, y_0=0, ellps="WGS84", datum="WGS84"), inv=FALSE, data.frame.out=FALSE, ...){
	# Get projection string:
	par <- getProjString(par=par, ..., x=x, requireClosed=FALSE)
	
	# Convert to a list of matrices and run the project() function on all elements:
	#out <- x[ ,c("x", "y"), drop=FALSE]
	out <- getMatrixList(x, data.frame.out=data.frame.out)
	#x <- rapply(x, rgdal::project, proj=par, inv=inv, how="replace")
	out <- projectMatrixList(out, par=par, inv=inv, data.frame.out=data.frame.out)
	#if(!inv){
		colnames(out) <- if(inv) c("longitude", "latitude") else c("x", "y")
		#}
	# Add the other columns:
	#if(ncol(x)>2){
	#	out <- cbind(out, x[, -(1:2)])
	#}
	#if(add){
	#	out <- cbind(out, x)
	#}
	
	attr(out, "proj") <- par
	out
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
getMultipolygon <- function(x, drop=TRUE, data.frame.out=FALSE, requireClosed=TRUE){
	if(isSpatial(x)){
		x <- spatial2matrixList(x, drop=drop, data.frame.out=data.frame.out)
		x <- matrixList2multipolygon(x, requireClosed=requireClosed)
	}
	else if(isMatrixList(x)){
		x <- matrixList2multipolygon(x, requireClosed=requireClosed)
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
		#x <- readWKTSplit(x)
		if(matrixListLevel(x)==1){
			x <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(x)), ID=1)))
		}
		else{
			x <- lapply(seq_along(x), function(i) sp::Polygons(list(sp::Polygon(x[[i]])), ID=i))
			x <- sp::SpatialPolygons(x)
		}
	}
	else if(isMultipolygon(x)){
		x <- readWKTSplit(x)
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
#' @param par				A list of proj4 parameters.
#' @param ...				Further proj4 parameters overriding those in \code{par}.
#' @param x					One of three onjects depending on the funciton: (1) a two column matrix of x and y coordinates, indicating only one polygon, or a list (of lists) of such matrices, indicating several polygons in a multipolygon. If a list of lists of two column matrices are given, the first matrix of each list is the polygon, and the following are subtractions. (2) A wkt string such as "MULTIPOLYGON(((4 55, 15 56, 15 59, 4 59, 4 55)))". (3) A spatial object.
#' @param list.out			Logical: If TRUE the projection info is returned as a list instead of a concatenate string.
#' @param requireClosed		Logical: If TRUE (default) require polygons to be closed in the sense that the last point should equal the first. Set this to FALSE to allow adding the first point as the last point.
#' @param inv				Logical: If TRUE, do the inverse conversion in rgdal::project().
#' @param data.frame.out	Logical: If TRUE convert the matrices to data frames with columns x and y.
#' @param drop				Logical: If TRUE drop the list if only one multipolygon or only one polygon is given.
#' @param FUN				The funciton to apply to the elements in \code{rapplyKeepDataFrames}.
#'
#' @export
#' @importFrom rgeos readWKT
#' @keywords internal
#' @rdname getProjString
#' 
getProjString <- function(par=list(proj="laea", units="kmi", lon_0=NA, lat_0=NA, x_0=0, y_0=0, ellps="WGS84", datum="WGS84"), ..., x=NULL, list.out=FALSE, requireClosed=TRUE){
	
	# NOTE: There is a difference between how rgeos::gCentroid calculates the centroid for a closed polyogn and a line string (in which case the center of mass of the mere points is calculated, as per the reply to the issue on this page: https://stackoverflow.com/questions/35720614/gcentroid-rgeos-r-vs-actual-centroid-in-python)
	
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
		p <- getMultipolygon(x, requireClosed=requireClosed)
		#if(is.list(p)){
		#	p <- getCoordsPolygon(p)
		#}
		p <- readWKTSplit(p)
		#p <- lapply(p, readWKTSplit)
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
		out <- lapply(y@Polygons, methods::slot, "coords")
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
		out <- lapply(y@Lines, methods::slot, "coords")
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
	if("polygons" %in% methods::slotNames(x)){
		out <- sp::disaggregate(x)
		out <- lapply(out@polygons, getCoordsMultipolygon, data.frame.out=data.frame.out)
	}
	else if("coords" %in% methods::slotNames(x)){
		out <- getCoordsSpatialPoints(x, data.frame.out=data.frame.out)
	}
	else if("lines" %in% methods::slotNames(x)){
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
matrixList2multipolygon <- function(x, requireClosed=TRUE){
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
	
	# Try to append the start point as end point in the polygon if requireClosed=FALSE:
	#write(x, "test2.txt")
	#if(requireClosed){
	if(!requireClosed){
		x <- rbind(x, utils::head(x, 1))
		isConnected <- TRUE
	}
	if(!isConnected){
		warning("Points do not form a closed polygon. Use requireClosed=FALSE to add the first point as the last point.")
	}
	
	# Convert to text string of pairs:
	x <- rapplyKeepDataFrames(x, mergeToPairs)
	
	x <- lapply(x, addParantheseis)
	x <- addParantheseis(x)
	x <- paste0("MULTIPOLYGON", x)
	write(x, "test.txt")
	
	### if(isConnected){
	### 	x <- lapply(x, addParantheseis)
	### 	x <- addParantheseis(x)
	### 	x <- paste0("MULTIPOLYGON", x)
	### 	write(x, "test.txt")
	### }
	### else{
	### 	warning("Points do not form a closed polygon. LINESTRING returned instead of MULTIPOLYGON. Use requireClosed=FALSE to add the first point as the last point.")
	### 	x <- paste0("LINESTRING", x)
	### }
	return(x)
}
#'
#' @export
#' @importFrom rgeos readWKT
#' @keywords internal
#' @rdname getProjString
#' 
multipolygon2spatial <- function(x){
	readWKTSplit(x)
}
#'
#' @export
#' @keywords internal
#' @rdname getProjString
#' 
isSpatial <- function(x){
	isS4(x) && any(c("lines", "coords", "polygons") %in% methods::slotNames(x))
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
#' Plan an acoustic-trawl survey.
#'
#' \code{surveyPlanner} reads a WKT (Well known text) file, shapefiles, or a StoX project and generates a survey plan of parallel or zig zag transects.  \cr \cr
#' \code{plotStratum} plots the survey plan returned by \code{surveyPlanner} . \cr \cr
#' \code{readStrataPolygons} reads a WKT (Well known text) file, shapefiles, or a StoX project and returns the strata system.  \cr \cr
#'
#' @param projectName   	The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}). Can also be the path to a shapefile or folder of shape files, or to a file with polygons given as a two column matrix with stratum name in the first column and a MULTIPOLYGON wkt string in the second conlumn.
#' @param parameters		Optional named list of parameters overriding the inputs "type", "bearing", "retour", "hours", "knots", "nmi", "seed" or "t0". This list is returned from \code{surveyPlanner} (located in the element $parameters), and can be useful when changing say the 'nmi' in the last strata given time lost due to weather in the first strata. See examples.
#' @param type				The type of the transects, repeated to the number of stratums. See details for possible values. Case insensitive.
#' @param bearing			The survey bearing of each transect, given in one of 4 ways: (1) by codes "N", "NW", "W", "WS", "S", "SE", "E", "NE", (2) as angles counter clockwise from EAST in degrees, (3) as a string "along" or "across" the stratum orientation as obtained by the principal components of the stratum borders after populating with 1000 points with nearly equal separation along the border in geographic corrdinates, or (4) as a data frame with columns "lon_start", "lon_stop", "lat_start", "lat_stop" defining the angles between a start and end point for each stratum.
#' @param rev				Logical: If TRUE, the bearing calculated by the funciton when bearing = "along" or "across" is reversed, i.e., going from East to West instead of West to East.
#' @param retour			Logical: If TRUE, continue with the transects back to the start point of the stratum.
#' @param toursFirst		Logical: If TRUE, do all tours first followed by the retours (only effective if retour is TRUE).
#' @param hours,nmi			The time/traveled distance to spend in each stratum, given in hours/nautical miles (repeated to the number of strata), where \code{nmi} has precedence over \code{hours}. If given as a list of one element, these are interpreted as the hours/nmi for the entire survey, in which case selecting only a subset of the strata using \code{strata} will increase the effort in each of the remaining strata.
#' @param t0				The start time of the survey, set to Sys.time() formatted to UTC by default. A as.POSIXct object or a string of a recognisable form (such as "\%Y-\%m-\%d \%H:\%M:\%S") are supported.
#' @param knots				The speed to use in the stratum, given in knots.
#' @param seed				The seed(s) to use for the random transects. If not given as an integer vector (such as c(2L, 5L)) of length equal to the number of strata, random seed are drawn using \code{\link{getSeedV}}.
#' @param angsep			The separation in degrees of points populating the stratum polygons (geographical coordinates longitude, latitude). Low values preserve the stratum borders when converting to Cartesian coordinates. 
#' @param distsep			The separation in nautical miles of points populating the transects in Cartesian coordinates before converting back to geographical coordinates. When given, this parameter preserves the shortest travel distance (great-circle distances) of the transects (inducing curved lines in geographical coordinates). 
#' @param margin			The margin to accept for deviation between input and output total travelled distance. Setting this may result in unequal coverage, since forcing the total traveled distance towards a fixed value may force specific paths due to the shape of the stratum. Rather, increase the input \code{hours}, og accept the "noise" in the output total traveled distance. For \code{plotStratum} margin is the margin around the borders of the survey area in units of its dimensions.
#' @param equalEffort		Logical: If TRUE, assign effort proportional to the area of each stratum.
#' @param byStratum			Logical: If FALSE, include the transport between strata in the total traveled distance given in $Survey of the output.
#' @param strata			Specification of the strata to include, either given as the string "all", implying all strata to be used; an index vector as given to "[]" (negative values may be used to exclude specific strata), or a character vector of the strata names. Note that if the strata are named by integers, these names must be given as character and not integers, which would act as indices.
#' @param cruise			A string or numeric giving the cruise name/code.
#' @param keepTransport		Logical: If FALSE, remove transport stretches in the Transect output.
#' @param x					The output from \code{\link{surveyPlanner}}.
#' @param transect			Logical: If TRUE, plot the transects.
#' @param centroid			Logical: If TRUE, plot the centroid of the strata.
#' @param transport_alpha	The transparency to use for the transport paths.
#' @param zoom				The zoom relative to the extent of the range of the transect positions. Default is 1.5.
#' @param aspectratio		The aspect ratio of the plot, adjusted automatically by default.
#' @param xlab,ylab			The x and y label for the plot.
#' @param xlim,ylim			The x and y limit of the plot.
#' @param keep0effort		Logical: If FALSE, keep only the strata with positive effort.
#' @param ...				Used for flexibility, and including shapenames: A list of length 3 giving the names of the longitude, latitude and stratum column in the shape files.
#' 
#' @details 
#' The \code{surveyPlanner} function generates the survey plan (transect lines) in a cartesian coordinate system, and transforms the positions to the geographical coordinate system (longitude, latitude) using the azimuthal equal distance projection, which ensures that distances, and also equal coverage, if the method used is designed with this prerequisite, are preserved in the transformation. 
#' Strindberg and Buckland (2004): Strindberg, S., & Buckland, S. T. (2004). Zigzag survey designs in line transect sampling. Journal of Agricultural, Biological, and Environmental Statistics, 9(4), 443
#' The following types are implemented:
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
#'	\item{"lonlat"}{"The input stratum polygons (longitude, latitude) in a list with one matrix per stratum"}
#'	\item{"parameters"}{"A list of parameters for each stratum: type, bearing, retour, hours, knots, nmi, seed"}
#'	\item{"proj"}{"The projection string used to convert from (longitude, latitude) to (x, y)"}
#'	\item{"area"}{"The area og each polygon in square nautical miles"}
#'	\item{"centroid"}{"The centroid of the strata, used in the projection"}
#'	\item{"strata"}{"The stratum names"}
#' }
#'
#' @return \code{polyArea} returns area in nmi squared, and \code{matrix2multipolygon} returns a MULTIPOLYGON wkt.
#'
#' @examples
#' # Generate parallel transects with equal effort in each stratum (proportional to area):
#' projectName <- "Test_Rstox"
#' system.time(Parallel <- surveyPlanner(projectName=projectName, type="Parallel", bearing="N",
#'     nmi=2000, knots=10, seed=0, equalEffort=TRUE))
#' dev.new()
#' p <- plotStratum(Parallel)
#'
#' # Running surveyPlanner() on a project takes some time, since the project has to be opened and
#' # the process data read to obtain the strata. A quicker way is to specify the strata definitions 
#' # directly, either as the path to a file or as a folder or vector of paths to shapefiles:
#' # This produces a different strata system than that in the Test_Rstox project:
#' system.time(Parallel <- surveyPlanner(projectName=file.path(dirname(getProjectPaths()$projectRoot),
#'      "reference", "stratum", "norwegian_sea2014.txt"), type="Parallel", bearing="N",
#'     nmi=2000, knots=10, seed=0, equalEffort=TRUE))
#' dev.new()
#' p <- plotStratum(Parallel)
#'
#' # Write the transects to one csv file (to write one file per stratum use byStratum=TRUE). 
#' # Use ext="nc" to write to a NetCDF4 file. Use 'dir' to specify the directory in which to place 
#' # the file(s) (otherwise the files are placed in the R Report directory of the project):
#' filename <- writeTransects(Parallel, projectName=projectName, byStratum=FALSE)
#' filename
#'
#' # Zigzag with equal coverage probability:
#' system.time(Zigzag <- surveyPlanner(projectName=projectName, type="RectEnclZZ", bearing="N",
#'     nmi=2000, knots=10, seed=0, equalEffort=TRUE))
#' dev.new()
#' p <- plotStratum(Zigzag)
#' 
#' # The parallel survey use 23.39 percent of sailing distance for transport between transects, 
#' # whereas the zigzag survey use 10.76 percent.
#' Parallel$Survey
#' Zigzag$Survey
#' 
#' # Reduce effort in the last three strata, e.g. due to lost time due to bad weather in stratum 1:
#' # Note that the parameter 'nmi' must be NULL (default) to effectively change the 'hours', since
#' # 'nmi', when specified, har precedence over 'hours'. 
#' # Also, the parameter 'equalEffort' must be FALSE (default) since TRUE implies recalculating
#' # the distance to traveled in each stratum so that this distance is proportional to stratum area:
#' hours <- Parallel$parameters$hours
#' hours[2:4] <- hours[2:4] - 50
#' system.time(Parallel2 <- surveyPlanner(projectName=projectName, type="Parallel", bearing="N",
#'     knots=10, seed=0, hours=hours))
#' dev.new()
#' p <- plotStratum(Parallel2)
#' 
#' # Tour-retour zigzag in stratum 1 and 3:
#' system.time(Zigzag2 <- surveyPlanner(projectName=projectName, type="RectEnclZZ", bearing="N",
#'     nmi=2000, knots=10, seed=0, equalEffort=TRUE, retour=c(TRUE, FALSE, TRUE, FALSE)))
#' dev.new()
#' p <- plotStratum(Zigzag2)
#' 
#' # Populate the transects with points separated by 10 nmi specified by the 'distsep' parameter, 
#' # for visualizing the true track along the great-circle:
#' system.time(Zigzag3 <- surveyPlanner(projectName=projectName, type="RectEnclZZ", bearing="N",
#'     nmi=2000, knots=10, seed=0, equalEffort=TRUE, retour=c(TRUE, FALSE, TRUE, FALSE), distsep=10))
#' dev.new()
#' p <- plotStratum(Zigzag3)
#' 
#' # Write the transects to a NetCDF4 file in the output folder of the project (alternatively specify
#' # the directory using 'dir', or the filenames using 'filenames'. Also use 'prefix' or 'suffix' 
#' # to add info to the file names):
#' writeTransects(Zigzag3, projectName, ext="nc")
#'
#' @seealso
#'
#'	See ?writeTransects for writing the transects to verious files.
#'
#' @export
#' @importFrom sp Lines Line SpatialLines
#' @importFrom rgeos gIntersection
#' @import data.table
#' @importFrom utils head tail
#' @rdname surveyPlanner
#' 
surveyPlanner <- function(projectName, parameters=NULL, type="Parallel", bearing="N", rev=FALSE, retour=FALSE, toursFirst=FALSE, hours=240, nmi=NULL, t0=NULL, knots=10, seed=0, angsep=1/6, distsep=NULL, margin=NULL, equalEffort=FALSE, byStratum=TRUE, strata="all", cruise="surveyPlanner", keepTransport=TRUE, centroid=NULL, ...){
	
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
				angles <- bearing * pi/180
			}
			else{
				angles <- bearing
			}
		}
		#if(rev){
		#	angles <- angles + pi
		#}
		rev <- rep(as.numeric(rev), length.out=length(angles))
		angles <- angles + rev * pi
		
		angles
	}
	
	# Order tours first and then retours reversed, i.e., forth and back along the entire survey instead of in each stratum:
	orderTourRetour <- function(x){
		x <- split(x, interaction(x$stratum, x$retour))
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
			#if(length(intersectsFirst)<3){
			if(length(intersectsFirst)<1){
				intersects@coords <- intersects@coords[FALSE,]
				return(intersects)
			}
			# Pick out the point farthes from the intersection point. This is done since the transects are regenerated later to incorporate any partial transects due to holes or bows in the polygon:
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
		
			coords$log_start <- trimws(format(round(coords$dist_start, digits=1), nsmall=1))
			coords$log_stop  <- trimws(format(round(coords$dist_stop, digits=1), nsmall=1))
			coords$log_mid   <- trimws(format(round(coords$dist_mid, digits=1), nsmall=1))
			
			# Add the survey start time:
			t0 <- unclass(as.POSIXct(t0, tz="UTC"))
			coords$time_start <- insertIfMissing(coords$time_start, as.POSIXct(coords$dist_start / coords$knots * 3600 + t0, origin="1970-01-01", tz="UTC"))
			coords$time_stop  <- insertIfMissing(coords$time_stop,  as.POSIXct(coords$dist_stop / coords$knots * 3600 + t0, origin="1970-01-01", tz="UTC"))
			coords$time_mid   <- insertIfMissing(coords$time_mid,   as.POSIXct(coords$dist_mid / coords$knots * 3600 + t0, origin="1970-01-01", tz="UTC"))
		
			coords$segmentLengths <- insertIfMissing(coords$segmentLengths, c(segmentLengths, NA))
		
			coords <- coords[setdiff(names(coords), c("x", "y"))]
			coords
		}
		
		# Split into strata if byStratum:
		x <- split(x, if(byStratum) x$stratum else 1)
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
		out <- splitByThreeCols(x, c("stratum", "transect", "segment"), prefix=TRUE)
		out <- Stratum2ListOfSpatialLines(out, coordNames=coordNames)
		out
	}
	#getCentroid <- function(x, proj="aeqd", lonlatnames=c("longitude", "latitude")){
	#	projList <- getProjString(proj=proj, x=x[, lonlatnames], list.out=TRUE, requireClosed=FALSE)
	#	centroid <- data.frame(lon_centroid=projList$lon_0, lat_centroid=projList$lat_0)
	#	centroid
	#}
	
	
	
	
	# Function used for linking strata, by incerting the start positions of the strata to the end position of the previous:
	linkStrata <- function(x){
		etEnd <- which(is.na(x$x_mid))
		stratumOrderStart <- etEnd[-length(etEnd)]
		atCols <- c("x_start", "y_start")
		replaceCols <- c("x_stop", "y_stop")
		x[stratumOrderStart, replaceCols] <- x[stratumOrderStart + 1, atCols]
	
		x
	}
	
	# Function for extracting the total sailed distance:
	getSailedDist <- function(x, nmi=NULL, area=NULL, byStratum=TRUE, nmi.out=TRUE){
		getSailedDist_OneStratum <- function(x){
			data.frame(
				transect = sum(x$segmentLengths[x$transport==0], na.rm=TRUE), 
				transport = sum(x$segmentLengths[x$transport==1], na.rm=TRUE), 
				total = sum(x$segmentLengths, na.rm=TRUE))
		}
		
		# If a data frame, split into strata (if byStratum):
		if(is.data.frame(x)){
			x <- split(x, if(byStratum) x$stratum else 1)
		}
		# Otherwise merge to one stratum if !byStratum:
		else if(length(x)>1 && !byStratum){
			x <- list(as.data.frame(data.table::rbindlist(x)))
		}
		
		# Get sailed distances from each stratum
		out <- lapply(x, getSailedDist_OneStratum)
		# WARNING: THIS REMOVED THE STRATA NAMES:
		# out <- as.data.frame(data.table::rbindlist(out))
		out <- do.call(rbind, out)
	
		# Add in percent if reference sailed distance nmi is given:
		if(length(nmi)){
			if(nmi.out){
				out <- cbind(out, nmi=nmi)
			}
			#else{
			#	out <- cbind(out)
			#}
			#outPercent <- round(out / nmi * 100, digits=2) # A bug, leading to total percent different from 100.
			outPercent <- round(out / out$total * 100, digits=2)
			colnames(outPercent) <- paste0(colnames(outPercent), "_percent")
			out <- cbind(out, outPercent)
		}
		if(length(area)){
			# Trick:
			out <- data.frameOrderByName(
				list(
					area=area, 
					coverage=area, 
					out
				), stringsAsFactors=FALSE
			)
			
			
			out$coverage <- out$transect / sqrt(out$area)
			
			
			
			#out <- cbind(area=area, coverage=out$transect / sqrt(area), out)
		}
	
		# THIS ADDED WRONG STRATA NAMES:
		#rownames(out) <- if(nrow(out)==1) "Survey" else paste0("Stratum ", rownames(out))
		if(nrow(out)==1){
			rownames(out) <- "Survey"
		}
	
		out
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
		if(tolower(type) == "parallel" && retour){
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
		if(tolower(type) == tolower("RectEnclZZ")){
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
		if(tolower(type) == tolower("EqSpZZ")){
			intersectsCoordsList <- parallel2zigzag(intersectsCoordsList)
			# Add end points to the zigzag transects. For this we need a spatial object:
			temp <- intersects
			# Use the idcol="transect" to split the transects afterwards:
			temp@coords <- data.matrix(data.table::rbindlist(intersectsCoordsList, idcol=FALSE))
			temp <- addEndTransects(temp, spatialLinesPolygon)
			# Split the transects into a list again:
			intersectsCoordsList <- coords2coordsList(temp@coords)
			# remove the last value since the last element of intersectsCoordsList was removed in the 
			#ind_xGrid <- ind_xGrid[-length(ind_xGrid)]
			####### xGrid_EqSpZZ <- sapply(sapply(intersectsCoordsList, "[", 1, 1), function(x) which.min(abs(x-xGrid)))
			#xGrid_EqSpZZ <- findInterval(sapply(intersectsCoordsList, "[", 1, 1), xGrid)
		}
		
		# The transects may intersect with the stratum polygon borders more than once, so we need to intersect again and split transects into subtransects when intersecting more than twice (two intersectiins at the borders):
		#intersectsCoordsList <- lapply(intersectsCoordsList, function(x) )
		
		intersectsCoordsList <- lapply(intersectsCoordsList, expand_transect_ends, fact=1.01)
		
		spatialLinesTransects <- lapply(intersectsCoordsList, list)
		spatialLinesTransects <- Transect2SpatialLines(spatialLinesTransects, prefix="")
		
		
		intersects <- rgeos::gIntersection(spatialLinesPolygon, spatialLinesTransects, byid=TRUE)
		intersectsCoordsList <- getIntersectsCoordsList(intersects)
		
		### Assure that the new intersects are between the relevant grid lines (why this? are the intersections not valid?):
		###if(tolower(type) == tolower("EqSpZZ")){
		###	selectInsidexGrid <- function(ind, xGrid, margin=0.01){
		###		bin <- xGrid_EqSpZZ[ind]
		###		width <- xGrid[bin + 1] - xGrid[bin]
		###		valid <- xGrid[bin] - margin * width <= intersectsCoordsList[[ind]][,1] & intersectsCoordsList[[ind]][,1] <= xGrid[bin + 1] + margin * width
		###		intersectsCoordsList[[ind]][valid, , drop=FALSE]
		###	}
		###	intersectsCoordsList <- lapply(seq_along(intersectsCoordsList), selectInsidexGrid, xGrid=xGrid)
		###}
		
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
				intersectsCoordsList[[i]] <- cbind(intersectsCoordsList[[i]], transect=i)
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
		intersectsCoordsList <- lapply(intersectsCoordsList, cbind, retour=as.numeric(retour))
		
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
	getTransectsByArea <- function(nmi_rest, area, fac, corners, xyRotated, type="Parallel", bearing="N", knots=10, seed=0, retour=FALSE){
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
		
		# If 'distsep' is given as a two element vector, the second element indicates populating the transects with points of distance dt[2]:
		if(length(distsep)){
			for(i in seq_along(intersectsCoordsList)){
				intersectsCoordsList[[i]] <- populatePath(intersectsCoordsList[[i]], dt=distsep, addInfo=FALSE)
			}
		}
		
		# Add transect and transport columns:
		for(i in seq_along(intersectsCoordsList)){
			# Add also whether the segments are transports (the last line is transport):
			transport <- c(double(nrow(intersectsCoordsList[[i]]) - 1), 1)
			segment <- seq_len(nrow(intersectsCoordsList[[i]]))
			#intersectsCoordsList[[i]] <- cbind(intersectsCoordsList[[i]], segment=segment, transect=i, transport=transport)
			intersectsCoordsList[[i]] <- cbind(intersectsCoordsList[[i]], segment=segment, transport=transport, knots=knots)
		}
		
		### # Add a column to each transect denoting start and end points
		# Combine to a matrix:
		coords <- as.data.frame(data.table::rbindlist(intersectsCoordsList, idcol=FALSE))
		# Update the segment IDs to account for multiple intersections:
		coords$segment <- unlist(by(coords$segment, coords$transect, seq_along))
		
		if(isTRUE(list(...)$plot)){
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
	transectsOneStratum <- function(stratumInd, xy, area, parameters, margin=NULL){
		# Get the parameters of the current stratumInd:
		parameters <- lapply(parameters, "[", stratumInd)
		
		# Rotate into a cartesian coordinate system having x axis aloing this bearing:
		xyRotated <- rotate2d(xy[[stratumInd]], parameters$bearing, data.frame.out=TRUE)
		
		# Get corners of the bounding box of the polygon (a slight value added to the y to ensure intersection with the polygon):
		dy <- diff(range(xyRotated$y))
		corners <- list(xmin=min(xyRotated$x), xmax=max(xyRotated$x), ymin=min(xyRotated$y) - dy*1e-9, ymax=max(xyRotated$y) + dy*1e-9)
		# Get the length of the stratum along the bearing:
		lengthOfStratum <- corners$xmax - corners$xmin
		
		# Subtract the length of the stratum, and return NULL if the traveled distance is shorter than this:
		if(parameters$nmi < lengthOfStratum){
			warning(paste("The traveled distance specified by nmi or duration and knots is shorter than the length of the stratum", stratumInd))
		}
		nmi_rest <- parameters$nmi - lengthOfStratum
		
		# Get the random seed point for the transects:
		set.seed(parameters$seed)
		fac <- runif(1)
		
		# If margin is given, iterate to obtain transects with total track length deviating at most by 'margin' relative to the input track length (margin = 0.05 implies between 19  and 21 hours, say):
		temp <- getTransectsByArea(nmi_rest=nmi_rest, area=area[stratumInd], fac=fac, corners=corners, xyRotated=xyRotated, type=parameters$type, bearing=parameters$bearing, seed=parameters$seed, knots=parameters$knots, retour=parameters$retour)
		
		numIter <- 1
		if(length(margin) && is.numeric(margin)){
			# Set the totalSailedDist, margin to use, and the last value for 'rest' and 'nmi_rest':
			totalSailedDist <- 0
			margin_nmi <- parameters$nmi * margin
			last_rest <- Inf
			last_nmi_rest <- Inf
			lastTemp <- NULL
			# Iterate to get a calculated tracklength within the margins
			while(abs(parameters$nmi - totalSailedDist) > margin_nmi){
				temp <- getTransectsByArea(nmi_rest=nmi_rest, area=area[stratumInd], fac=fac, corners=corners, xyRotated=xyRotated, type=parameters$type, bearing=parameters$bearing, seed=parameters$seed, knots=parameters$knots, retour=parameters$retour)
				
				totalSailedDist1 <- getDistTime(temp$coords, t0=parameters$t0, byStratum=FALSE)
				totalSailedDist <- sum(totalSailedDist1$segmentLengths, na.rm=TRUE)
				
				# Update the tracklength and rest tracklength:
				#totalSailedDist <- temp$Survey$total
				rest <- parameters$nmi - totalSailedDist
				# If increasing in rest value, break the loop and rerun with the previous settings:
				if(abs(last_rest) < abs(rest)){
					temp <- lastTemp
					warning(paste0("Sailed distance in stratum ", parameters$stratum, " did not converge to the desired sailed distance (",  parameters$nmi, " nmi). The closest used."))
					break
				}
				# Set the values of the last run:
				#nmi_rest <- nmi_rest + if(rest > 0) rest else 2 * rest
				last_nmi_rest <- nmi_rest
				last_rest <- rest
				lastTemp <- temp
				# Set new nmi_rest to use:
				nmi_rest <- nmi_rest + rest
				
				numIter <- numIter + 1
			}
			cat("Number of iterations to achieve total sailed distance within ", margin, " of the requested nmi in stratum ", parameters$stratum, " (", parameters$nmi, "): ", numIter, "\n", sep="")
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
		coords$type <- parameters$type
		
		#return(c(list(geo=geo, xy=xy), temp[names(temp) != "coords"]))
		return(coords)
	}
	############################################################
	############################################################
	
	
	# Define the stratum specific parameters:
	parameterNames <- c("type", "bearing", "retour", "hours", "knots", "nmi", "seed", "t0")
	# Save the input parameters for reference. These are included in the output:
	Input <- list(projectName=projectName, rev=rev, toursFirst=toursFirst, distsep=distsep, angsep=angsep, margin=margin, equalEffort=equalEffort, byStratum=byStratum, ...)
	
	
	# Read the the stratum polygons from a folder of shape files or from a StoX project:
	temp <- readStrataPolygons(projectName, ...)
	#lonlatAll <- temp$lonlatAll
	lonlat <- temp$lonlat
	#strataNames <- temp$strataNames
	strataNames <- names(lonlat)
	
	# Include all or a subset of the strata:
	#if(identical(strata, "all")){
	#	strata <- seq_along(strataNames)
	#}
	#else if(is.character(strata)){
	#	temp <- match(strata, strataNames)
	#	if(any(is.na(temp))){
	#		warning(paste0("The following stratum names specified in 'strata' were not found: ", paste(strata[is.na(temp)], collapse=", ")))
	#	}
	#	strata <- temp[!is.na(temp)]
	#}
	#else if(!all(strata < 0)){
	#	strata <- strata[strata >= 1 & strata <= length(lonlat)]
	#}
	strata <- subsetStrata(strata, lonlat)
	
	# Restrict the strata polygons to those specified by the input 'strata', and also by discarding strata with zero effort:
	lonlat <- lonlat[strata]
	strataNames <- strataNames[strata]
	nstrata <- length(lonlat)
	lonlatAll <- as.data.frame(data.table::rbindlist(lonlat, idcol="stratum"))
	
	# Include the stratum polygons as a list of matrices and a SpatialPolygons object:
	Input$lonlat <- lonlat
	Input$lonlatSP <- getSpatial(Input$lonlat)
	Input$lonlatAll <- lonlatAll
	
	# Get the projection to use, centered at the centroid of the total survey area (using rgeos::gCentroid()):
	if(length(centroid)==0){
		centroid <- rep(NA, 2)
	}
	Input$centroid <- centroid
	proj <- getProjString(proj="aeqd", x=lonlatAll[,c("longitude", "latitude")], lon_0=centroid[1], lat_0=centroid[2], requireClosed=FALSE)
	
	
	
	##### Generate the parameters, one value per stratum: #####
	if(length(parameters)==0 || !is.list(parameters)){
		parameters <- list()
	}
	else if(is.list(parameters) && "parameters" %in% names(parameters)){
		parameters <- parameters$parameters
	}
	
	# Treat parameters:	
	if("Straum" %in% names(parameters)){
		parameters <- parameters$Straum
	}
	# Add type to the parameters:
	if(length(parameters$type)==0){
		# Check for valid type:
		implementedTypes <- c("Parallel", "RectEnclZZ", "EqSpZZ")
		type <- implementedTypes[match(tolower(type), tolower(implementedTypes))]
		if(is.na(type)){
			warning(paste0("type ", type, " not matching any of the implemented types (", paste(implementedTypes, sep=", "), "). Parallel chosen"))
			type <- "Parallel"
		}
		# Add to the parameters list:
		parameters$type <- type
	}
	# Add bearing to the parameters:
	if(length(parameters$bearing)==0){
		# Get bearing of the stratum:
		parameters$bearing <- getBearing(bearing, data=lonlat, proj=proj, rev=rev)
	}
	# Add retour to the parameters:
	if(length(parameters$retour)==0){
		parameters$retour <- retour
	}
	# Add hours to the parameters:
	if(length(parameters$hours)==0){
		parameters$hours <- hours
	}
	if(is.list(parameters$hours)){
		parameters$hours <- rep(parameters$hours[[1]]/nstrata, length.out=nstrata)
	}
	# Add knots to the parameters:
	if(length(parameters$knots)==0){
		parameters$knots <- knots
	}
	# Get the total traveled length in nautical miles:
	if(length(parameters$nmi)==0){
		if(length(nmi)){
			parameters$nmi <- nmi
		}
		else{
			parameters$nmi <- parameters$hours * parameters$knots
		}
	}
	if(is.list(parameters$nmi)){
		parameters$nmi <- rep(parameters$nmi[[1]]/nstrata, length.out=nstrata)
	}
	
	# Add seed to the parameters:
	if(length(parameters$seed)==0){
		# Draw seeds for the transects:
		if(!is.integer(seed) || length(seed) < nstrata){
			set.seed(seed[1])
			seed <- getSeedV(seed[1], nstrata)
		}
		# Add to the parameters list:
		parameters$seed <- seed
	}
	# Add t0 to the parameters:
	if(length(parameters$t0)==0){
		if(length(t0)==0){
			t0 <- format(Sys.time(), tz="UTC")
		}
		else{
			t0 <- format(as.POSIXlt(t0), tz="UTC")
		}
		parameters$t0 <- t0
	}
	# Repeat the parameters to length equal to the number of strata
	#suppressWarnings(parameters <- lapply(parameters, function(x) if(is.list(x)) rep(x[[1]]/nstrata, length.out=nstrata) else rep(x, length.out=nstrata)))
	suppressWarnings(parameters <- lapply(parameters, rep,  length.out=nstrata))
	##########
	
	
	# If any parameters$nmi is zero, discard these strata:
	if(any(parameters$nmi == 0)){
		validStrata <- which(parameters$nmi > 0)
		parameters <- lapply(parameters, "[", validStrata)
		
		lonlat <- lonlat[validStrata]
		strataNames <- strataNames[validStrata]
		lonlatAll <- as.data.frame(data.table::rbindlist(lonlat, idcol="stratum"))
		proj <- getProjString(proj="aeqd", x=lonlatAll[,c("longitude", "latitude")], lon_0=centroid[1], lat_0=centroid[2], requireClosed=FALSE)
	}
	
		
	# Extract the centroids of the survey and the strata:
	survey_centroid <- getCentroid(lonlatAll)
	#stratum_centroid <- do.call(rbind, lapply(lonlat, getCentroid))
	stratum_centroid <- getCentroid(lonlat)
	
	###projList <- getProjString(proj="aeqd", x=lonlatAll[,c("longitude", "latitude")], list.out=TRUE)
	###centroid <- data.frame(lon_centroid=projList$lon_0, lat_centroid=projList$lat_0)
	###
	# Get the stratum areas:
	area <- unlist(lapply(lonlat, polyArea))
	if(equalEffort){
		parameters$nmi <- sum(parameters$nmi) * area / sum(area)
		###parameters$hours <- sum(parameters$hours) * area / sum(area)
		parameters$hours <- parameters$nmi / parameters$knots
	}
	
	# Convert parameters to a data frame and add stratum:
	parameters <- as.data.frame(c(list(stratum=strataNames), parameters), stringsAsFactors=FALSE)
	
	# Populate the stratum polygon borders with denser points, in order to preserve the geographic coordinate definition when converting to Cartesian coordinates (i.e., follow a latitude if two points are on the same latitude. If given only by two points, the azimuthal equal distance projection will follow the great circle, which in general will not coincide with the intended equal latitude path):
	lonlatPopulated <- lapply(lonlat, populatePath, dt=angsep, addInfo=FALSE)
	Input$lonlatPopulated <- lonlatPopulated
	
	# Convert to Cartesian:
	xy <- lapply(lonlatPopulated, geo2xy, data.frame.out=TRUE, par=proj)
	Input$xy <- xy
	
	
	#########################################
	##### Get transects for all strata: #####
	#########################################
	out <- lapply(seq_along(xy), transectsOneStratum, xy=xy, area=area, parameters=parameters, margin=margin)

	Transect <- as.data.frame(data.table::rbindlist(out, idcol="stratum"))
	# Convert the idcol 'stratum' from index to name:
	Transect$stratum <- strataNames[Transect$stratum]
	#Transect$stratum <- strataNames[validStrata][Transect$stratum]
	
	# If requested, reorder the strata so that the tours are first, followed by the retours reversed:
	if(toursFirst && length(unique(Transect$retour))==2){
		Transect <- orderTourRetour(Transect)
	}
	# Insert the next position as the stop position of the end of all strata except the last:
	if(!byStratum){
		Transect <- linkStrata(Transect)
	}
	# Get distance and time variables:
	Transect <- getDistTime(Transect, t0=parameters$t0, byStratum=byStratum)
	
	# Add the cruise and logStoXid, which is cruise/logDistance/date/time:
	Transect$cruise <- cruise
	Transect <- getLogStoXid(Transect, timevar="time_start")
	#dateSlashTime <- gsub(" ", "/", as.character(Transect$time_start), fixed=TRUE)
	#Transect$logStoXid <- paste(Transect$cruise, Transect$log_start, dateSlashTime, sep="/")
	
	# Convert back to (longitude, latitude):
	# Create a data frame in which to put the geographic coordinates, and cbind this to the output:
	xcols <- c("x_start", "x_stop", "x_mid")
	ycols <- c("y_start", "y_stop", "y_mid")
	loncols <- c("lon_start", "lon_stop", "lon_mid")
	latcols <- c("lat_start", "lat_stop", "lat_mid")
	lonlat <- Transect[,c(xcols, ycols)]
	colnames(lonlat) <- c(loncols, latcols)
	Transect <- cbind(lonlat, Transect)
	
	
	xy <- cbind(unlist(Transect[, xcols]), unlist(Transect[, ycols]))
	geo <- geo2xy(xy, par=proj, inv=TRUE)
	Transect[,c(loncols, latcols)] <- c(geo)
	#Transect <- cbind(lonlat, Transect)
	#########################################
	
	
	# Reorder the columns to have stratum, transect, segment first:
	firstcols <- c("cruise", "stratum", "transect", "segment", "transport", "retour", "logStoXid", "type")
	#Transect <- Transect[, c(firstcols, setdiff(colnames(Transect), firstcols)), with=FALSE]
	Transect <- Transect[, c(firstcols, setdiff(colnames(Transect), firstcols))]
	
	# Get the total sailed distance by stratum and after linking strata:
	totalSailedDist <- getSailedDist(Transect, nmi=parameters$nmi, area=area, nmi.out=FALSE, byStratum=TRUE)
	totalSailedDist1 <- getSailedDist(Transect, nmi=sum(parameters$nmi), area=sum(area), nmi.out=FALSE, byStratum=FALSE)
	
	
	# Get the number of transects of each stratum:	
	numTransects <- by(Transect$transport, Transect$stratum, function(x) sum(x==1))
	numTransects <- c(unlist(numTransects))
	
	# Construct the Stratum data frame:
	Stratum <- data.frameOrderByName(
		list(
			stratum = structure(strataNames, names=strataNames), 
			stratum_centroid[names(stratum_centroid) != "stratum"], 
			parameters[parameterNames], 
			totalSailedDist, 
			numTransects = numTransects
		), stringsAsFactors=FALSE
	)
	rownames(Stratum) <- Stratum$stratum
	
	# Convert the bearing to degrees (after comment from Espen Johnsen on 2019-01-22):
	Stratum$bearing <- Stratum$bearing * 180/pi
	
	# Construct the Survey data frame:
	Survey <- data.frame(survey_centroid, nmi=sum(parameters$nmi), totalSailedDist1, proj=proj, stringsAsFactors=FALSE)
	
	if(!keepTransport){
		Transect <- Transect[Transect$transport==0, , drop=FALSE]
	}
	
	# Output the following objects:
	# 1. Transect: Data frame with the transects (stratum, transect, segment, transport, retour, type, start, stop and mid of lon, lat, x, y, dist and time, and segmentLengths) [Already in place as 'Transect']
	# 2. Stratum: Data frame strata (stratum, area, type, bearing, retour, hours, knots, nmi, t0, seed, totalSailedDist)
	# 3. Survey: Data frame with a single row for the entire survey (projectName, proj, longitude_centroid, latitude_centroid)
	# 4. Input: A list of the input parameters
	list(
		Transect = Transect, 
		Stratum = Stratum, 
		Survey = Survey, 
		parameters = parameters, 
		Input = Input)
}
#' 
#' @export
#' @rdname surveyPlanner
#' 
addStratum <- function(x, acousticProc="FilterAcoustic"){
	
	temp <- x$outputData[[acousticProc]]$FilterAcoustic_AcousticData_DistanceFrequency.txt
	temp <- getLogStoXid(temp, timevar="start_time")
	
	# Add PSU ID:
	m <- match(temp$logStoXid, x$processData$edsupsu$EDSU)
	temp$PSU <- x$processData$edsupsu$PSU[m]
	# Add Stratum ID:
	m <- match(temp$PSU, x$processData$psustratum$PSU)
	temp$Stratum <- x$processData$psustratum$Stratum[m]
	
	x$outputData[[acousticProc]]$FilterAcoustic_AcousticData_DistanceFrequency.txt <- temp
	x
}
#' 
#' @export
#' @keywords internal
#' @rdname surveyPlanner
#' 
subsetStrata <- function(strata, lonlat){
	strataNames <- names(lonlat)
	if(identical(strata, "all")){
		strata <- seq_along(strataNames)
	}
	else if(is.character(strata)){
		temp <- match(strata, strataNames)
		if(any(is.na(temp))){
			warning(paste0("The following stratum names specified in 'strata' were not found: ", paste(strata[is.na(temp)], collapse=", ")))
		}
		strata <- temp[!is.na(temp)]
	}
	else if(!all(strata < 0)){
		strata <- strata[strata >= 1 & strata <= length(lonlat)]
	}
	strata
}
#' 
#' @export
#' @import ggplot2
#' @rdname surveyPlanner
#' 
plotStratum <- function(x, plot=c("map", "stratum", "transect"), centroid=NULL, transport_alpha=0.1, zoom=1.5, aspectratio=NULL, xlab="Longitude", ylab="Latitude", xlim=NULL, ylim=NULL, keep0effort=TRUE, strata="all", strataNameCol="darkblue", ...){
	
	if(is.character(x)){
		x <- list(Input=readStrataPolygons(x, ...))
		strata <- subsetStrata(strata=strata, lonlat=x$Input$lonlat)
		x$Input$lonlat <- x$Input$lonlat[strata]
		x$Input$lonlatAll <- as.data.frame(data.table::rbindlist(x$Input$lonlat, idcol="stratum"))
	}
	
	# Remove non-empty strata;
	if(!keep0effort){
		x$Input$lonlatAll <- x$Input$lonlatAll[x$Input$lonlatAll$stratum %in% x$Stratum$stratum, ]
	}
	
	# Get the range in geographic coordinates:
	#rangelonlat <- cbind(range(x$Transect$lon_start), range(x$Transect$lat_start))
	rangelonlat <- cbind(range(x$Input$lonlatAll$longitude), range(x$Input$lonlatAll$latitude))
	if(length(centroid)==0){
		# centroid <- unlist(x$centroid)
		centroid <- apply(rangelonlat, 2, mean)
	}
	#location <- colMeans(x[, c("longitude", "latitude")])
	
	# Get the data from the map package or alternatively from Google:
	if("map" %in%  tolower(plot)){
		# Google option removed on 2019-01-23. See News for Rstox 1.10:
		#if(google){
		#	gmap <- ggmap::get_map(location=centroid, zoom=gzoom, maptype="terrain", source="google", col="bw")
		#	# Initiate the plot:
		#	p <- ggmap::ggmap(gmap)
		#}
		#else{
			# get the map and set the limits and aspect ratio:
			gmap <- map_data("world")
			spanlonlat <- apply(rangelonlat, 2, diff)
			#fact <- c(-1, 1) * (1 + margin)
			if(zoom <= 0){
				warning("The value of 'zoom' must be positive, with default 1 implying no zoom. The default was used.")
			}
			fact <- c(-1, 1) * zoom
			if(length(xlim) == 0){
				xlim <- centroid[1] + fact * spanlonlat[1] / 2
			}
			if(length(ylim) == 0){
				ylim <- centroid[2] + fact * spanlonlat[2] / 2
			}
			# Adjust the aspect ratio by latitude:
			aspectratio <- 1 / cos(centroid[2] * pi/180)
			# Initiate the plot:
			p <- ggplot() + geom_polygon(data=gmap, aes_string(x="long", y="lat", group="group")) + coord_fixed(aspectratio, xlim=xlim, ylim=ylim)
			#}
	}
	else{
		# Initiate an empty plot:
		p <- ggplot()
	}
	
	# Add the strata:
	if(any(c("stratum", "strata") %in% tolower(plot))){
		p <- p + geom_polygon(data=x$Input$lonlatAll, aes_string(x="longitude", y="latitude", fill="stratum", group="stratum"), colour="black", alpha=0.3, inherit.aes=FALSE)
		#p <- p + geom_polygon(data=x$Input$lonlatAll, aes_string(x="longitude", y="latitude", fill="stratum", group="stratum"), colour=NA, alpha=0.3, inherit.aes=FALSE)
	}
	
	
	# Add transects:
	if(any(c("transect", "transects") %in% tolower(plot))){
		if(length(x$Transect)==0){
			warning("The data frame 'Transect' missing in the input list 'x'")
		}
		else{
		# Use the retour as line type:
			hasRetour <- length(unique(x$Transect$retour)) > 1
			# Convert to factor:
			x$Transect$stratum <- as.factor(x$Transect$stratum)
			x$Transect$retour <- as.factor(x$Transect$retour)
			x$Transect$alpha <- 1 - x$Transect$transport
		
			p <- p + 
				geom_segment(data=x$Transect, aes_string(x="lon_start", y="lat_start", xend="lon_stop", yend="lat_stop", group="stratum", colour="stratum", alpha="alpha", linetype="retour"), show.legend=TRUE) + 
				scale_alpha(range = c(transport_alpha, 1), guide=FALSE) + 
				scale_colour_discrete(guide=FALSE) + 
				if(hasRetour) scale_linetype(name="retour") else scale_linetype(guide=FALSE)
		}
	}
	
	# Add labels:
	p <- p + xlab(xlab) + ylab(ylab)
	
	# Add stratum names:
	if(any(c("stratum", "strata") %in% tolower(plot))){
		### stratum_centroid <- getCentroid(x$Input$lonlat)
		### #p <- p + geom_text(data=x$Stratum, aes_string(group="stratum", x="lon_centroid", y="lat_centroid", label="stratum"))
		### p <- p + geom_polygon(data=x$Input$lonlatAll, aes_string(x="longitude", y="latitude", group="stratum"), fill=NA, colour="black", inherit.aes=FALSE)
		### p <- p + geom_text(data=stratum_centroid, aes_string(group="stratum", x="lon_centroid", y="lat_centroid", label="stratum"), colour=strataNameCol)
		p <- addStratumBordersAndNames(p, x, strataNameCol=strataNameCol)
	}
	#annotate("text", x=x$Stratum$lon_centroid, y=x$Stratum$lat_centroid, label=x$Stratum$stratum, alpha=0.5, col=)
	
	# Run the plot
	print(p)
	
	return(p)
}
#' 
#' @export
#' @import ggplot2
#' @rdname surveyPlanner
#' 
addStratumBordersAndNames <- function(p, x, strataNameCol="darkblue"){
	stratum_centroid <- getCentroid(x$Input$lonlat)
	p <- p + geom_polygon(data=x$Input$lonlatAll, aes_string(x="longitude", y="latitude", group="stratum"), fill=NA, colour="black", inherit.aes=FALSE)
	p <- p + geom_text(data=stratum_centroid, aes_string(group="stratum", x="lon_centroid", y="lat_centroid", label="stratum"), colour=strataNameCol)
	p
}
#' 
#' @export
#' @importFrom rgdal readOGR
#' @import ggplot2
#' @importFrom tools file_path_sans_ext file_ext
#' @rdname surveyPlanner
#' 
readStrataPolygons <- function(projectName, ...){
	
	# Function for reading a WKT string to a list of geographical data:
	readWKTstring <- function(x){
		# Function for converting the list of polygon matrices to data frames named with "longitude", "latitude":
		as.data.frameAddLonLatColnames <- function(x){
			as.data.frameAddLonLatColnamesOne <- function(x){
				setNames(as.data.frame(x), c("longitude", "latitude"))
			}
			if(is.list(x)){
				lapply(x, as.data.frameAddLonLatColnamesOne)
			}
			else{
				as.data.frameAddLonLatColnamesOne(x)
			}
		}
		
		if(!is.data.frame(x)){
			x <- read.table(x, sep="\t", stringsAsFactors=FALSE)
		}
		
		# Interpret which of the columns contain the multipolygons:
		multipolygonCol <- which(tolower(sapply(x, function(x) substr(head(x, 1), 1, 5))) == "multi")
		if(length(multipolygonCol) == 0){
			warning("There is no column with MULTIPOLYGON() strings")
		}
		
		strataNames <- x[, 1]
		lonlatFull <- lapply(x[, multipolygonCol], getMatrixList)

		lonlatFull <- lapply(lonlatFull, as.data.frameAddLonLatColnames)
		#lonlat <- lapply(lonlat, as.data.frame, col.names=c("longitude", "latitude"))
		#lonlat <- lapply(lonlat, setNames, c("longitude", "latitude"))
		names(lonlatFull) <- strataNames
		
		# Check the number of polygons in each multipolygon:
		areMultipolygons <- sapply(lonlatFull, function(x) length(nrow(x))==0)
		if(any(areMultipolygons)){
			warning(paste0("The following strata contains multiple polygons: ", paste0(strataNames[areMultipolygons], collapse=", "), ". Only the first polygon kept in the output 'lonlat' and 'lonlatAll' (see 'lonlatFull' for all the multipolygons)."))
			lonlat <- lapply(lonlatFull, function(x) if(!is.data.frame(x)) x[[1]] else x)
		}
		else{
			lonlat <- lonlatFull
		}
		lonlatAll <- as.data.frame(data.table::rbindlist(lonlat, idcol="stratum"))
		
		# Return:
		list(lonlat=lonlat, lonlatAll=lonlatAll, lonlatFull=lonlatFull, strataNames=strataNames)
	}
	
	# Function for reading shapefiles:
	readShapefiles <- function(x, shapenames=list(longitude="long", latitude="lat", stratum="id"), ...){
		dsn <- dirname(path.expand(x))
		layer <- tools::file_path_sans_ext(basename(x))
		shape <- rgdal::readOGR(dsn=dsn, layer=layer)
		shape <- ggplot2::fortify(shape)
		#lonlatAll <- data.frame(longitude=shape$long, latitude=shape$lat, stratum=shape$id)
		lonlatAll <- data.frame(longitude=shape[[shapenames$longitude]], latitude=shape[[shapenames$latitude]], stratum=shape[[shapenames$stratum]], stringsAsFactors=FALSE)
		lonlat <- split(lonlatAll, lonlatAll$stratum)
		lonlat <- lapply(lonlat, "[", c("longitude", "latitude"))
		strataNames <- unique(lonlatAll$stratum)
		
		# Return:
		list(lonlat=lonlat, lonlatAll=lonlatAll, strataNames=strataNames)
	}
	
	# If the 'projectName' is a project, get the strata from getBaseline():
	if(all(isProject(projectName))){
		# Get the baseline output and number of strata:
		g <- getBaseline(projectName, endProcess="ReadProcessData", input="proc", proc=NULL, drop=FALSE)
		strataNames <- g$processData$stratumpolygon$Stratum
	
		# Get the strata polygons in geographic coordinates (longitude, latitude) in a list named with the strata names:
		lonlat <- lapply(g$processData$stratumpolygon$Polygon, getMatrixList, data.frame.out=TRUE)
		names(lonlat) <- strataNames
		lonlat <- lapply(lonlat, "colnames<-", c("longitude", "latitude"))
		# Test of southern hemisphere:
		#lonlat <- lapply(lonlat, function(x) {x$latitude <- -x$latitude; x})
	
		# Create a single data frame version of the strata polygons, with stratum as the third column, and get a common projection definition using the centroid of the system:
		lonlatAll <- as.data.frame(data.table::rbindlist(lonlat, idcol="stratum"))
		
		out <- list(lonlat=lonlat, lonlatAll=lonlatAll, strataNames=strataNames)
	}
	# Read the strata from shapefiles, a WKT file ot WKT string:
	else if(is.character(projectName)){
		if(all(file.exists(projectName))){
			# Assure that the 'projectName' is a vector of files and not a directory, for convenience:
			if(length(projectName) == 1 && isTRUE(file.info(projectName)$isdir)){
				projectName <- list.files(projectName, full.names=TRUE)
			}
			# Read from shapefiles:
			if(any(tolower(tools::file_ext(projectName)) == "shp")){
				out <- readShapefiles(projectName[1], ...)
			}
			# Read from a WKT file:
			else{
				out <- readWKTstring(projectName[1])
			}
		}
		# Read from a WKT string:
		else{
			out <- readWKTstring(textConnection(projectName[1]))
		}
	}
	# Support for giving a dara frame with stratum name in the first column, and a column of MULTIPOLYGON strings:
	else if(is.data.frame(projectName) && nrow(projectName)>0){
		out <- readWKTstring(projectName)
	}
	# Support for a list as returned from getBaseline():
	else if(is.list(projectName) && is.data.frame(projectName$processData$stratumpolygon)){
		out <- readWKTstring(projectName$processData$stratumpolygon)
	}
	else{
		warning("'projectName' is not a project, path to shapefiles/folder of shapefiles, or a path to a WKT file or the content of the WKT file (string)")
		return()
	}
	
	return(out)
}
#' 
#' @export
#' @keywords internal
#' @rdname surveyPlanner
#' 
getCentroid <- function(lonlat, proj="aeqd", lonlatnames=c("longitude", "latitude")){
	getCentroidOne <- function(x, proj="aeqd", lonlatnames=c("longitude", "latitude")){
		projList <- getProjString(proj=proj, x=x[, lonlatnames], list.out=TRUE, requireClosed=FALSE)
		centroid <- data.frame(lon_centroid=projList$lon_0, lat_centroid=projList$lat_0)
		centroid
	}
	
	if(is.list(lonlat) && is.data.frame(lonlat[[1]])){
		cbind(stratum=names(lonlat), do.call(rbind, lapply(lonlat, getCentroid)))
	}
	else if(is.data.frame(lonlat)){
		getCentroidOne(lonlat, proj=proj, lonlatnames=lonlatnames)
	}
	else{
		stop("Input needs to be a data frame or list of data frames")
	}
}


#*********************************************
#*********************************************
#' Write transects generated by \code{\link{surveyPlanner}} to various report files. File names, if not given explicitely, are set to contain the transect type and if byStratum=TRUE, the stratum names. 
#'
#' \code{writeTransectsToMAxSea} writes the transects of all strata to separate .asc files for import in MaxSea. \cr \cr
#' \code{writeTransectsINFO} writes the transects of all strata to separate hunam readable .txt files. \cr \cr
#' \code{writeTransectsCSV} writes the transects of all strata to separate csv files files. \cr \cr
#' \code{writeTransects} writes the data to CSV or NetCDF files. Only a selection of the columns are saved by default (override this by \code{cols}): The transect secifications ("stratum", "transect", "segment", "transport", "retour"), the start and stop position, time and distance ("lon_start", "lon_stop", "lat_start", "lat_stop", "time_start", "time_stop", "dist_start", "dist_stop", "segmentLengths"). By default only transects and not transport stretches are saved (override this by \code{keepTransport}). \cr \cr
#' \code{writeTransectsGPX} writes the data to the GPS exchange format. \cr \cr
#' 
#' @param x								The output from \code{\link{surveyPlanner}}.
#' @param projectName   				The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param dir   						The path to the directory in which to put the files. If given, it takes precidence over the report directory of the project given by \code{projectName}.
#' @param digits						Precision to use in the reports.
#' @param byStratum						Logical: If TRUE, create one file per stratum.
#' @param cols							The columns to write to file for \code{writeTransects}.
#' @param keepTransport					Logical: If TRUE, save also the transport stretches to the file.
#' @param prefix,suffix					Strings to add prior to or after the file names (but before file extension).
#' @param filenames						Optional file names overriding the default file names.
#' @param ext							The file extension. For \code{writeTransects} \code{ext} = "nc" implies writing NetCDF files, and otherwise a CSV files are written.
#' @param item.type,item.id,item.col	The type (default 257), id (default 20) and color (default 1) to pass to MaxSea.
#' @param north,east					Strings denoting North and East to pass to MaxSea.
#' @param ...							Parameters passed to \code{\link[utils]{write.table}}, \code{\link[utils]{write.csv}} and \code{\link[data.table]{fwrite}}. For \code{writeTransects} the \code{...} contains the important variables \code{sep} specifying the separator used in CSV files, and \code{dateTimeAs} which defines how to save the time in \code{\link[data.table]{fwrite}}.
#' 
#' @return The file names.
#'
#' @export
#' @rdname writeTransects
#' 
writeTransects <- function(x, projectName, dir=NULL, digits=5, byStratum=TRUE, cols=NULL, keepTransport=TRUE, prefix="", suffix="", filenames=NULL, ext="txt", ...){
	
	# Function for writing one stratum:
	writeTransects_OneStratum <- function(stratumInd, x, cols, keepTransport, ext, units, ...){
		# Extract the file name from the names of the input:
		filename <- names(x$Transect)[stratumInd]
		# Select the current stratum:
		Transect <- x$Transect[[stratumInd]]
		
		# Select the appropriate columns, and only the transect rows (not transport):
		if(!keepTransport){
			Transect <- Transect[Transect$transport==0, , drop=FALSE]
		}
		Transect <- Transect[, cols, drop=FALSE]
		numericCols <- sapply(Transect, is.numeric)
		Transect[,numericCols] <- round(Transect[,numericCols], digits=digits)
		
		# Write the data:
		if(ext=="nc"){
			#library(ncdf4)
			# Define the variables with Length and dimension state:
			L <- nrow(Transect)
			dimState <- ncdf4::ncdim_def(name="Row", units="count", vals=seq_len(L))
			ncvars <- lapply(seq_along(cols), function(i) ncdf4::ncvar_def(cols[i], units=units[i], dim=dimState))
			
			# Create the NetCDF file:
			ncnew <- ncdf4::nc_create(filename, ncvars)
			# Write the variables to the file:
			lapply(seq_along(cols), function(i) ncdf4::ncvar_put(nc=ncnew, varid=cols[i], vals=Transect[[cols[i]]], start=1, count=L))
			
			ncdf4::ncatt_put(nc=ncnew, varid=0, attname="UNIX epoch time", "1970-01-01 00:00:00")
			
	      	# Close the file:
			ncdf4::nc_close(ncnew)
		}
		else{
			data.table::fwrite(Transect, filename, ...)
		}
	}
	
	# Define units of all present columns_
	allcols <- c(
		"cruise", "stratum", "transect", "segment", "transport", "retour", "logStoXid", "type", 
		"lon_start", "lon_stop", "lon_mid", "lat_start", "lat_stop", "lat_mid", 
		"knots", 
		"x_start", "x_stop", "x_mid", "y_start", "y_stop", "y_mid", 
		"dist_start", "dist_stop", "dist_mid", 
		"log_start", "dlogstop", "dlogmid", 
		"time_start", "time_stop", "time_mid", 
		"segmentLengths")
	allunits <- c(
		"String", "Integer", "Integer", "Integer", "Logical", "String", 
		rep("Decimal degrees", 6), 
		"Knots", 
		rep("Nautical miles", 6), 
		rep("Nautical miles", 3), 
		rep("Nautical miles (rounded off to 1 decimal)", 3), 
		rep("UNIX time", 3), 
		"Nautical miles")
	
	# Define the columns to keep:
	if(!length(cols)){
		cols <- c(
			"cruise", "stratum", "transect", "segment", "transport", "retour", "logStoXid", "type", "knots", 
			"lon_start", "lon_stop", "lat_start", "lat_stop", 
			"dist_start", "dist_stop", 
			"log_start", "log_stop", 
			"time_start", "time_stop", 
			"segmentLengths"
		)
	}
	else if(tolower(cols)=="all"){
		cols <- colnames(x$Transect)
	}
	if(!keepTransport){
		cols <- cols[cols != "transport"]
	}
	
	units <- allunits[match(cols, allcols)]
				 
	# Split into strata (if byStratum==TRUE), and set the files names as names of the list:
	if(length(filenames)==0){
		filenames <- getTransectFileName(x=x, projectName=projectName, prefix=prefix, suffix=suffix, ext=ext, dir=dir, byStratum=byStratum)
	}
	
	x$Transect <- split(x$Transect, if(byStratum) x$Transect$stratum else 1)
	names(x$Transect) <- filenames
	# Write the files:
	lapply(seq_along(x$Transect), writeTransects_OneStratum, x=x, cols=cols, keepTransport=keepTransport, ext=ext, units=units, ...)
	
	# Return the file names
	filenames
}
#'
#' @export
#' @rdname writeTransects
#' 
writeTransectsMaxSea <- function(x, projectName, dir=NULL, item.type=257, item.id=20, item.col=1, north="N", east="E", digits=3, prefix="", suffix="", filenames=NULL, ...){
	# Function for writing one stratum:
	writeTransectsMaxSea_OneStratum <- function(stratumInd, x, item.type=257, item.id=20, item.col=1, north="N", east="E", digits=3, ...){
		# Extract the file name from the names of the input:
		filename <- names(x$Transect)[stratumInd]
		
		# Select the current stratum:
		Transect <- x$Transect[[stratumInd]]
		
		# Create a matrix suited for loading in MaxSea:
		numrow <- nrow(Transect)
		out <- cbind(
			rep(item.type, numrow),
			rep(item.id, numrow),
			rep(item.col, numrow),
			round(Transect$abs_lat_start, digits=digits),
			Transect$NorthSouth, 
			#rep(north, numrow),
			round(Transect$abs_lon_start, digits=digits),
			Transect$EastWest)
			
		# Write to the .asc file:
	    write.table(out, file=filename, row.names=FALSE, col.names=FALSE, sep=",", quote=FALSE, ...)
	}
	
	# Split into strata, and set the files names as names of the list:
	if(length(filenames)==0){
		filenames <- getTransectFileName(x=x, projectName=projectName, prefix=prefix, suffix=suffix, ext="asc", dir=dir, byStratum=TRUE)
	}
	
	# Add columns NorthSouth and EastWest, and abs_lat_start and abs_lon_start to the transects, for writing:
	x$Transect$NorthSouth <- c("N", "S")[as.numeric(x$Transect$lat_start > 0) + 1]
	x$Transect$EastWest <- c("E", "W")[as.numeric(x$Transect$lon_start > 0) + 1]
	x$Transect$abs_lat_start <- x$Transect$lat_start
	x$Transect$abs_lon_start <- x$Transect$lon_start
	
	#x$Transect <- split(x$Transect, if(byStratum) x$Transect$stratum else 1)
	x$Transect <- split(x$Transect, x$Transect$stratum)
	names(x$Transect) <- filenames
	# Write the files:
	lapply(seq_along(x$Transect), writeTransectsMaxSea_OneStratum, x=x, item.type=item.type, item.id=item.id, item.col=item.col, north=north, east=east, digits=digits, ...)
	
	# Return the file names
	filenames
}
#'
#' @export
#' @rdname writeTransects
#' 
writeTransectsINFO <- function(x, projectName, dir=NULL, digits=2, prefix="", suffix="", filenames=NULL){
	# Function for writing one stratum:
	writeTransectsINFO_OneStratum <- function(stratumInd, x, digits=2){
		# Extract the file name from the names of the input:
		filename <- names(x$Transect)[stratumInd]
		
		# Select the current stratum:
		Transect <- x$Transect[[stratumInd]]
		Stratum <- x$Stratum[[stratumInd]]
		parameters <- x$parameters[[stratumInd]]
		
		# Get the total travelled distance and the survey coverage:
		dist <- Stratum$total
	    area.nm2 <- Stratum$area
	    Sur.cov <- dist / sqrt(area.nm2)
		
		# Select the current stratum:
		LatDeg <- floor(Transect$lat_start)
		LonDeg <- floor(Transect$lon_start)
		LatMin <- round(60*(Transect$lat_start  - LatDeg), digits=digits)
		LonMin <- round(60*(Transect$lon_start - LonDeg), digits=digits)
		lonlat <- data.frame(LatDeg=LatDeg, LatMin=LatMin, LonDeg=LonDeg, LonMin=LonMin)
		
		# wWrite to the file:
	    capture.output( cat("\n", format(c(date()), width=20, justify = "left")), file=filename)
	    capture.output( cat("\n", "Stratum (Cruise region) ", format(Stratum$stratum, width=7, justify="right")), file=filename, append=TRUE)
	    capture.output( cat("\n", "Speed and time available", format(c(paste(parameters$knots, "knots"), paste(parameters$hours, "h")), width=7, justify="right")), file=filename, append=TRUE)
	    capture.output( cat("\n", "Stratum area (n.mi2)    ", format(area.nm2, width=7, justify="right")), file=filename, append=TRUE)
	    capture.output( cat("\n", "Sailing distance (n.mi) ", format(dist, width=7, justify="right")), file=filename, append=TRUE)
	    capture.output( cat("\n", "Survey coverage         ", format(Sur.cov, width=7, justify="right")), file=filename, append=TRUE)
	    capture.output( cat("\n", " "), file=filename, append=TRUE)
	    capture.output( cat("\n", "Transect positions      "), file=filename, append=TRUE)
	    capture.output( cat("\n", " "), file=filename, append=TRUE)
	    capture.output( cat("\n", " "), lonlat, file=filename, append=TRUE) 
	}
	
	# Split into strata, and set the files names as names of the list:
	if(length(filenames)==0){
		filenames <- getTransectFileName(x=x, projectName=projectName, prefix=prefix, suffix=if(nchar(suffix)==0) "INFO" else suffix, ext="txt", dir=dir, byStratum=TRUE)
	}
	
	x$Transect <- split(x$Transect, x$Transect$stratum)
	x$Stratum <- split(x$Stratum, x$Stratum$stratum)
	x$parameters <- split(x$parameters, x$parameters$stratum)
	names(x$Transect) <- filenames
	# Write the files:
	lapply(seq_along(x$Transect), writeTransectsINFO_OneStratum, x=x, digits=digits)
	
	# Return the file names
	filenames
}
#'
#' @export
#' @rdname writeTransects
#' 
writeTransectsTRACK <- function(x, projectName, dir=NULL, digits=5, prefix="", suffix="", filenames=NULL, ...){
	# Function for writing one stratum:
	writeTransectsTRACK_OneStratum <- function(stratumInd, x, ...){
		# Extract the file name from the names of the input:
		filename <- names(x$Transect)[stratumInd]
		
		# Select the current stratum:
		Transect <- x$Transect[[stratumInd]]
		
		out <- data.frame(Line=1, longitude=round(Transect$lon_start, digits=digits), latitude=round(Transect$lat_start, digits=digits))
		write.csv(out, file=filename, row.names=FALSE, ...)
	}
	
	# Split into strata, and set the files names as names of the list:
	if(length(filenames)==0){
		filenames <- getTransectFileName(x=x, projectName=projectName, prefix=prefix, suffix=if(nchar(suffix)==0) "TRACK" else suffix, ext="txt", dir=dir, byStratum=TRUE)
	}
	
	x$Transect <- split(x$Transect, x$Transect$stratum)
	names(x$Transect) <- filenames
	# Write the files:
	lapply(seq_along(x$Transect), writeTransectsTRACK_OneStratum, x=x, ...)
	
	# Return the file names
	filenames
}
#'
#' @export
#' @importFrom data.table fwrite
#' @rdname writeTransects
#' 
writeTransectsGPX <- function(x, projectName, dir=NULL, digits=5, prefix="", suffix="", filenames=NULL){
	# Function for writing one stratum:
	writeTransectsGPX__OneStratum <- function(stratumInd, x){
		# Extract the file name from the names of the input:
		filename <- names(x$Transect)[stratumInd]
		
		# Select the current stratum:
		Transect <- x$Transect[[stratumInd]]
		
		out <- data.frame(wp=seq_len(nrow(Transect)), Long=round(Transect$lon_start, digits=digits), Lat=round(Transect$lat_start, digits=digits))
		# Use the suggested pgirmess package:
		pgirmess::writeGPX(out, file=filename)
	}

	# Split into strata, and set the files names as names of the list:
	if(length(filenames)==0){
		filenames <- getTransectFileName(x=x, projectName=projectName, prefix=prefix, suffix=suffix, ext="gpx", dir=dir, byStratum=TRUE)
	}
	
	x$Transect <- split(x$Transect, x$Transect$stratum)
	names(x$Transect) <- filenames
	# Write the files:
	lapply(seq_along(x$Transect), writeTransectsGPX__OneStratum, x=x)
	
	# Return the file names
	filenames
}
#'
#' @export
#' @importFrom data.table fwrite
#' @rdname writeTransects
#' 
getTransectFileName <- function(x, projectName, prefix="", suffix="", ext="txt", dir=NULL, byStratum=TRUE){
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
	Transects <- paste0("TransectType_", type)
	stratum <- paste0("_Stratum_", unique(x$Stratum$stratum))
	if(nchar(prefix)){
		prefix <- paste0(prefix, "_")
	}
	if(nchar(suffix)){
		suffix <- paste0("_", suffix)
	}
	file.path(dir, paste0(prefix, Transects, if(byStratum) stratum, suffix, ".", ext))
}


data.frameOrderByName <- function(x, ...){
	orderByNames <- function(x, names){
		dimx <- dim(x)
		if(length(dimx)==0){
			if(length(x)==1){
				x
			}
			else{
				x[match(names, names(x))]
			}
		}
		else{
			if(dimx[1]==1){
				x
			}
			else{
				x[match(names, rownames(x)), ]
			}
		}
	}
	firstCol <- x[1]
	rest <- x[-1]
	
	rest <- lapply(rest, orderByNames, names=names(firstCol[[1]]))
	
	do.call(data.frame, c(firstCol, rest, ...))
}