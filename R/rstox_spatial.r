#*********************************************
#*********************************************
#' Get polygon area and convert to or from geographic and Cartesian coordinates.
#'
#' \code{polyArea} calculates the area of a multipolygon. \cr \cr
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
geo2xy <- function(x, par=list(proj="aeqd", units="kmi", lon_0=NA, lat_0=NA, x_0=0, y_0=0, ellps="WGS84", datum="WGS84"), inv=FALSE, data.frame.out=FALSE, ...){
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
#' @keywords internal
#' @rdname polyArea
#' 
getSpatial <- function(x){
	if(isMatrixList(x)){
		x <- matrixList2multipolygon(x)
		x <- rgeos::readWKT(x)
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
	
	# Function for extracting the coordinates of one multipolygon:
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
	# Function for extracting the coordinates of spatialPoints:
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
		if(length(out)==1){
			out <- out[[1]]
		}
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
#' Get polygon area and convert to or from geographic and Cartesian coordinates.
#'
#' @param x					Either a two column matrix of x and y coordinates, indicating only one polygon, or a list of such matrices, indicating several polygons. If a list of lists of two column matrices are given, the first matrix of each list is the polygon, and the following are subtractions. \code{wkt} cal also be a wkt string such as "MULTIPOLYGON(((4 55, 15 56, 15 59, 4 59, 4 55)))".
#' @param multipolygon		A multipolygon string.
#' @param drop				Logical: If TRUE drop the list if only one multipolygon or only one polygon is given.
#' @param data.frame.out	Logical: If TRUE convert the matrices to data frames with columns x and y.
#' 
#' @export
#' @importFrom sp Lines Line SpatialLines
#' @importFrom rgeos gIntersection
#' @import data.table
#' @importFrom utils head tail
#' @rdname surveyPlanner
#' 
surveyPlanner <- function(projectName, type=c("p", "ze", "z"), bearing="N", hours=240, knots=10, nmi=NULL, speed=NULL, seed=0, dt=1/60) {
	populatePath <- function(xy, N=100, dt=NULL, list.out=FALSE){
		temp <- attributes(xy)
		if(!is.data.frame(xy)){
			xy <- as.data.frame(xy)
		}
		# Get cummulative traveled distance, which we interpret as time (for constant speed):
		difft <- sqrt(rowSums(do.call(cbind, lapply(xy, diff))^2))
		if(max(difft)==0){
			return(cbind(xy, Populated=0))
		}
		t <- c(0, cumsum(difft))
		if(length(dt)==0){
			dt <- t[length(t)] / N
		}
	
		tseqOne <- function(i, xy, difft, t, dt){
			if(difft[i] > dt){
				tnew <- seq(t[i], t[i+1], dt)
			}
			else{
				tnew <- t[i]
			}
			tnew
		}
	
		tnew <- unlist(lapply(seq_len(length(difft)), tseqOne, xy=xy, difft=difft, t=t, dt=dt))
		xynew <- apply(xy, 2, function(z) approx(x=t, y=z, xout=tnew)$y)
		if(length(dim(xynew)) < 2){
			xynew <- t(xynew)
		}
		xynew <- as.data.frame(xynew)
		# Add the last point to close the path:
		xynew <- rbind(xynew, xy[nrow(xy), ])
		# Add a column denoting which rows are populated:
		Populated <- double(nrow(xynew)) + 1
		Populated[c(1, length(Populated))] <- 0
		xynew <- cbind(xynew, Populated=Populated)
		
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
		A = matrix(c(cos(ang), sin(ang), -sin(ang), cos(ang)), 2, 2, byrow=TRUE)
		out <- t(A %*% t(x))
		if(data.frame.out){
			out <- as.data.frame(out)
		}
		colnames(out) <- c("x", "y")
		out
	}
	# Function for calculating the bearing of a survey:
	getBearing <- function(bearing, deg=TRUE){
		if(is.character(bearing)){
			NWSE <- c("N", "W", "S", "E")
			WE <- c("W", "E")
			NWSE_angles <- 1:4 * pi/2
			WE_angles <- c(1,-1) * pi/4
			strings <- c(NWSE, outer(WE, NWSE, paste0))
			angles <- c(NWSE_angles, outer(NWSE_angles, WE_angles, "+"))
			hit <- which(strings==bearing)
			if(length(hit)){
				return(angles[hit])
			}
			else{
				warning(paste0("'bearing not matching any of'", paste(strings, collapse=", ")))
				return(bearing)
			}
		}
		else{
			return(if(deg) bearing*pi/180 else bearing)
		}
	}
	# Function for getting a transect at the end with the inverse angle of the first/last transect:
	getEndTransectOne <- function(poly, intersects, last=FALSE){
		if(last){
			intersects@coords <- intersects@coords[rev(seq_len(nrow(intersects@coords))),]
		}
		first <- intersects@coords[1,]
		firstLine <- rbind(intersects@coords[1,], intersects@coords[1,] + diff(intersects@coords[1:2,]) * c(-1,1) * 2)
		# Create a spatial lines object:
		spatialLinesEnd <- sp::Lines(list(sp::Line(firstLine)), ID = 1)
		spatialLinesEnd <- sp::SpatialLines(list(spatialLinesEnd))
	
		# Find intersection point between the line and the polygon:
		intersectsFirst <- rgeos::gIntersection(poly, spatialLinesEnd)
		if(length(intersectsFirst)==0){
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
	# Function for adding stratum ends:
	addEndTransects <- function(intersects, poly){
		first <- getEndTransectOne(poly, intersects)
		last <- getEndTransectOne(poly, intersects, last=TRUE)
		#intersects@coords <- rbind(cbind(Transect=0, first@coords), intersects@coords, cbind(Transect=max(poly[, "Transect"]) + 1, last@coords))
		intersects@coords <- rbind(first@coords, intersects@coords, last@coords)
		intersects
	}
	## Function for ordering points alternately in a list:
	#orderAlternate <- function(i, x, decreasing=FALSE){
	#	# Order the first by x and then y:
	#	if(i==1){
	#		x[[i]][order(x[[i]]$x, x[[i]]$y), ]
	#	}
	#	else{
	#		at <- as.numeric(decreasing) + i - 1
	#		print(c(i, at, at%%2 == 1))
	#		x[[i]][order(x[[i]]$y, decreasing=at%%2 == 1), ]
	#	}
	#}
	## Function for ordering points alternately in a list:
	#orderAlternate <- function(i, x, decreasing=FALSE){
	#	# Order the first by x, y:
	#	x[[i]] <- x[[i]][order(x[[i]]$x, x[[i]]$y), ]
	#	# Then order alternately
	#	at <- as.numeric(decreasing) + i - 1
	#	x[[i]][order(x[[i]]$y, decreasing=at%%2 == 1), ]
	#}
	# Function for ordering points alternately in a list:
	orderTransectsByXY <- function(x){
		orderTransectsByXYOne <- function(x){
			x[order(x$x, x$y), ]
		}
		lapply(x, orderTransectsByXYOne)
	}
	# Function for ordering points alternately in a list:
	orderAlternateByY <- function(x, decreasing=FALSE){
		orderAlternateOne <- function(i, x, decreasing=FALSE){
			# Order alternately
			at <- as.numeric(decreasing) + i - 1
			decreasing <- at%%2 == 1
			x[[i]][order(x[[i]]$y, decreasing=decreasing), ]
		}
		lapply(seq_along(x), orderAlternateOne, x, decreasing=decreasing)
	}
	orderAlternate <- function(x, decreasing=FALSE){
		x <- orderTransectsByXY(x)
		x <- orderAlternateByY(x, decreasing=decreasing)
		x
	}
	# Function for selecting the first point of each list element, and generating zigzag grid by merging consecutive points:
	parallel2zigzag <- function(x){
		start <- data.table::rbindlist(lapply(x, utils::head, 1))
		tempSeq <- seq_len(nrow(start) - 1)
		transecind <- rep(tempSeq, each=2)
		ind <- c(outer(0:1, tempSeq, "+"))
		start <- start[ind,]
		start <- split(start, transecind)
		start
	}
	# Function for generating transects for one stratum:
	transectsOneStratum <- function(stratum, xy, area, type=c("parallel", "zigzag"), bearing="N", hours=240, knots=10, nmi=NULL, speed=NULL, seed=seed){
		# Get bearing of the stratum, and rotate into a cartesian coordinate system having x axis aloing this bearing:
		bearing <- getBearing(bearing)
		xyRotated <- rotate2d(xy[[stratum]], bearing, data.frame.out=TRUE)
		
		# Get the length of the stratum along the bearing:
		xmin <- min(xyRotated$x)
		xmax <- max(xyRotated$x)
		lengthOfStratum <- xmax - xmin
		# Get the total traveled length specified by nmi and knots
		if(length(speed)){
			knots <- speed * 3600/1852
		}
		if(length(nmi)==0){
			nmi <- hours * knots
		}
		
		# Subtract the length of the stratum, and return NULL if the traveled distance is shorter than this:
		if(nmi < lengthOfStratum){
			warning("The traveled distance specified by nmi or duration and knots is shorter than the length of the stratum")
		}
		nmi_rest <- nmi - lengthOfStratum
		
		# Get the random seed point for the transects:
		set.seed(seed[stratum])
		fac <- runif(1)
		
		getTransectsByArea <- function(nmi_rest, area, fac, xmin, xmax, xyRotated, bearing){
			# Get the number of transects
			transectSpacing <- area / nmi_rest
			firstTransectPos <- transectSpacing * fac
			# Get x positions of the transects
			xTransects <- seq(xmin - firstTransectPos, xmax + transectSpacing, by=transectSpacing)
		
			# Convert the polygon to a SpatialLines object for use in the intersection between polygon borders and transect lines:
		   	spatialLinesPolygon <- lapply(seq_len(nrow(xyRotated)-1), function(ind) sp::Lines(list(sp::Line(xyRotated[ind + 0:1, ])), ID = ind))
			spatialLinesPolygon <- sp::SpatialLines(spatialLinesPolygon)
		
			# Get lower and upper y value of the polygon:
			loweryPolygon <- min(xyRotated$y)
			upperyPolygon <- max(xyRotated$y)
			# Get the grid of lines which are used to find intersection points between transects and polygon borders:
			transects <- data.frame(
				x = rep(xTransects, each=2), 
				y = rep(c(loweryPolygon, upperyPolygon), length(xTransects)), 
				Ind = rep(seq_along(xTransects), each=2))
			transects <- split(transects, transects$Ind)
			###transects <- lapply(transects, data.matrix)
		
			if(type[1] == "ze"){
				# Order alternately: 
				transects <- orderAlternate(transects)
				# Select the first end point of each grid line, and generate zigzag grid by merging consecutive points:
				transects <- parallel2zigzag(transects)
			}
			spatialLinesTransect <- lapply(seq_along(transects), function(Ind) sp::Lines(list(sp::Line(data.matrix(transects[[Ind]][,1:2]))), ID=Ind))
			spatialLinesTransect <- sp::SpatialLines(spatialLinesTransect)
		
			# Get intersection points between the grid and the polygon borders:
			intersects <- rgeos::gIntersection(spatialLinesPolygon, spatialLinesTransect, byid=TRUE)
			
			# Split by x:
			transectID <- sapply(strsplit(rownames(intersects@coords), " "), utils::tail, 1)
			intersectsCoordsList <- split(as.data.frame(intersects@coords), transectID)
			intersectsCoordsList <- intersectsCoordsList[order(as.numeric(names(intersectsCoordsList)))]
		
			intersectsCoordsList <- orderTransectsByXY(intersectsCoordsList)
			decreasing <- diff(intersectsCoordsList[[1]][, "y"]) <= 0
			intersectsCoordsList <- orderAlternate(intersectsCoordsList, decreasing=decreasing)
		
			# Pick out the first two points, in order to keep only the inner borders:
			intersectsCoordsList <- lapply(intersectsCoordsList, utils::head, n=2)
		
			# For zigzag transects, set the end point of each transect to the start point of the next, and remove the last transect:
			if(type[1] == "z"){
				intersectsCoordsList <- parallel2zigzag(intersectsCoordsList)
	
				# Add end points to the zigzag transects:
				# We need a spatial object:
				temp <- intersects
				# Use the idcol="Transect" to split the transects afterwards:
				temp@coords <- data.matrix(data.table::rbindlist(intersectsCoordsList, idcol=FALSE))
				temp <- addEndTransects(temp, spatialLinesPolygon)
				# Split the transects into a list again:
				ind <- rep(seq_len(nrow(temp@coords)/2), each=2)
				intersectsCoordsList <- split(as.data.frame(temp@coords), ind)
			}
		
			# If 'dt' is given as a two element vector, the second element indicates populating the transects with points of distance dt[2]:
			if(length(dt)>1){
				for(i in seq_along(intersectsCoordsList)){
					intersectsCoordsList[[i]] <- populatePath(intersectsCoordsList[[i]], dt=dt[2])
				}
			}
	
			# Add a column to each transect denoting start and end points
			intersectsCoordsList <- lapply(intersectsCoordsList, function(z) cbind(z, StartEnd=c(rep(1, nrow(z)-1), 0)))
			# Combine to a matrix:
			coords <- data.matrix(data.table::rbindlist(intersectsCoordsList, idcol="Transect"))
			#plot(xyRotated, type="o")
			#lines(coords[,c("x", "y")], col=2, type="o")
			#abline(v=xTransects, col=4)
		
			# Get the new area:
			TrackLength <- sum(sqrt(rowSums(diff(coords[,c("x", "y")])^2)))
			return(list(coords=coords, TrackLength=TrackLength))
		}
		
		TrackLength <- 0
		margin <- nmi * 0.04
		last_rest <- Inf
		last_nmi_rest <- Inf
		
		while(abs(nmi - TrackLength) > margin){
			temp <- getTransectsByArea(nmi_rest=nmi_rest, area=area[[stratum]], fac=fac, xmin=xmin, xmax=xmax, xyRotated=xyRotated, bearing=bearing)
			TrackLength <- temp$TrackLength
			rest <- nmi - TrackLength
			#print(c(stratum, rest, nmi_rest, last_nmi_rest, TrackLength))
			
			if(abs(last_rest) < abs(rest)){
				temp <- getTransectsByArea(nmi_rest=last_nmi_rest, area=area[[stratum]], fac=fac, xmin=xmin, xmax=xmax, xyRotated=xyRotated, bearing=bearing)
				TrackLength <- temp$TrackLength
				warning(paste0("Sailed distance in stratum ", stratum, " did not converge to the desired sailed distance (",  nmi, " nmi). The closest used."))
				break
			}
			#nmi_rest <- nmi_rest + if(rest > 0) rest else 2 * rest
			last_nmi_rest <- nmi_rest
			nmi_rest <- nmi_rest + rest
			last_rest <- rest
		}
	
		coords <- temp$coords
		otherCols <- coords[, !colnames(coords) %in% c("x", "y")]
		coords <- coords[, colnames(coords) %in% c("x", "y")]
		# Rotate back:
		coords <- rotate2d(coords, -bearing)
		xy <- cbind(coords, as.data.frame(otherCols))
		
		# Convert back to (longitude, latitude):
		geo <- geo2xy(coords, par=proj, inv=TRUE, data.frame.out=TRUE)
		# Add non-coordinate columns:
		geo <- cbind(geo, as.data.frame(otherCols))
	
		return(list(xy=xy, lonlat=lonlat, TrackLength=TrackLength))
	}
	
	# Get the baseline output and number of strata:
	g <- getBaseline(projectName, input="proc", proc=NULL, drop=FALSE)
	nstrata <- nrow(g$processData$stratumpolygon)
	# Draw seeds for the transects:
	set.seed(seed)
	seed <- getSeedV(seed, nstrata)
	
	# Get the strata polygons in geographic coordinates (longitude, latitude) in a list named with the strata names:
	lonlat <- lapply(g$processData$stratumpolygon$Polygon, getMatrixList, data.frame.out=TRUE)
	names(lonlat) <- g$processData$stratumpolygon$Stratum
	lonlat <- lapply(lonlat, "colnames<-", c("Longitude", "Latitude"))
	
	# Create a single data frame version of the strata polygons, with stratum as the third column, and get a common projection definition using the centroid of the system:
	lonlatAll <- data.table::rbindlist(lonlat, idcol="Stratum")
	projList <- getProjString(proj="aeqd", x=lonlatAll[,c("Longitude", "Latitude")], list.out=TRUE)
	centroid <- data.frame(Longitude=projList$lon_0, Latitude=projList$lat_0)
	proj <- getProjString(proj="aeqd", x=lonlatAll[,c("Longitude", "Latitude")])
	
	# Get the stratum areas:
	area <- lapply(lonlat, polyArea)
	
	# Populate the stratum polygon borders with denser points, in order to preserve the geographic coordinate definition when converting to Cartesian coordinates (i.e., follow a latitude if two points are on the same latitude. If given only by two points, the azimuthal equal distance projection will follow the great circle, which in general will not coincide with the intended equal latitude path):
	lonlatPopulated <- lapply(lonlat, populatePath, dt=dt[1])
	
	# Convert to Cartesian:
	xy <- lapply(lonlatPopulated, geo2xy, data.frame.out=TRUE, par=proj)
	
	# Get transects for all strata:
	out <- lapply(seq_along(xy), transectsOneStratum, xy=xy, area=area, type=type, bearing=bearing, hours=hours, knots=knots, nmi=nmi, speed=speed, seed=seed)
	xyTransect <- lapply(out, "[[", "xy")
	xyTransectAll <- data.table::rbindlist(xyTransect, idcol="Stratum")
	lonlatTransect <- lapply(out, "[[", "coords")
	lonlatTransectAll <- data.table::rbindlist(lonlatTransect, idcol="Stratum")
	TrackLength <- sapply(out, "[[", "TrackLength")
	
	# Return a list of geographic coordinates for the stratum polygons and transects:
	list(
		lonlat=lonlat, lonlatAll=lonlatAll, 
		lonlatTransect=lonlatTransect, lonlatTransectAll=lonlatTransectAll, 
		xyTransect=xyTransect, xyTransectAll=xyTransectAll, 
		lonlatPopulated=lonlatPopulated, proj=proj, centroid=centroid, seed=seed, TrackLength=TrackLength)
}
#' 
#' @export
#' @rdname surveyPlanner
#' 
plotStratum <- function(stratum, transect=NULL, centroid=NULL, transport_alpha=0.1){
	library(rgdal)
	library(ggmap)
	library(ggplot2)
	
	if(length(centroid)==0){
		centroid <- data.frame(Longitude=mean(range(stratum$Longitude)), Latitude=mean(range(stratum$Latitude)))
	}
	#location <- colMeans(stratum[, c("Longitude", "Latitude")])
	
	gmap <- get_map(location=centroid, zoom=4, maptype="terrain", source="google", col="bw")
	
	#gmap <- map_data("world")
 
	
	p <- ggmap(gmap) + #ggplot(stratum, aes(x = Longitude, y = Latitude)) +
		geom_polygon(data=stratum, aes(x=Longitude, y=Latitude, fill=Stratum, group=Stratum), colour="black", alpha=0.3, inherit.aes=FALSE)# + 
		#geom_point(data=stratum, aes(x=Longitude, y=Latitude, fill=Stratum, group=Stratum), shape="*")
	
	#if(length(centroid)){
	#	p <- p + 
	#		geom_point(data=centroid, aes(x=Longitude, y=Latitude))
	#}
	#
	# Add stratum entry and exit points:
	if(length(transect)){
		transect$Type <- as.numeric(transect$Type)
		p <- p + 
			geom_path(data=transect, aes(x=Longitude, y=Latitude, group=Stratum, colour=Stratum, alpha=StartEnd), show.legend=FALSE) + 
			scale_alpha(range = c(transport_alpha, 1)) + 
			scale_colour_continuous(guide=FALSE)# + 
			#coord_map("azequidistant", orientation = c(centroid$Latitude, centroid$Longitude, 0))
	}
	# Run the plot
	print(p)
}
