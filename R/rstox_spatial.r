#*********************************************
#*********************************************
#' Polygon area
#'
#' Calculate area of wkt polygon with az.eq.are projection / adapted origo. The helper function \code{matrix2multipolygon} converts a two column matrix or a list of two column matrices to a wkt string.
#' 
#' @param x					Either a two column matrix of x and y coordinates, indicating only one polygon, or a list of such matrices, indicating several polygons. If a list of lists of two column matrices are given, the first matrix of each list is the polygon, and the following are subtractions. \code{wkt} cal also be a wkt string such as "MULTIPOLYGON(((4 55, 15 56, 15 59, 4 59, 4 55)))".
#' @param multipolygon		A multipolygon string.
#' @param drop				Logical: If TRUE drop the list if only one multipolygon or only one polygon is given.
#' @param data.frame.out	Logical: If TRUE convert the matrices to data frames with columns x and y.
#' 
#' @return \code{polyArea} returns area in nmi squared, and \code{matrix2multipolygon} returns a MULTIPOLYGON wkt.
#'
#' @examples
#' library(rgeos)
#' p1 <- cbind(x=c(4, 15, 15, 4, 4), y=c(55, 56, 59, 59, 55))
#' s11 <- cbind(x=c(7, 12, 12, 7, 7), y=c(56, 57, 58, 58, 56))
#' s12 <- cbind(x=c(7, 14, 5, 7), y=c(56, 57, 58, 56))
#' p2 <- cbind(x=20 + c(4, 15, 15, 4, 4), y=c(55, 56, 59, 59, 55))
#' s2 <- cbind(x=20 + c(7, 12, 12, 7, 7), y=c(56, 56, 58, 58, 56))
#' x <- list(
#' 	list(p1, s11, s12),
#' 	list(p2, s2))
#' matrix2multipolygon(x)
#' multipolygon <- matrix2multipolygon(x)
#' plot(readWKT(multipolygon), col='black', pbg='white')
#' polyArea(x)
#' # Convert back to a list of matrices:
#' multipolygon2matrix(multipolygon)
#' # Output from multipolygon2matrix() are rotated counter clockwise:
#' y <- multipolygon2matrix(multipolygon)
#' all.equal(x, y)
#' x1 <- x[[1]][[1]]
#' y1 <- y[[1]][[1]]
#' orderCols12 <- function(x) x[order(x[,1], x[,2]), ]
#' all.equal(orderCols12(x1), orderCols12(y1))
#'
#' @export
#' @import rgdal
#' @importFrom sp CRS spTransform proj4string
#' @importFrom rgeos readWKT gArea
#' @rdname polyArea
#' 
polyArea <- function(x) {
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
#' @rdname polyArea
#' 
matrix2multipolygon <- function(x){
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
	if(is.character(x)){
		return(x)
	}
	if(!is.list(x)){
		if(length(dim(x))==2){
			x <- mergeToPairs(x)
		}
		else{
			stop("The input x must be a two column matrix or a list of two column matrices")
		}
	}
	else{
		x <- rapply(x, mergeToPairs, how="replace")
	}
	x <- lapply(x, addParantheseis)
	x <- addParantheseis(x)
	x <- paste0("MULTIPOLYGON", x)
	return(x)
}
#' 
#' @export
#' @importFrom sp disaggregate
#' @importFrom rgeos readWKT
#' @importFrom methods slot
#' @rdname polyArea
#' 
multipolygon2matrix <- function(multipolygon, drop=TRUE, data.frame.out=FALSE){
	# Function for extracting the coordinates of one multipolygon:
	getCoordsOne <- function(y, data.frame.out){
		out <- lapply(y@Polygons, slot, "coords")
		if(data.frame.out){
			out <- lapply(out, as.data.frame)
		}
		out
	}
	# Convert to a wkt object organized in a list with one element per multipolygon, each holding one element per polygon:
	out <- rgeos::readWKT(matrix2multipolygon(multipolygon))
	out <- disaggregate(out)
	# Extract the coordinates:
	out <- lapply(out@polygons, getCoordsOne, data.frame.out=data.frame.out)
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
