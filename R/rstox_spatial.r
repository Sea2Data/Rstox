#*********************************************
#*********************************************
#' Polygon area
#'
#' Calculate area of wkt polygon with az.eq.are projection / adapted origo. The helper function wkt_MULTIPOLYGON() converts a two column matrix or a list of two column matrices to a wkt string.
#' 
#' @param wkt	Either one two column matrix of x and y coordinates, indicating only one polygon, or a list of such matrices, indicating several polygons. If a list of lists of two column matrices are given, tne first matrix of each list is the polygon, and the following are subtractions. \code{wkt} cal also be a wkt string such as "MULTIPOLYGON(((4 55, 15 56, 15 59, 4 59, 4 55)))".
#' 
#' @return \code{polyArea} returns area in nmi squared, and \code{wkt_MULTIPOLYGON} returns a MULTIPOLYGON wkt.
#'
#' @examples
#' library(rgeos)
#' p1 <- cbind(c(4, 15, 15, 4, 4), c(55, 56, 59, 59, 55))
#' s11 <- cbind(c(7, 12, 12, 7, 7), c(56, 57, 58, 58, 56))
#' s12 <- cbind(c(7, 14, 5, 7), c(56, 57, 58, 56))
#' p2 <- cbind(20 + c(4, 15, 15, 4, 4), c(55, 56, 59, 59, 55))
#' s2 <- cbind(20 + c(7, 12, 12, 7, 7), c(56, 56, 58, 58, 56))
#' wkt <- list(
#' 	list(p1, s11, s12),
#' 	list(p2, s2))
#' wkt_MULTIPOLYGON(wkt)
#' plot(readWKT(wkt_MULTIPOLYGON(wkt)), col='black', pbg='white')
#' polyArea(wkt)
#'
#' @export
#' @import rgdal
#' @importFrom sp CRS spTransform proj4string
#' @importFrom rgeos readWKT gArea
#' @rdname polyArea
#' 
polyArea <- function(wkt) {
	# We need rgdal when AreaMethod=Acurate in StratumArea!!!!
	###if(is.numeric(wkt)){
	###	wkt <- paste0("MULTIPOLYGON(((", paste(apply(wkt, 1, paste, collapse=" "), collapse=", "), ")))")
	###}
	wkt <- wkt_MULTIPOLYGON(wkt)
	p <- rgeos::readWKT(wkt)
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
wkt_MULTIPOLYGON <- function(wkt){
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
	if(is.character(wkt)){
		return(wkt)
	}
	if(!is.list(wkt)){
		if(length(dim(wkt))==2){
			wkt <- mergeToPairs(wkt)
		}
		else{
			stop("The input wkt must be a two column matrix or a list of two column matrices")
		}
	}
	else{
		wkt <- rapply(wkt, mergeToPairs, how="replace")
	}
	wkt <- lapply(wkt, addParantheseis)
	wkt <- addParantheseis(wkt)
	wkt <- paste0("MULTIPOLYGON", wkt)
	return(wkt)
}
