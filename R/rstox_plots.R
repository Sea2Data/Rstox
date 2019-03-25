#*********************************************
#*********************************************
#' Plot fish stations by species
#'
#' @param projectName   The name or full path of the project, a baseline object (as returned from \code{getBaseline} or \code{runBaseline}), og a project object (as returned from \code{openProject}). For \code{createProject}, \code{projectName}=NULL (the default) returns available templates, and for \code{openProject}, zeros length \code{projectName} returns all StoX projects in the default workspace either given as a vector of full paths, or, in the case projectName is an empty list, a list of names of StoX projects located in the default workspace and sub directories. Projects locataed in sub directories of the default workspace can be given by the relative path, or are searched for by name.
#' @param species		A vector of the species for which stations will be plotted, given in the current version as the norwegian name ('noname'). there can be several species for each station, which will lead to several points in the same location (see \code{jitter}).
#' @param proc			The process from which to get the fish stations (an object including the sting "FishStation").
#' @param size			The size of the points, equal for all points in the current version.
#' @param col,shape		The color/shape of the fish stations. If TRUE, use ggplot colors/shapes for each species.
#' @param alpha			Transparency in the range [0,1].
#' @param jitter		A vector of 2 or 3 elements, where the first two are the jitter scales in the x (longitude) and y (latitude) direction, respectively (), and the third element is the seed for the jittering. Use jittering with caution, since it shifts the position of the fish stations!
#' @param zoom,offset	Used for zooming the plot. A value of zoom = 1 indicates no zoom, values samller than 1 zooms in and values larger than one zooms out. The \code{offest} sets the center of the zoom relative to the limits of the original plot, where 0.5 (default) indicates the center of the plot.
#'
#' @return A ggplot
#'
#' @examples
#' projectName <- "Test_Rstox"
#' # Show the species sampled during the cruise:
#' g <- getBaseline(projectName, input=FALSE, proc="ReadBioticXML")
#' speciesFrequency <- sort(table(g$ReadBioticXML_BioticData_CatchSample.txt$noname), decreasing=TRUE)
#' species <- names(speciesFrequency)
#'
#' # Plot the 6 most frequent species:
#' plotFishStation(projectName, species=species[1:6], col=TRUE, size=2)
#' # Add jitter to the plot, resolving stations with multiple species. 
#' # CAUTION: This adds randmoness to the fish station positions!:
#' plotFishStation(projectName, species=species[1:6], col=TRUE, size=2, jitter=c(0.2, 0.2, 1))
#' 
#' @importFrom stats density
#' @export
#' @rdname plotFishStation
#' 
plotFishStation <- function(projectName, species="torsk", proc="FilterBiotic", size=1, col=1, shape=16, alpha=1, jitter=FALSE, zoom=1, offset=c(0.5, 0.5)){
	
	# Get all species:
	FishStation <- papply(species, getFishStationOneSpecies, projectName=projectName, proc=proc)
	FishStation <- data.table::rbindlist(FishStation)
	FishStation$species <- as.factor(FishStation$species)
	
	# Run and return the plot:
	plotLonLat(x=FishStation, lon="longitudestart", lat="latitudestart", species="species", size=size, zoom=zoom, offset=offset, col=col, shape=shape, alpha=alpha, jitter=jitter)
}
#'
#' @export
#' @import ggplot2
#' @rdname plotFishStation
#' 
plotLonLat <- function(x, lon="lon", lat="lat", species="species", size=1, zoom=1, offset=c(0.5, 0.5), col=1, shape=16, alpha=1, jitter=FALSE){
	# Get the map:
	gmap <- map_data("world")
	
	# Initiate the plot, with the map and zoom:
	p <- ggplot() + 
		geom_polygon(
			data=gmap, 
			aes_string(
				x="long", 
				y="lat", 
				group="group"
			)
		) + 
		zoom_lon_lat(
			x = x, 
			lon = lon, 
			lat = lat, 
			zoom = zoom, 
			offset = offset
		) + 
		guides(colour = guide_legend(override.aes = list(size=5)))
	
	# Apply jittering:	
	if(length(jitter) == 1 && !identical(jitter, FALSE)){
		jitter <- rep(jitter, length.out=2)
	}
	
	# Plot the fish stations:
	p <- p + geom_point(
		data = x, 
		aes_string(
			x = lon, 
			y = lat, 
			col = if(isTRUE(col)) species else shQuote(col), 
			shape = if(isTRUE(shape)) species else shape
		), 
		size = size, 
		alpha = alpha, 
		position = if(length(jitter) && !identical(jitter, FALSE)) position_jitter(width=jitter[1], height=jitter[2], seed=if(length(jitter) == 3) jitter[3] else NA) else "identity"
	)
	
	# If shape is given explicitely:
	if(!isTRUE(shape)){
		p <- p + scale_shape_identity()
	}
	
	p
}
#'
#' @export
#' @rdname plotFishStation
#' 
zoom_xlim_ylim <- function(xlim, ylim, zoom=1, offset=c(0.5, 0.5)){
	# Get the center of the plot, applying the offset:
	xcenter <- min(xlim) + offset[1] * diff(xlim)
	ycenter <- min(ylim) + offset[2] * diff(ylim)
	if(zoom <= 0){
		warning("The value of 'zoom' must be positive, with default 1 implying no zoom. The default was used.")
	}
	
	# Zoom:
	fact <- c(-1, 1) * zoom
	xlim <- xcenter + fact * diff(xlim) / 2
	ylim <- ycenter + fact * diff(ylim) / 2
	list(xlim=xlim, ylim=ylim)
}
#'
#' @export
#' @importFrom ggplot2 coord_fixed
#' @rdname plotFishStation
#' 
zoom_lon_lat <- function(x, lon="lon", lat="lat", zoom=1, offset=c(0.5, 0.5)){
	# Get xlim and ylim:
	xlim <- range(x[[lon]], na.rm=TRUE)
	ylim <- range(x[[lat]], na.rm=TRUE)
	
	# Adjust the aspect ratio by latitude:
	ymid <- mean(ylim)
	aspectratio <- 1 / cos(ymid * pi/180)
	
	# Apply the zoom_
	temp <- zoom_xlim_ylim(xlim, ylim, zoom, offset)
	xlim <- temp$xlim
	ylim <- temp$ylim
	
	# Return the coordinates as a ggplot object to add to a plot:
	coord_fixed(aspectratio, xlim=xlim, ylim=ylim)
}
#'
#' @export
#' @rdname plotFishStation
#' 
getFishStationOneSpecies <- function(species="torsk", projectName, proc="FilterBiotic", msg=FALSE){
	
	# Extract the requested species. This will be expanded to using TSN, aphia, or whatever is in the data in the Rstox framework:
	g <- getBaseline(projectName, FilterBiotic=list(BioticData="ReadBioticXML", FishStationExpr = paste0("fs.hasCatch(", shQuote(species), ")")), input=FALSE, proc=proc, endProcess=proc, drop=FALSE, msg=msg)

	# Get the element holding the fish stations:
	atFishStation <- grep("FishStation", names(g$outputData[[proc]]))
	FishStation <- data.table::as.data.table(g$outputData[[proc]][[atFishStation]])

	# Remove stations at (0,0):
	at00 <- FishStation$longitudestart == 0 & FishStation$latitudestart == 0
	FishStation <- FishStation[!at00, ]
	# Add the species as a column:
	FishStation <- cbind(FishStation, species=species)
	FishStation
}
