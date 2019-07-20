#*********************************************
#*********************************************
#' Plot fish stations by species
#'
#' This function plots the fish stations of the biotic data of a StoX project. The stations can be plotted with colors indicating the species specified by the parameter \code{species}. For stations with more than one of these species present, the last is plotted last, and this will overlay the other species in the station. To avoid this, there is an option \code{jitter}.
#' 
#' These are the details
#' \deqn{\sum_{i} \hat{f} (x_i) \frac{b_{i+1}} {b_i}  = 1}
#' End of the details
#' 
#' @param projectName   The name or full path of the project, a baseline object (as returned from \code{getBaseline} or \code{runBaseline}), og a project object (as returned from \code{openProject}). For \code{createProject}, \code{projectName}=NULL (the default) returns available templates, and for \code{openProject}, zeros length \code{projectName} returns all StoX projects in the default workspace either given as a vector of full paths, or, in the case projectName is an empty list, a list of names of StoX projects located in the default workspace and sub directories. Projects locataed in sub directories of the default workspace can be given by the relative path, or are searched for by name.
#' @param species		A vector of the species for which stations will be plotted, given in the current version as the norwegian name ('noname'). there can be several species for each station, which will lead to several points in the same location (see \code{jitter}).
#' @param proc			The process from which to get the fish stations (an object including the sting "FishStation").
#' @param type			The type of the points/lines, one of "p" (points), "l" (lines) and "o" (both lines and points).
#' @param size			The size of the points, equal for all points in the current version. For type = "o", a vector of two element sspecifies the line and point size, respectively.
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
#' speciesFrequency <- sort(table(g$ReadBioticXML_BioticData_catchsample.txt$commonname), decreasing=TRUE)
#' species <- names(speciesFrequency)
#'
#' # Plot the 6 most frequent species:
#' plotFishStation(projectName, commonname=species[1:6], col=TRUE, size=2)
#' # Add jitter to the plot, resolving stations with multiple species. 
#' # CAUTION: This adds randmoness to the fish station positions!:
#' plotFishStation(projectName, commonname=species[1:6], col=TRUE, size=2, jitter=c(0.2, 0.2, 1))
#' 
#' @importFrom stats density
#' @export
#' @rdname plotFishStation
#' 
plotFishStation <- function(projectName, commonname="torsk", proc="FilterBiotic", size=1, col=TRUE, shape=16, alpha=1, jitter=FALSE, zoom=1, offset=c(0.5, 0.5), type="p", add0=FALSE, subset=NULL, legendPos=c(0.05, 0.05), unit="tonnes"){
	
	# Read the data:
	g <- getBaseline(projectName, input=FALSE, proc="ReadBioticXML", endProcess="ReadBioticXML", drop=TRUE)
	
	# If empty or numeric get all species, and possibly subset by the integer:
	if(length(commonname) == 0 || is.numeric(commonname)){
		temp <- table(g$ReadBioticXML_BioticData_catchsample.txt$commonname)
		temp <- temp[order(temp, decreasing=TRUE)]
		if(is.numeric(commonname)){
			commonname <- names(temp)[seq_len(commonname)]
		}
		else{
			commonname <- names(temp)
		}
	}
	
	# Merge fishstation and catchsample:
	FishStation <- g[[grep("fishStation", names(g), ignore.case=TRUE)]]
	Catchsample <- g[[grep("catchsample", names(g), ignore.case=TRUE)]]
	FishStationCatchsample <- merge(FishStation, Catchsample, all=TRUE)
	
	if(length(subset)) {
		FishStationCatchsample <- subset(FishStationCatchsample, eval(parse(text = subset)))
	}
	
	# Pick out only the selected species:
	valid <- FishStationCatchsample$commonname %in% commonname
	out <- subset(FishStationCatchsample, valid)
	
	# Add zero catches:
	if(add0) {
		allFishStations <- expand.grid(unique(FishStationCatchsample$serialnumber), commonname)
		outFull <- as.data.frame(array(0, dim=c(nrow(allFishStations), ncol(FishStationCatchsample))))
		names(outFull) <- names(FishStationCatchsample)
		outFull[, c("serialnumber", "commonname")] <- allFishStations
		insertPos <- match(
			apply(out[, c("serialnumber", "commonname")],     1, paste, collapse="/"), 
			apply(outFull[, c("serialnumber", "commonname")], 1, paste, collapse="/")
		)
		outFull[insertPos, ] <- out
		
		out <- outFull
	}
	
	# Run and return the plot:
	plotLonLat(x=out, lon="longitudestart", lat="latitudestart", species="commonname", type=type, size=size, zoom=zoom, offset=offset, col=col, shape=shape, alpha=alpha, jitter=jitter, legendPos=legendPos, unit=unit)
}
#'
#' @export
#' @import ggplot2
#' @rdname plotFishStation
#' 
plotLonLat <- function(x, lon="lon", lat="lat", species="species", type="p", size=1, zoom=1, offset=c(0.5, 0.5), col=TRUE, shape=16, alpha=1, jitter=FALSE, maxPieRadius=1, legendPos=c(0.05, 0.05), unit="tonnes"){
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
	
	#### Plot the fish stations: ####
	if(type == "pie") {
		# Create a data frame with the species in the columns:
		
		
		catch2speciesMatrix <- function(x, specvar="commonname", groupvar="serialnumber", valuevar="catchweight", var=NULL, sumCol="SumValue") {
			# Get all groups and species:
			atGroups <- which(!duplicated(x[[groupvar]]))
			groups <- x[[groupvar]][atGroups]
			species <- unique(x[[specvar]])
			species <- species[!is.na(species)]
			
			# Create a data.table of groups and species:
			out <- array(NA, dim=c(length(groups), length(species)))
			rownames(out) <- groups
			colnames(out) <- species
			
			# Insert the values:
			ind <- cbind(x[[groupvar]], x[[specvar]])
			valid <- rowSums(is.na(ind)) == 0
			out[ind[valid, , drop=TRUE]] <- x[[valuevar]][valid]
			out <- as.data.frame(out)
			
			if(length(sumCol)) {
				temp <- data.frame(rowSums(out, na.rm=TRUE))
				names(temp) <- sumCol
				out <- cbind(out, temp)
			}
			
			
			# Add the specified variables:
			if(length(var)) {
				out <- cbind(x[atGroups, c(groupvar, var), drop=FALSE], out)
			}
			# Add the species column names as an attribute:
			attr(out, "SpeciesCols") <- species
			
			# Retrun:
			out
		}
		
		library(scatterpie)
		
		PieData <- catch2speciesMatrix(x, var=c("longitudestart", "latitudestart"), sumCol="SumValue")
		# Scale the total catch to the 'maxPieRadius', using the square root of the total catch to correspond to the pie radius:
		unit <- getPlottingUnit(unit, var="weight", baseunit="kg")
		maxCatchTonnes <- max(PieData$SumValue) / unit$scale
		PieData$PieRadius <- sqrt(PieData$SumValue)
		PieData$PieRadius <- PieData$PieRadius / max(PieData$PieRadius) * maxPieRadius
		# The geom_scatterpie_legend() does not handle NAs:
		PieData[is.na(PieData)] <- 0
		
		
		#legendPos <- sapply(p$coordinates$limits, function(x) min(x) + diff(x) * legendMargin)
		
		minLim <- sapply(p$coordinates$limits, min)
		diffLim <- sapply(p$coordinates$limits, diff)
		
		
		legendPos <- minLim + diffLim * legendPos
		
		p <- p + 
			scatterpie::geom_scatterpie(
				aes_string(
					x = lon, 
					y = lat, 
					group = "serialnumber", 
					r = "PieRadius"),
				data = PieData, 
				cols = attr(PieData, "SpeciesCols"), 
				alpha = alpha
			) +
			scatterpie::geom_scatterpie_legend(PieData$PieRadius, x=legendPos[1], y=legendPos[2], labeller = function(x) paste(round(x^2 * maxCatchTonnes, digits=1), unit$unit))
	}
	
	else if(type == "p") {
		plotspec <- list(
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
	
		if(startsWith(type, "p")){
			p <- p + do.call(geom_point, plotspec)
		}
		else if(startsWith(type, "l")){
			p <- p + do.call(geom_path, plotspec)
		}
		else if(startsWith(type, "o")){
			if(length(size) == 1){
				size <- rep(size, 2)
			}
			plotspec$size <- size[1]
			p <- p + do.call(geom_path, plotspec)
			plotspec$size <- size[2]
			p <- p + do.call(geom_point, plotspec)
		}
		else{
			warnings("Input 'type' not recognized. Defaulted to points.")
			p <- p + do.call(geom_point, plotspec)
		}
	}
	
	
	#p <- p + geom_point(
	#	data = x, 
	#	aes_string(
	#		x = lon, 
	#		y = lat, 
	#		col = if(isTRUE(col)) species else shQuote(col), 
	#		shape = if(isTRUE(shape)) species else shape
	#	), 
	#	size = size, 
	#	alpha = alpha, 
	#	position = if(length(jitter) && !identical(jitter, FALSE)) position_jitter(width=jitter[1], height=jitter[2], seed=if(length(jitter) == 3) jitter[3] else NA) else "identity"
	#)
	
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
zoom_lon_lat <- function(x, lon="lon", lat="lat", xlim=NA, ylim=NA, zoom=1, offset=c(0.5, 0.5)){
	# If xlim or ylim is not present:
	if(length(xlim) && !is.na(xlim) && length(ylim) && !is.na(ylim)){
		aspectratio <- 1 / cos(mean(ylim) * pi/180)
	}
	else{
		# Get xlim and ylim:
		xlim <- range(x[[lon]], na.rm=TRUE)
		ylim <- range(x[[lat]], na.rm=TRUE)
	
		# Adjust the aspect ratio by latitude:
		aspectratio <- 1 / cos(mean(ylim) * pi/180)
	
		# Apply the zoom_
		temp <- zoom_xlim_ylim(xlim, ylim, zoom, offset)
		xlim <- temp$xlim
		ylim <- temp$ylim	
	}
	
	# Return the coordinates as a ggplot object to add to a plot:
	coord_fixed(aspectratio, xlim=xlim, ylim=ylim)
}
#'
#' @export
#' @rdname plotFishStation
#' 
getFishStationOneSpecies <- function(species=NULL, projectName, proc="FilterBiotic", msg=FALSE){
	# !!!!!!!!!!!! Here we should merge the fishstation and catchsample instead: !!!!!!!!!!!!!!!!!!!!!!
	
	# Extract the requested species. This will be expanded to using TSN, aphia, or whatever is in the data in the Rstox framework:
	#g <- getBaseline(projectName, FilterBiotic=list(BioticData="ReadBioticXML", FishStationExpr = paste0("fs.hasCatch(", shQuote(species), ")")), input=FALSE, proc=proc, endProcess=proc, drop=FALSE, msg=msg)
	g <- getBaseline(projectName, FilterBiotic=list(BioticData="ReadBioticXML", FishStationExpr = paste0("fs.hasCatch(", shQuote(species), ")")), input=FALSE, endProcess=proc, drop=FALSE, msg=msg)
	
	# Get the element holding the fish stations:
	atFishStation <- grep("fishStation", names(g$outputData[[proc]]), ignore.case=TRUE)
	FishStation <- data.table::as.data.table(g$outputData[[proc]][[atFishStation]])

	# Remove stations at (0,0):
	at00 <- FishStation$longitudestart == 0 & FishStation$latitudestart == 0
	FishStation <- FishStation[!at00, ]
	# Add the species as a column:
	FishStation <- cbind(FishStation, species=species)
	FishStation
}
