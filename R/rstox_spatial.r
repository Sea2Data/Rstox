#*********************************************
#*********************************************
#' Polygon area
#'
#' Calculate area of wkt polygon with az.eq.are projection / adapted origo. The helper function \code{matrix2multipolygon} converts a two column matrix or a list of two column matrices to a wkt string.
#' 
#' @param wkt	Either one two column matrix of x and y coordinates, indicating only one polygon, or a list of such matrices, indicating several polygons. If a list of lists of two column matrices are given, the first matrix of each list is the polygon, and the following are subtractions. \code{wkt} cal also be a wkt string such as "MULTIPOLYGON(((4 55, 15 56, 15 59, 4 59, 4 55)))".
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
polyArea <- function(wkt) {
	# We need rgdal when AreaMethod=Acurate in StratumArea!!!!
	###if(is.numeric(wkt)){
	###	wkt <- paste0("MULTIPOLYGON(((", paste(apply(wkt, 1, paste, collapse=" "), collapse=", "), ")))")
	###}
	wkt <- matrix2multipolygon(wkt)
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
#' @rdname polyArea
#' 
multipolygon2matrix <- function(multipolygon, drop=TRUE){
	# Function for extracting the coordinates of one multipolygon:
	getCoordsOne <- function(y){
		as.data.frame(lapply(y@Polygons, slot, "coords")	)
	}
	# Convert to a wkt object organized in a list with one element per multipolygon, each holding one element per polygon:
	out <- rgeos::readWKT(matrix2multipolygon(multipolygon))
	out <- disaggregate(out)
	# Extract the coordinates:
	out <- lapply(out@polygons, getCoordsOne)
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




parallelTransects <- function(projectName, bearing="N", t=100, vel=10, nsim=50, rev.entrance=F) {
	library(splancs)
	library(sp)
	library(geosphere)
	
	g <- getBaseline(projectName, input="proc", proc=NULL, drop=FALSE)
	nstrata <- nrow(g$processData$stratumpolygon)
	lonlat <- vector("list", nstrata)
	for(i in seq_along(lonlat)){
		lonlat[[i]] <- cbind(multipolygon2matrix(g$processData$stratumpolygon$Polygon[i]), Stratum=g$processData$stratumpolygon$Stratum[i])
	}
	#r$Stratum <- factor(r$Stratum)
	lonlat <- do.call("rbind", lonlat)
	names(lonlat) <- c("Longitude", "Latitude", "Stratum")
	lonlat$Stratum <- factor(lonlat$Stratum)
	
	# Accept only North or East for bearing (this sohuld be replaced by a general angle, which will make 'rev.entrance') obsolete):
	getLatlon12 <- function(x, bearing){
		if(bearing == "N"){
			c(mean(x[,1]), min(x[,2]), mean(x[,1]), max(x[,2]), head(x[,3], 1))
		}
		else if(bearing == "E"){
			c(min(x[,1]), mean(x[,2]), max(x[,1]), mean(x[,2]), head(x[,3], 1))
		}
		else{
			stop("Invaid bearing (must be one of 'N' or 'E')")
		}
	}
	
	uStratum <- unique(lonlat$Stratum)
	lonlat12 <- by(lonlat, lonlat$Stratum, getLatlon12, bearing=bearing)
	lonlat12 <- do.call("rbind", lonlat12)
	

	if (nsim == 1) {
		nsim == 0
		}
		
	
		tra <- vector("list", 1000)
		for(i in seq_along(tra)){
			print(i)
			tra[[i]] <- parallelTransectOneStratum(1, lonlat=lonlat, lonlat12=lonlat12, bearing=bearing, t=t, vel=vel, nsim=nsim, rev.entrance=rev.entrance)
		}
	
	
	
	transect <- lapply(uStratum, parallelTransectOneStratum, lonlat=lonlat, lonlat12=lonlat12, bearing=bearing, t=t, vel=vel, nsim=nsim, rev.entrance=rev.entrance)
	list(lonlat=lonlat, lonlat12=lonlat12, transect=transect)
	
}


parallelTransectOneStratum <- function(stratum, lonlat, lonlat12, bearing="N", t, vel, nsim=50, rev.entrance=F){
		
	name.field <- lonlat$Stratum[which(lonlat[,3] == stratum)[1]]

	# EXTRACTION AND FORMATTING OF POLYGON NODES AND ENTRANCE AND EXIT POINTS
	lbno <- lonlat[which(lonlat[,3] == stratum),1:2]			# picks polygon nodes for field no
	lon <- lbno[,1]											# polygon node decimal longitudes
	lat <- lbno[,2]											# polygon node decimal latitudes
	lb12 <- lonlat12[which(lonlat12[,5] == stratum),1:4]	# picks entrance and exit points for field no
	if(rev.entrance==T) lb12 <- lonlat12[which(lonlat12[,5] == stratum),c(3,4,1,2)] # reverse entrance and exit
	lb12 <- as.numeric(lb12)								# converts to numeric (lb12 is one line)
	lon1 <- lb12[1]				# decimal longitude of entrance point
	lat1 <- lb12[2]				# decimal latitude of entrance point
	lon2 <- lb12[3]				# decimal longitude of exit point
	lat2 <- lb12[4]				# decimal latitude of exit point

	n <- length(lon);			# number of nodes in polygon 
	lon <- matrix(lon,n,1)		# ascertains lon to be column vector
	lat <- matrix(lat,n,1)		# ascertains lat to be column vector
	# ascertains lon(1) = lon(end) and lat(1) = lat(end) if the difference of entrance and exit points
	# exceed 1e-6 deg for either lon-values, lat-values or both
	if (abs(lon[1]-lon[n]) > 1e-6 | abs(lat[1]-lat[n]) > 1e-6)	# TRUE if difference exceeds threshold
		{
	    lon <- c(lon, lon[1]);	# adds a new last lon-value identical to first one
	    lat <- c(lat, lat[1]);	# adds a new last lat-value identical to first one
	    n <- n + 1;				# adjusts n to define the length of lon and lat
	    }
	lon <- matrix(lon,n,1)		# ascertains lon to be a vertical vector
	lat <- matrix(lat,n,1)		# ascertains lat to be a vertical vector

	inndata <- cbind(lon,lat,lon1,lat1,lon2,lat2)
	#	browser()
		y <- subconvex(inndata)
		uc <- y$uc
		vc <- y$vc
	#	vel <- inndata[1,7]
	#	t <- inndata[1,8]
		c <- ccalc(uc,vc,vel,t)
	#	fac <- inndata[1,9]
	#	if (fac == 0) {
		#if (nsim == 1) {
		#	plot(lon,lat,type='l')
		#	}
		for (jsim in 1:max(1,nsim)) {
			fac <- runif(1)
	#		}

		ur1 <- fac*c
		urnd <- seq(min(uc)+ur1, max(uc),by = c)
		nurnd <- length(urnd)
		uupp <- y$uupp
		ulow <- y$ulow
		vupp <- y$vupp
		vlow <- y$vlow
		q <- subyz(urnd, nurnd, uupp, ulow, vupp, vlow)
		yzu <- q$yz[,1]
		yzd <- q$yz[,2]
		A <- y$A
		c0 <- y$c0
		lon0 <- y$lon0
		lat0 <- y$lat0
		zig <- subuplow(urnd,nurnd,uupp,vupp,ulow,vlow,yzu,yzd,A,c0,lon0,lat0)
		#lines(zig$ziglon1,zig$ziglat1,col = 'red')
		}
		zig <- subuplow(urnd,nurnd,uupp,vupp,ulow,vlow,yzu,yzd,A,c0,lon0,lat0)
                       
		lonrnd <- zig$ziglon1
		latrnd <- zig$ziglat1
		lonlatrnd <- data.frame(lon=lonrnd, lat=latrnd)
  
	  dist1 <- 0
	  for(i in 1:(length(lonrnd)-1)){
	    dist1 <- dist1 + (spDistsN1(cbind(lonrnd,latrnd),cbind(lonrnd,latrnd)[i,], longlat=T)[i+1])
	    }
	  dist1 <- dist1/1.852  

	  #dist1 <- spDistsN1(cbind(lonrnd,latrnd),cbind(lonrnd,latrnd)[1,], longlat=T)[i+1]
	  out <- list(name.field=name.field,
	        cruiseline = as.data.frame(cbind(lonrnd,latrnd)), 
	#        Area = A,
	        Distance = dist1
	#        Sur.cov = dist1/ sqrt(A)
	        ) 

	  ## Create file to MaxSea
	  ## Create ascii fil til import i MaxSea (se manual for code)
	  item.type <- 257  # Linje
	  item.id <- 20      # Setter id-coder på linjene
	  item.col <- 1     # Blå
	  north <- "N"
	  west <- "E"


	  ##
	  
	  filnavn.ut <- file.path(getProjectPaths(projectName)$RReportDir, paste0(name.field, ".asc"))
	  #filnavn.ut <- paste("../Kartdata/rutelinjer/",name.field,".asc",sep="")
	  ant.dec.pos <- 3
	  n.row <- nrow(out$cruiseline)
                      
	  out1 <- cbind(rep(item.type,n.row),
	            rep(item.id,n.row),
	            rep(item.col,n.row),
	            round(out$cruiseline$lat,ant.dec.pos),
	            rep(north,n.row),
	            round(out$cruiseline$lonrnd,ant.dec.pos),
	            rep(west,n.row))
				
	  write.table(out1,file=filnavn.ut,row.names=F, col.names=F,sep=",",quote=F)

	  polyg <- cbind(lon,lat)
	  polyg.sp = SpatialPolygons(list(Polygons(list(Polygon(polyg)), "x")))
	  #plot(polyg.sp, main=name.field)
	  #plot(lon,lat,type = 'l',main=name.field)
	  #points(c(lon1,lon2),c(lat1,lat2),col=2)
	  #text(c(lon1,lon2),c(lat1,lat2),labels=1:2,col=2)
	  #lines(lonlatrnd, col="red")
	  #points(spsample(polyg.sp, n = 10, "random"), pch = 2, col=2)
	  #spsample(polyg.sp, n = 10, "random")

		#out <- list(lon=lon,lat=lat,zig)
		#track.description <- paste("../Kartdata/rutelinjer/",name.field,".asc",sep="")
	
	tmp <- as.data.frame(cbind(floor(out$cr$latrnd), round(60*(out$cr$latrnd- floor(out$cr$latrnd)),2),
	                           floor(out$cr$lonrnd), round(60*(out$cr$lonrnd- floor(out$cr$lonrnd)),2)))
	names(tmp) <- c("LatDeg","LatMin","LonDeg","LonMin")

	  name.field
	  vel # Speed
	  dist1 ## Sailing distance
	  area.nm2 <- areaPolygon(polyg)/1852/1852 # Meters2
	  Sur.cov <- dist1/ sqrt(area.nm2)
	  #print(tmp)  
	  ## TABLE
	  # filnavn1 <- paste("../Kartdata/rutelinjer/INFO",name.field,".txt",sep="")
	  filnavn1 <- file.path(getProjectPaths(projectName)$RReportDir, paste0("INFO", name.field, ".txt"))
	  capture.output( cat("\n",format(c(date()),width=20, justify = "left")), file=filnavn1)
	  capture.output( cat("\n", "Stratum (Toktområde)    ", format(name.field,width=7, justify ="right")), file=filnavn1, append=T)
	  capture.output( cat("\n", "Speed and time available", format(c(paste(vel,"knots"),paste(t, "h")),width=7, justify ="right")), file=filnavn1, append=T)
	  capture.output( cat("\n", "Stratum area (n.mi2)    ", format(area.nm2,width=7, justify ="right")), file=filnavn1, append=T)
	  capture.output( cat("\n", "Sailing distance (n.mi) ", format(dist1,width=7, justify ="right")), file=filnavn1, append=T)
	  capture.output( cat("\n", "Survey coverage         ", format(Sur.cov,width=7, justify ="right")), file=filnavn1, append=T)
	  capture.output( cat("\n", " "), file=filnavn1, append=T)
	  capture.output( cat("\n", "Transect positions      "), file=filnavn1, append=T)
	  capture.output( cat("\n", " "), file=filnavn1, append=T)
	  capture.output(cat("\n", " "), tmp, file= filnavn1, append=T)  
	  #filnavn2 <- paste("../Kartdata/rutelinjer/TRACK",name.field,".txt",sep="")
	  filnavn2 <- file.path(getProjectPaths(projectName)$RReportDir, paste0("TRACK", name.field, ".txt"))
	  out2 <- as.data.frame(cbind(1,out$cr))
	  names(out2) <- c("Line","Longitude", "Latitude")
	  write.csv(out2, file=filnavn2,row.names=F)
	  
	  lonlatrnd
}


subconvex <- function(x) {
	lon <- x[,1]
	lat <- x[,2]
	lon1 <- x[1,3]
	lat1 <- x[1,4]
	lon2 <- x[1,5]
	lat2 <- x[1,6]
	lonc <- lon
	latc <- lat
	n = length(lonc)
	if (lonc[1] == lonc[n] & latc[1] == latc[n]) {
		lonc = lonc[2:n];
		latc = latc[2:n]
		}
	j <- which(diff(lonc) == 0 & diff(latc) == 0)
	nj <- length(j)
	if (nj > 0){
		ind = setdiff(1:length(lonc), j+1);
		lonc = lonc(ind);
		latc = latc(ind)
		}
	lon0 <- mean(lonc)
	lat0 <- mean(latc)
	c0 <- cos(lat0/180*pi);
	x1 <- (lon1-lon0)*60*c0
	y1 <- (lat1-lat0)*60
	x2 <- (lon2-lon0)*60*c0
	y2 <- (lat2-lat0)*60
	tet <- atan2(y2-y1,x2-x1)
	A = matrix(cbind(cos(tet), sin(tet), -sin(tet), cos(tet)),2,2,byrow = T)
	#out <- cbind(x1,y1)
	xc <- (lonc-lon0)*60*c0
	yc <- (latc-lat0)*60;
	uv <- A%*%t(cbind(xc,yc))
	uc <- uv[1,]
	vc <- uv[2,]
	n <- length(uc)
	jmin <- which(uc == min(uc))
	jmax <- which(uc == max(uc))
	if (length(jmin) == 2){
		j1 <- min(jmin);
		j2 <- max(jmin)
		if (j2 - j1 == 1) {
			uc <- t(c(uc[1:j1], uc[j1], uc[j2:n]));
			uc <- t(uc);
			vc <- t(c(vc[1:j1], (vc[j1]+vc[j2])/2, vc[j2:n]));
			vc <- t(vc);
			jmin <- j1[1] + 1
		}
		if (j2 - j1 > 1) {
			uc <- t(c(uc[j1], uc));
			uc <- t(uc);
			vc <- t(c((vc[j1]+vc[j2])/2, vc));
			vc <- t(vc);
			jmin <- 1
		}
	}
	if (length(jmax) == 2) {
		n <- length(uc);
		jmax <- which(uc == max(uc));
		j1 <- min(jmax);
		j2 <- max(jmax);
		if (j2 - j1 == 1){
			uc <- t(c(uc[1:j1], uc[j1], uc[j2:n]));
			uc <- t(uc);
			vc <- t(c(vc[1:j1], (vc[j1]+vc[j2])/2, vc[j2:n]));
			vc <- t(vc);
			jmax <- j1 + 1
		}
		if (j2 - j1 > 1) {
			uc <- t(c(uc[j1], uc));
			uc <- t(uc);
			vc <- t(c((vc[j1]+vc[j2])/2, vc));
			vc <- t(vc);
			jmax <- 1
		}
	}
	n <- length(uc)
	if (jmax > jmin){
		ind1 <- seq(jmin,jmax,1);
		ind2 <- c(seq(jmax,n,1), 1:jmin);
		nind2 <- length(ind2);
		ind2 <- ind2[nind2:1];
#		ind2 <- ind2[order(ind2, decreasing = T)]
	}
	if (jmax < jmin) {
		ind1 <- c(seq(jmin,n,1), 1:jmax);
#		ind2 <- jmax:jmin;
		ind2 <- jmin:jmax;
#		ind2 <- ind2[order(ind2,decreasing = T)]
	}
	if (mean(vc[ind1]) < mean(vc[ind2])){
		indupp <- ind2;
		indlow <- ind1
	}
	if (mean(vc[ind1]) > mean(vc[ind2])){
		indupp <- ind1;
		indlow <- ind2
	}
	dupp <- diff(uc[indupp])
	jupp <- which(dupp < 0)
	nupp <- length(jupp)
	if (nupp > 0){
		for (j in 1:nupp){
			j1 <- indupp[jupp[j]]
			j2 <- indupp[jupp[j]+1]
			if (vc[j1] < vc[j2]) {
				uc[j1] <- uc[j2]
			}
			if (vc[j1] > vc[j2]) {
				uc[j2] <- uc[j1]
			}
		}
	}
	dlow <- diff(uc[indlow])
	jlow <- which(dlow < 0)
	nlow <- length(jlow)
	if (nlow > 0){
		for (j in 1:nlow){
			j1 <- indlow[jlow[j]]
			j2 <- indlow[jlow[j] + 1]
			if (vc[j1] < vc[j2]) {
				uc[j1] <- uc[j2]
			}
			if (vc[j1] > vc[j2]) {
				uc[j2] <- uc[j1]
			}
		}
	}
	uupp <- uc[indupp]
	vupp <- vc[indupp]
	ulow <- uc[indlow]
	vlow <- vc[indlow]
	lonupp <- lonc[indupp]
	latupp <- latc[indupp]
	lonlow <- lonc[indlow]
	latlow <- latc[indlow]
	vm <- mean(vupp) - mean(vlow)
	out <- list(uc = uc, 
	vc = vc, 
	uupp = uupp,
	vupp = vupp,
	ulow = ulow,
	vlow =vlow,
	lonc = lonc,
	latc = latc,
	lon0 = lon0,
	lat0 = lat0,
	c0 = c0,
	vm = vm,
	A = A)
}

ccalc <- function(uc,vc,vel,t) {
	poly <- cbind(uc,vc)
	area <- areapl(poly)
	umin <- min(uc)
	umax <- max(uc)
	L <- vel*t
	du <- umax - umin
	c <- area/(vel*t - du)
	if (c < 0) {
		c <- 0.5*du
		}
		out <- c
}

subyz <- function(urnd, nurnd, uupp, ulow, vupp, vlow) {
	yzu <- matrix(0,nurnd,1)
	yzd <- yzu
	for (j in 1:nurnd) {  # loop to identify (u,v) positions of nodes
    ju <- which(urnd[j]-uupp < 0);  # to identify actual upper polygon segment
    ju1 <- ju[1]-1;  # index to first point of upper polygon segment with node j
    ju2 <- ju1 + 1;  # index to second point of upperpolygon segment with node j
    yzu[j] <- vupp[ju1] + (urnd[j]-uupp[ju1])/(uupp[ju2]-uupp[ju1])*(vupp[ju2]-vupp[ju1]);
    # yzu = y-values of nodes in upper polygon
    jd <- which(urnd[j]-ulow < 0); # to identify actual lower polygon segment
    jd1 <- jd[1]-1;  # index to first point of lower polygon segment with node j
    jd2 <- jd1+1;  # index to second point of lower polygon segment with node j
    yzd[j] <- vlow[jd1] + (urnd[j]-ulow[jd1])/(ulow[jd2]-ulow[jd1])*(vlow[jd2]-vlow[jd1]);
    }
    # yzd = y-values of nodes in lower polygondasfasdf
    yz <- cbind(yzu,yzd)
    out <- list(yz = yz)
}

subuplow <- function(urnd,nurnd,uupp,vupp,ulow,vlow,yzu,yzd,A,c0,lon0,lat0) {
	#zigu1 <- NULL
	#zigv1 <- NULL
	#zigu2 <- NULL
	#zigv2 <- NULL
	for (j in 1:nurnd-1) {
		jodd <- j%%2
		ku <- which(uupp > urnd[j] & uupp  < urnd[j + 1])
		kd <- which(ulow > urnd[j] & ulow < urnd[j + 1])
		if (length(ku) > 0) {
			du1 <- c(urnd[j], uupp[ku], urnd[j+1]);
			dv1 <- c(yzu[j], vupp[ku], yzu[j+1])
		}
		if (length(ku) == 0) {
			du1 <- c(urnd[j], urnd[j+1]);
			dv1 <- c(yzu[j], yzu[j+1]	)
		}
		if (length(kd) > 0) {
			du2 <- c(urnd[j], ulow[kd], urnd[j+1]);
			dv2 <- c(yzd[j], vlow[kd], yzd[j+1])
		}
		if (length(kd) == 0) {
			du2 <- c(urnd[j], urnd[j+1]);
			dv2 <- c(yzd[j], yzd[j+1])
		}
		if (jodd == 1 & j == 1) {
			zigu1 <- du1;
			zigv1 <- dv1;
			zigu2 <- du2;
			zigv2 <- dv2
		}
		if (jodd == 0 & j == 1) {
			zigu1 <- du2;
			zigv1 <- dv2;
			zigu2 <- du1;
			zigv2 <- dv1
		}		
		if (jodd == 1 & j > 1) {
			zigu1 <- c(zigu1, du1);
			zigv1 <- c(zigv1, dv1);
			zigu2 <- c(zigu2, du2);
			zigv2 <- c(zigv2, dv2);
		}
    	if (jodd == 0 & j > 1) {
			zigu1 <- c(zigu1, du2); # updates successive u-values in transect one
        	zigv1 <- c(zigv1, dv2); # updates successive v-values in transect one
        	zigu2 <- c(zigu2, du1); # updates successive u-values in transect two
        	zigv2 <- c(zigv2, dv1); # updates successive v-values in transect two
        }
	}
	zigu1 <- c(zigu2[1], zigu1)
	zigv1 <- c(zigv2[1], zigv1)
	zigu2 <- c(zigu1[2], zigu2)
	zigv2 <- c(zigv1[2], zigv2)

	zigu1 <- c(zigu1, zigu2[length(zigu2)])
	zigv1 <- c(zigv1, zigv2[length(zigv2)])
	zigu2 <- c(zigu2, zigu1[length(zigu1) - 1])
	zigv2 <- c(zigv2, zigv1[length(zigv1) - 1])       	

	j <- which(uupp < urnd[1])
	if (length(j) > 0) {
		zigu2 <- c(uupp[j], zigu2);
		zigv2 <- c(vupp[j], zigv2)
		}
	j <- which(ulow < urnd[1])
	if (length(j) > 0) {
		zigu1 <- c(ulow[j], zigu1);
		zigv1 <- c(vlow[j], zigv1)
		}
	ju <- which(uupp > urnd[length(urnd)])
	jd <- which(ulow > urnd[length(urnd)])
	jrnd <- nurnd%%2
	if (jrnd == 0) {
		if (length(ju) > 0) {
			zigu2 <- c(zigu2, uupp[ju]);
			zigv2 <- c(zigv2, vupp[ju])
			}
		if (length(jd) > 0) {
			zigu1 <- c(zigu1, ulow[jd])
			zigv1 <- c(zigv1, vlow[jd])
			}
		}
	if (jrnd == 1) {
		if (length(ju) > 0) {
			zigu1 <- c(zigu1, uupp[ju])
			zigv1 <- c(zigv1, vupp[ju])
			}
		if (length(jd) > 0){
			zigu2 <- c(zigu2, ulow[jd])
			zigv2 <- c(zigv2, vlow[jd])
			}
		}
	lonlatu <- t(A)%*%t(cbind(zigu1,zigv1))
	lonu <- lonlatu[1,]/60/c0 + lon0
	latu <- lonlatu[2,]/60 + lat0
	lonlatd <- t(A)%*%t(cbind(zigu2, zigv2))
	lond <- lonlatd[1,]/60/c0 + lon0
	latd <- lonlatd[2,]/60 + lat0
	ziglon1 <- lonu
	ziglat1 <- latu 
	ziglon2 <- lond
	ziglat2 <- latd
	xz1 <- (ziglon1-lon0)*60*cos(lat0/180*pi)
	yz1 <- (ziglat1-lat0)*60
	xz2 <- (ziglon2-lon0)*60*cos(lat0/180*pi)
	yz2 <- (ziglat2-lat0)*60
	dist1 <- sum(sqrt(diff(xz1)^2 + (diff(yz1))^2))
	dist2 <- sum(sqrt(diff(xz2)^2 + (diff(yz2))^2))
	out <- list(ziglon1 = ziglon1, ziglat1 = ziglat1, 
	ziglon2 = ziglon2, ziglat2 = ziglat2, 
	dist1 = dist1, dist2 = dist2, zigu1 = zigu1, zigv1 = zigv1)
}


		
		
#paralltrx(3, 100, 10, 50)
plotStratum <- function(lonlat, lonlat12=NULL){
	library(rgdal)
	library(ggmap)
	library(ggplot2)
	
	location <- colMeans(lonlat[, c("Longitude", "Latitude")])
	
	gmap <- get_map(location=location, zoom=4, maptype="terrain", source="google", col="bw")

	p <- ggmap(gmap) + #ggplot(lonlat, aes(x = Longitude, y = Latitude)) +
		geom_polygon(data=lonlat, aes(x=Longitude, y=Latitude, fill=Stratum, group=Stratum), colour="black", alpha=0.3, inherit.aes=FALSE)
	# Add stratum entry and exit points:
	if(length(lonlat12)){
		p <- p + 
			geom_point(data=lonlat12, aes(x=V1, y=V2, colour=V5), shape=19, show.legend=FALSE) + 
			geom_point(data=lonlat12, aes(x=V3, y=V4, colour=V5), shape=17, show.legend=FALSE)
			scale_colour_continuous(guide=FALSE)
	}
	print(p)
}


#lonlat <- read.table("~/Documents/Produktivt/Prosjekt/PHD_Holmin/2017-12-04 PELFOSS SurveyPlanner/SurveyPlanner/SurveyPlanner/Kartdata_maitokt2017/lonlatx.txt")
#names(lonlat) <- c("Longitude", "Latitude", "Stratum")
#lonlat$Stratum <- factor(lonlat$Stratum)
#lonlat12 <- read.table("~/Documents/Produktivt/Prosjekt/PHD_Holmin/2017-12-04 PELFOSS SurveyPlanner/SurveyPlanner/SurveyPlanner/Kartdata_maitokt2017/lonlat12x.txt")
#lonlat12$V5 <- factor(lonlat12$V5)
#
#plotStratum(lonlat, lonlat12)
#
#
#
#
#
#projectName <- "Test_Rstox"
#g <- getBaseline(projectName, input="proc", proc=NULL, drop=FALSE)
#nstrata <- nrow(g$processData$stratumpolygon)
#lonlat <- vector("list", nstrata)
#for(i in seq_along(lonlat)){
#	lonlat[[i]] <- cbind(multipolygon2matrix(g$processData$stratumpolygon$Polygon[i]), Stratum=g$processData$stratumpolygon$Stratum[i])
#}
##r$Stratum <- factor(r$Stratum)
#lonlat <- do.call("rbind", lonlat)
#names(lonlat) <- c("Longitude", "Latitude", "Stratum")
#lonlat$Stratum <- factor(lonlat$Stratum)
#
#plotStratum(lonlat)







































# Failed attempt to clean up the parallel transect code. The code was too little documented and too complicated to be rewritten without assintance from the author:
#
#
#	if (nsim == 1) {
#		nsim == 0
#		}
#
#	# EXTRACTION AND FORMATTING OF POLYGON NODES AND ENTRANCE AND EXIT POINTS
#	lbno <- lonlat[which(lonlat[,3] == no),1:2]			# picks polygon nodes for field no
#	lon <- lbno[,1]											# polygon node decimal longitudes
#	lat <- lbno[,2]											# polygon node decimal latitudes
#	lb12 <- lonlat12[which(lonlat12[,5] == no),1:4]	# picks entrance and exit points for field no
#	if(rev.entrance==T) lb12 <- lonlat12[which(lonlat12[,5] == no),c(3,4,1,2)] # reverse entrance and exit
#	lb12 <- as.numeric(lb12)								# converts to numeric (lb12 is one line)
#	lon1 <- lb12[1]				# decimal longitude of entrance point
#	lat1 <- lb12[2]				# decimal latitude of entrance point
#	lon2 <- lb12[3]				# decimal longitude of exit point
#	lat2 <- lb12[4]				# decimal latitude of exit point
#	
#	n <- length(lon)			# number of nodes in polygon 
#	lon <- matrix(lon,n,1)		# ascertains lon to be column vector
#	lat <- matrix(lat,n,1)		# ascertains lat to be column vector
#	# ascertains lon(1) = lon(end) and lat(1) = lat(end) if the difference of entrance and exit points
#	# exceed 1e-6 deg for either lon-values, lat-values or both
#	if (abs(lon[1]-lon[n]) > 1e-6 | abs(lat[1]-lat[n]) > 1e-6)	# TRUE if difference exceeds threshold
#	{
#		lon <- c(lon, lon[1])	# adds a new last lon-value identical to first one
#		lat <- c(lat, lat[1])	# adds a new last lat-value identical to first one
#		n <- n + 1				# adjusts n to define the length of lon and lat
#	}
#	lon <- matrix(lon,n,1)		# ascertains lon to be a vertical vector
#	lat <- matrix(lat,n,1)		# ascertains lat to be a vertical vector
#
#	y <- subconvex(lon,lat,lon1,lat1,lon2,lat2)
#	uc <- y$uc
#	vc <- y$vc
##	vel <- inndata[1,7]
##	t <- inndata[1,8]
#	c <- ccalc(uc, vc, vel, t)
##	fac <- inndata[1,9]
#
## The following is only for plotting to show that the transects fill up the area:
#	plot(lon,lat,type='l')
#	for (jsim in 1:max(1,nsim)) {
#		fac <- runif(1)
#
#		ur1 <- fac*c
#		urnd <- seq(min(uc)+ur1, max(uc),by = c)
#		#nurnd <- length(urnd)
#		uupp <- y$uupp
#		ulow <- y$ulow
#		vupp <- y$vupp
#		vlow <- y$vlow
#		q <- subyz(urnd, uupp, ulow, vupp, vlow)
#		yzu <- q$yz[,1]
#		yzd <- q$yz[,2]
#		A <- y$A
#		c0 <- y$c0
#		lon0 <- y$lon0
#		lat0 <- y$lat0
#		zig <- subuplow(urnd, uupp, vupp, ulow, vlow, yzu, yzd, A, c0, lon0, lat0)
#		lines(zig$ziglon1,zig$ziglat1,col = 'red')
#	}
#	zig <- subuplow(urnd,uupp,vupp,ulow,vlow,yzu,yzd,A,c0,lon0,lat0)
#                       
#	lonrnd <- zig$ziglon1
#	latrnd <- zig$ziglat1
#  
#	dist1 <- 0
#	for(i in 1:(length(lonrnd)-1)){
#	  dist1 <- dist1 + (spDistsN1(cbind(lonrnd,latrnd),cbind(lonrnd,latrnd)[i,], longlat=T)[i+1])
#	  }
#	dist1 <- dist1/1.852  
#	
#	#dist1 <- spDistsN1(cbind(lonrnd,latrnd),cbind(lonrnd,latrnd)[1,], longlat=T)[i+1]
#	out <- list(name.field=name.field,
#        cruiseline = as.data.frame(cbind(lonrnd,latrnd)), 
##        Area = A,
#        Distance = dist1
##        Sur.cov = dist1/ sqrt(A)
#        ) 
#
#	## Create file to MaxSea
#	## Create ascii fil til import i MaxSea (se manual for code)
#	item.type <- 257  # Linje
#	item.id <- 20      # Setter id-coder på linjene
#	item.col <- 1     # Blå
#	north <- "N"
#	west <- "E"
#	
#	
#	##
#	tracks <- "~/Documents/Produktivt/Prosjekt/PHD_Holmin/2017-12-04 PELFOSS SurveyPlanner/SurveyPlanner/SurveyPlanner/Kartdata_maitokt2017/tracks/"
#	filnavn.ut <- file.path(tracks, paste0(name.field, ".asc"))
#	ant.dec.pos <- 3
#	n.row <- nrow(out$cruiseline)
#	                    
#	out1 <- cbind(rep(item.type,n.row),
#            rep(item.id,n.row),
#            rep(item.col,n.row),
#            round(out$cruiseline$lat,ant.dec.pos),
#            rep(north,n.row),
#            round(out$cruiseline$lonrnd,ant.dec.pos),
#            rep(west,n.row))
#            
#	write.table(out1,file=filnavn.ut,row.names=F, col.names=F,sep=",",quote=F)
#	
#	polyg <- cbind(lon,lat)
#	polyg.sp = SpatialPolygons(list(Polygons(list(Polygon(polyg)), "x")))
#	plot(polyg.sp, main=name.field)
#	#plot(lon,lat,type = 'l',main=name.field)
#	points(c(lon1,lon2),c(lat1,lat2),col=2)
#	text(c(lon1,lon2),c(lat1,lat2),labels=1:2,col=2)
#	lines(lonrnd, zig$ziglat1, col="red")
#	#points(spsample(polyg.sp, n = 10, "random"), pch = 2, col=2)
#	spsample(polyg.sp, n = 10, "random")
#
#	#out <- list(lon=lon,lat=lat,zig)
#	#track.description <- paste("../Kartdata/rutelinjer/",name.field,".asc",sep="")
#	
#	tmp <- as.data.frame(cbind(floor(out$cr$latrnd), round(60*(out$cr$latrnd- floor(out$cr$latrnd)),2),
#                           floor(out$cr$lonrnd), round(60*(out$cr$lonrnd- floor(out$cr$lonrnd)),2)))
#						   names(tmp) <- c("LatDeg","LatMin","LonDeg","LonMin")
#
#	name.field
#	vel # Speed
#	dist1 ## Sailing distance
#	area.nm2 <- areaPolygon(polyg)/1852/1852 # Meters2
#	Sur.cov <- dist1/ sqrt(area.nm2)
#	print(tmp)  
#	## TABLE
#	filnavn1 <-  file.path(tracks, paste0("INFO", name.field, ".asc"))
#	capture.output( cat("\n",format(c(date()),width=20, justify = "left")), file=filnavn1)
#	capture.output( cat("\n", "Stratum (Toktområde)    ", format(name.field,width=7, justify ="right")), file=filnavn1, append=T)
#	capture.output( cat("\n", "Speed and time available", format(c(paste(vel,"knots"),paste(t, "h")),width=7, justify ="right")), file=filnavn1, append=T)
#	capture.output( cat("\n", "Stratum area (n.mi2)    ", format(area.nm2,width=7, justify ="right")), file=filnavn1, append=T)
#	capture.output( cat("\n", "Sailing distance (n.mi) ", format(dist1,width=7, justify ="right")), file=filnavn1, append=T)
#	capture.output( cat("\n", "Survey coverage         ", format(Sur.cov,width=7, justify ="right")), file=filnavn1, append=T)
#	capture.output( cat("\n", " "), file=filnavn1, append=T)
#	capture.output( cat("\n", "Transect positions      "), file=filnavn1, append=T)
#	capture.output( cat("\n", " "), file=filnavn1, append=T)
#	capture.output(cat("\n", " "), tmp, file= filnavn1, append=T)  
#	filnavn2 <- file.path(tracks, paste0(name.field, ".txt"))
#	out2 <- as.data.frame(cbind(1,out$cr))
#	names(out2) <- c("Line","Longitude", "Latitude")
#	write.csv(out2, file=filnavn2,row.names=F)
#}
#
##' @importFrom utils head tail
#subconvex <- function(lon, lat, lon1, lat1, lon2, lat2) {
#	# Account for the reduced cross sectional radius of the Earth by latitude. [HERE WE SHOULD MAYBE USE A LATLON TO XY CONVERSION, WITH ORIGIN AT lonlatMean]:
#	c0 <- cos(mean(lat)/180*pi)
#	lonlat <- cbind(lon * c0, lat)
#	# Remove the last position if identical to the first [CHECK WHY IN parallelTransectsOneStratum() THE LAST POINT IS ADDED!]:
#	if (all(head(lonlat,1) == tail(lonlat,1))) {
#		lonlat <- lonlat[-1,,drop=FALSE]
#	}
#	# Remove duplicate points:
#	#j <- which(diff(lon) == 0 & diff(lat) == 0)
#	#nj <- length(j)
#	#if (nj > 0){
#	#	ind = setdiff(1:length(lon), j+1)
#	#	lon = lon(ind)
#	#	lat = lat(ind)
#	#}
#	
#	lonlatMean <- colMeans(lonlat)
#	
#	#lon0 <- mean(lon)
#	#lat0 <- mean(lat)
#	#c0 <- cos(lat0/180*pi)
#	#x1 <- (lon1-lon0)*60*c0
#	#y1 <- (lat1-lat0)*60
#	#x2 <- (lon2-lon0)*60*c0
#	#y2 <- (lat2-lat0)*60
#	#tet <- atan2(y2-y1,x2-x1)
#	# Create a rotation matrix by which to rotate the points into the coorstinate system with x axis along the direction from (lon1, lat1) to (lon2, lat2)
#	tet <- atan2(lat2 - lat1, c0 * (lon2 - lon1))
#	A = matrix(c(cos(tet), sin(tet), -sin(tet), cos(tet)), 2, 2, byrow=TRUE)
#	
#	lonlatShifted <- lonlat - lonlatMean
#	
#	lonlatShiftedRotated <- A %*% t(lonlatShifted)
#	
#	#xc <- (lon-lon0)*60*c0
#	#yc <- (lat-lat0)*60
#	#uv <- A%*%t(cbind(xc,yc))
#	#uc <- uv[1,]
#	#vc <- uv[2,]
#	
#	n <- nrow(lonlatShiftedRotated)
#	jmin <- which(uc == min(uc))
#	jmax <- which(uc == max(uc))
#	if (length(jmin) == 2){
#		j1 <- min(jmin)
#		j2 <- max(jmin)
#		if (j2 - j1 == 1) {
#			uc <- t(c(uc[1:j1], uc[j1], uc[j2:n]))
#			uc <- t(uc)
#			vc <- t(c(vc[1:j1], (vc[j1]+vc[j2])/2, vc[j2:n]))
#			vc <- t(vc)
#			jmin <- j1[1] + 1
#		}
#		if (j2 - j1 > 1) {
#			uc <- t(c(uc[j1], uc))
#			uc <- t(uc)
#			vc <- t(c((vc[j1]+vc[j2])/2, vc))
#			vc <- t(vc)
#			jmin <- 1
#		}
#	}
#	if (length(jmax) == 2) {
#		n <- length(uc)
#		jmax <- which(uc == max(uc))
#		j1 <- min(jmax)
#		j2 <- max(jmax)
#		if (j2 - j1 == 1){
#			uc <- t(c(uc[1:j1], uc[j1], uc[j2:n]))
#			uc <- t(uc)
#			vc <- t(c(vc[1:j1], (vc[j1]+vc[j2])/2, vc[j2:n]))
#			vc <- t(vc)
#			jmax <- j1 + 1
#		}
#		if (j2 - j1 > 1) {
#			uc <- t(c(uc[j1], uc))
#			uc <- t(uc)
#			vc <- t(c((vc[j1]+vc[j2])/2, vc))
#			vc <- t(vc)
#			jmax <- 1
#		}
#	}
#	n <- length(uc)
#	if (jmax > jmin){
#		ind1 <- seq(jmin,jmax,1)
#		ind2 <- c(seq(jmax,n,1), 1:jmin)
#		nind2 <- length(ind2)
#		ind2 <- ind2[nind2:1]
##		ind2 <- ind2[order(ind2, decreasing = T)]
#	}
#	if (jmax < jmin) {
#		ind1 <- c(seq(jmin,n,1), 1:jmax)
##		ind2 <- jmax:jmin
#		ind2 <- jmin:jmax
##		ind2 <- ind2[order(ind2,decreasing = T)]
#	}
#	if (mean(vc[ind1]) < mean(vc[ind2])){
#		indupp <- ind2
#		indlow <- ind1
#	}
#	if (mean(vc[ind1]) > mean(vc[ind2])){
#		indupp <- ind1
#		indlow <- ind2
#	}
#	dupp <- diff(uc[indupp])
#	jupp <- which(dupp < 0)
#	nupp <- length(jupp)
#	if (nupp > 0){
#		for (j in 1:nupp){
#			j1 <- indupp[jupp[j]]
#			j2 <- indupp[jupp[j]+1]
#			if (vc[j1] < vc[j2]) {
#				uc[j1] <- uc[j2]
#			}
#			if (vc[j1] > vc[j2]) {
#				uc[j2] <- uc[j1]
#			}
#		}
#	}
#	dlow <- diff(uc[indlow])
#	jlow <- which(dlow < 0)
#	nlow <- length(jlow)
#	if (nlow > 0){
#		for (j in 1:nlow){
#			j1 <- indlow[jlow[j]]
#			j2 <- indlow[jlow[j] + 1]
#			if (vc[j1] < vc[j2]) {
#				uc[j1] <- uc[j2]
#			}
#			if (vc[j1] > vc[j2]) {
#				uc[j2] <- uc[j1]
#			}
#		}
#	}
#	uupp <- uc[indupp]
#	vupp <- vc[indupp]
#	ulow <- uc[indlow]
#	vlow <- vc[indlow]
#	lonupp <- lon[indupp]
#	latupp <- lat[indupp]
#	lonlow <- lon[indlow]
#	latlow <- lat[indlow]
#	vm <- mean(vupp) - mean(vlow)
#	out <- list(uc = uc, 
#	vc = vc, 
#	uupp = uupp,
#	vupp = vupp,
#	ulow = ulow,
#	vlow =vlow,
#	lonlat = lonlat,
#	lonlatMean = lonlatMean,
#	c0 = c0,
#	vm = vm,
#	A = A)
#}
#
#ccalc <- function(uc, vc, vel, t) {
#	poly <- cbind(uc, vc)
#	area <- areapl(poly)
#	umin <- min(uc)
#	umax <- max(uc)
#	L <- vel*t
#	du <- umax - umin
#	c <- area/(vel * t - du)
#	if (c < 0) {
#		c <- 0.5 * du
#	}
#	out <- c
#}
#	
#subyz <- function(urnd, uupp, ulow, vupp, vlow) {
#	nurnd <- length(urnd)
#	yzu <- matrix(0,nurnd,1)
#	yzd <- yzu
#	for (j in 1:nurnd) {  # loop to identify (u,v) positions of nodes
#	    ju <- which(urnd[j]-uupp < 0)  # to identify actual upper polygon segment
#	    ju1 <- ju[1]-1  # index to first point of upper polygon segment with node j
#	    ju2 <- ju1 + 1  # index to second point of upperpolygon segment with node j
#	    yzu[j] <- vupp[ju1] + (urnd[j]-uupp[ju1])/(uupp[ju2]-uupp[ju1])*(vupp[ju2]-vupp[ju1])
#	    # yzu = y-values of nodes in upper polygon
#	    jd <- which(urnd[j]-ulow < 0) # to identify actual lower polygon segment
#	    jd1 <- jd[1]-1  # index to first point of lower polygon segment with node j
#	    jd2 <- jd1+1  # index to second point of lower polygon segment with node j
#	    yzd[j] <- vlow[jd1] + (urnd[j]-ulow[jd1])/(ulow[jd2]-ulow[jd1])*(vlow[jd2]-vlow[jd1])
#    }
#    # yzd = y-values of nodes in lower polygondasfasdf
#    yz <- cbind(yzu,yzd)
#    out <- list(yz = yz)
#}
#    
#subuplow <- function(urnd, uupp, vupp, ulow, vlow, yzu, yzd, A, c0, lon0, lat0) {
#	#zigu1 <- NULL
#	#zigv1 <- NULL
#	#zigu2 <- NULL
#	#zigv2 <- NULL
#	nurnd <- length(urnd)
#	for (j in 1:nurnd-1) {
#		jodd <- j%%2
#		ku <- which(uupp > urnd[j] & uupp  < urnd[j + 1])
#		kd <- which(ulow > urnd[j] & ulow < urnd[j + 1])
#		if (length(ku) > 0) {
#			du1 <- c(urnd[j], uupp[ku], urnd[j+1])
#			dv1 <- c(yzu[j], vupp[ku], yzu[j+1])
#		}
#		if (length(ku) == 0) {
#			du1 <- c(urnd[j], urnd[j+1])
#			dv1 <- c(yzu[j], yzu[j+1]	)
#		}
#		if (length(kd) > 0) {
#			du2 <- c(urnd[j], ulow[kd], urnd[j+1])
#			dv2 <- c(yzd[j], vlow[kd], yzd[j+1])
#		}
#		if (length(kd) == 0) {
#			du2 <- c(urnd[j], urnd[j+1])
#			dv2 <- c(yzd[j], yzd[j+1])
#		}
#		if (jodd == 1 & j == 1) {
#			zigu1 <- du1
#			zigv1 <- dv1
#			zigu2 <- du2
#			zigv2 <- dv2
#		}
#		if (jodd == 0 & j == 1) {
#			zigu1 <- du2
#			zigv1 <- dv2
#			zigu2 <- du1
#			zigv2 <- dv1
#		}		
#		if (jodd == 1 & j > 1) {
#			zigu1 <- c(zigu1, du1)
#			zigv1 <- c(zigv1, dv1)
#			zigu2 <- c(zigu2, du2)
#			zigv2 <- c(zigv2, dv2)
#		}
#    	if (jodd == 0 & j > 1) {
#			zigu1 <- c(zigu1, du2) # updates successive u-values in transect one
#        	zigv1 <- c(zigv1, dv2) # updates successive v-values in transect one
#        	zigu2 <- c(zigu2, du1) # updates successive u-values in transect two
#        	zigv2 <- c(zigv2, dv1) # updates successive v-values in transect two
#        }
#	}
#	zigu1 <- c(zigu2[1], zigu1)
#	zigv1 <- c(zigv2[1], zigv1)
#	zigu2 <- c(zigu1[2], zigu2)
#	zigv2 <- c(zigv1[2], zigv2)
#	
#	zigu1 <- c(zigu1, zigu2[length(zigu2)])
#	zigv1 <- c(zigv1, zigv2[length(zigv2)])
#	zigu2 <- c(zigu2, zigu1[length(zigu1) - 1])
#	zigv2 <- c(zigv2, zigv1[length(zigv1) - 1])       	
#	
#	j <- which(uupp < urnd[1])
#	if (length(j) > 0) {
#		zigu2 <- c(uupp[j], zigu2)
#		zigv2 <- c(vupp[j], zigv2)
#	}
#	j <- which(ulow < urnd[1])
#	if (length(j) > 0) {
#		zigu1 <- c(ulow[j], zigu1)
#		zigv1 <- c(vlow[j], zigv1)
#	}
#	ju <- which(uupp > urnd[length(urnd)])
#	jd <- which(ulow > urnd[length(urnd)])
#	jrnd <- nurnd%%2
#	if (jrnd == 0) {
#		if (length(ju) > 0) {
#			zigu2 <- c(zigu2, uupp[ju])
#			zigv2 <- c(zigv2, vupp[ju])
#		}
#		if (length(jd) > 0) {
#			zigu1 <- c(zigu1, ulow[jd])
#			zigv1 <- c(zigv1, vlow[jd])
#		}
#	}
#	if (jrnd == 1) {
#		if (length(ju) > 0) {
#			zigu1 <- c(zigu1, uupp[ju])
#			zigv1 <- c(zigv1, vupp[ju])
#		}
#		if (length(jd) > 0){
#			zigu2 <- c(zigu2, ulow[jd])
#			zigv2 <- c(zigv2, vlow[jd])
#		}
#	}
#	lonlatu <- t(A)%*%t(cbind(zigu1,zigv1))
#	lonu <- lonlatu[1,]/60/c0 + lon0
#	latu <- lonlatu[2,]/60 + lat0
#	lonlatd <- t(A)%*%t(cbind(zigu2, zigv2))
#	lond <- lonlatd[1,]/60/c0 + lon0
#	latd <- lonlatd[2,]/60 + lat0
#	ziglon1 <- lonu
#	ziglat1 <- latu 
#	ziglon2 <- lond
#	ziglat2 <- latd
#	xz1 <- (ziglon1-lon0)*60*cos(lat0/180*pi)
#	yz1 <- (ziglat1-lat0)*60
#	xz2 <- (ziglon2-lon0)*60*cos(lat0/180*pi)
#	yz2 <- (ziglat2-lat0)*60
#	dist1 <- sum(sqrt(diff(xz1)^2 + (diff(yz1))^2))
#	dist2 <- sum(sqrt(diff(xz2)^2 + (diff(yz2))^2))
#	out <- list(ziglon1 = ziglon1, ziglat1 = ziglat1, 
#	ziglon2 = ziglon2, ziglat2 = ziglat2, 
#	dist1 = dist1, dist2 = dist2, zigu1 = zigu1, zigv1 = zigv1)
#}

