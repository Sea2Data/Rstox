#' Generate DATRAS output for a specific StoX project  
#' or user specified cruise number and ship name. 
#' NOTE: There are some differences between the output of this function 
#' and the previous submissions in the HH file due to lack of trawl sensor and CTD data:
#' (HydroStNo, Netopening, DoorSpread, WingSpread) (SurTemp BotTemp SurSal BotSal)
#'
#' @param datrasProject The name or full path of the project, a baseline object (as returned from \code{getBaseline} or \code{runBaseline}), or a project object (as returned from \code{openProject}). Projects located in sub directories of the default workspace can be given by the relative path, or are searched for by name.
#'
#' @examples
#' \dontrun{
#' # Process existing project
#' datras <- exportDatras("NMD_CruiseNumber_2018102_ShipName_G.O.Sars")
#' head(datras$outputData$DATRASConvert$DATRASConvert_BioticData_HH.txt)
#' head(datras$outputData$DATRASConvert$DATRASConvert_BioticData_HL.txt)
#' }
#'
#' @importFrom XML xmlParse getNodeSet xmlGetAttr
#' @export
#' @rdname exportDatras
#'
exportDatras <- function(datrasProject)
{
	# For getting the ship code
	# TODO: There is old and new ship code, must think of some way to differentiate that (currently, assuming only new)
	getICESShipCode <- function(shipName){
		# We have to remove "."," " and use uppercase
		shipName <- toupper(gsub("[[:space:][:punct:]]", "", shipName))

		# Replace Å with AA
		shipName <- gsub("\u00C5", "AA", shipName)

		tempShipFile <- tempfile()
		download.file("https://vocab.ices.dk/services/rdf/collection/TS_Ship", tempShipFile)

		data <- XML::xmlParse(tempShipFile)
		nodes <- XML::getNodeSet(data, paste0("//skos:concept[contains(translate(skos:prefLabel[normalize-space()],'abcdefghijklmnopqrstuvwxyz. ','ABCDEFGHIJKLMNOPQRSTUVWXYZ'), \"", shipName, "\")]"))
		shipDetailUrl <- XML::xmlGetAttr(nodes[[1]], "rdf:about")
		shipCode <- tail(unlist(strsplit(shipDetailUrl, split="/")), 1)
		
		# Simple hack for Cefas Endeavour
		if(shipCode == "END") shipCode <- "ENDW"

		unlink(tempShipFile)

		return(shipCode)
	}

	# For StatRec calculation
	create.rect.f<-function(data, var1=data$ShootLong, var2=data$ShootLat) {
	       data$ices.lon<-ifelse(var1> -1 & var1<0, "E9",
		  ifelse(var1>  0 & var1<1,"F0",
		  ifelse(var1> 1 & var1<2,"F1",
		  ifelse(var1> 2 & var1<3,"F2",
		  ifelse(var1> 3 & var1<4,"F3",
		  ifelse(var1> 4 & var1<5,"F4",
		  ifelse(var1> 5 & var1<6,"F5",
		  ifelse(var1> 6 & var1<7,"F6",
		  ifelse(var1> 7 & var1<8,"F7",
		  ifelse(var1> -3 & var1< -2,"E7",
		  ifelse(var1> -2 & var1< -1,"E8",NA)))))))))))
	       ##
	       data$ices.lat<-ifelse(var2>56 &var2<=56.5,41,
		  ifelse(var2>56.5 &var2<=57,42,
		  ifelse(var2>57 &var2<=57.5,43,
		  ifelse(var2>57.5 &var2<=58,44,
		  ifelse(var2>58 &var2<=58.5,45,
		  ifelse(var2>58.5 &var2<=59,46,
		  ifelse(var2>59 &var2<=59.5,47,
		  ifelse(var2>59.5 &var2<=60,48,
		  ifelse(var2>60 &var2<=60.5,49,
		  ifelse(var2>60.5 &var2<=61,50,
		  ifelse(var2>61 &var2<=61.5,51,
		  ifelse(var2>61.5 &var2<=62,52,NA))))))))))))
	       data$ices.rect<-paste(data$ices.lat,data$ices.lon,sep="")
	}

	# Get Biotic data using Rstox (if there is a valid project)
	# TODO: check datras output
	#if(!is.null(cruiseNumber) && !is.null(cruiseShip)) {
	#	datrasProject <- getNMDdata(cruise = cruiseNumber, shipname = cruiseShip, model = "DATRASTemplate", ver = ver, server = server, snapshot = snapshot, ow = overwrite)
	#	if(is.null(datrasProject))
	#		datrasProject <- paste("NMD", "CruiseNumber", cruiseNumber,"ShipName", cruiseShip, sep="_")
	#}
	rstox.data <- NULL

	# Process StoX baseline
	rstox.data <- getBaseline(datrasProject)
	if(is.null(rstox.data)){
		cat("ERROR: Unable to process biotic data!")
		return(NULL)
	}

	# Get the DATRAS convert outputs
	if(is.null(rstox.data$outputData) || is.null(rstox.data$outputData$DATRASConvert)){
		cat("ERROR: No DATRAS output found. Please double check the project settings and ensure datrasConvert R model is included!")
		return(NULL)
	}

	hhRaw <- rstox.data$outputData$DATRASConvert$DATRASConvert_BioticData_HH.txt
	hlRaw <- rstox.data$outputData$DATRASConvert$DATRASConvert_BioticData_HL.txt
	caRaw <- rstox.data$outputData$DATRASConvert$DATRASConvert_BioticData_CA.txt

	# Prepare HH table
	## Read from rstox output
	txt <- textConnection(paste(hhRaw[,1]))
	hh <- read.csv(txt, header=FALSE, stringsAsFactors=FALSE)
	## Set names of the fields
	names(hh) <- unlist(strsplit(names(hhRaw), split="[.]"))

	# Prepare HL table
	## Read from rstox output
	txt <- textConnection(paste(hlRaw[,1]))
	hl <- read.csv(txt, header=FALSE, stringsAsFactors=FALSE)
	## Set names of the fields
	names(hl) <- unlist(strsplit(names(hlRaw), split="[.]"))

	# Prepare CA table
	## Read from rstox output
	txt <- textConnection(paste(caRaw[,1]))
	ca <- read.csv(txt, header=FALSE, stringsAsFactors=FALSE)
	## Set names of the fields
	names(ca) <- unlist(strsplit(names(caRaw), split="[.]"))

	## because it was a rental vessel, there are some missing parameters with the Stox conversion
	hh$Country<-'NOR'
	hl$Country<-'NOR'
	ca$Country<-'NOR'

	## get ship code from ICES
	cruiseNo <- unique(rstox.data$outputData$ReadBioticXML$ReadBioticXML_BioticData_FishStation.txt$cruise)
	Year <- unique(hh$Year)
	cruiseShip <-  unlist(lapply(getNMDinfo("cs"), function(x) x[x$Cruise==cruiseNo[1] & x$Year==Year[1], "ShipName"]))
	shipCode <- getICESShipCode(cruiseShip[1])

	hh$Ship <- shipCode
	hl$Ship <- shipCode
	ca$Ship <- shipCode

	#IU: Transform StatRec into character#
	transform(hh, StatRec = as.character(StatRec))
	hh$StatRec <- create.rect.f(hh)

	#IU: Transform into character#
	transform(hh, TimeShot = as.character(TimeShot))
	## add a 0 to the front of TimeShot
	hh$TimeShot<-ifelse(nchar(hh$TimeShot)==3,paste(0,hh$TimeShot,sep=''),hh$TimeShot);

	# Remove specCode "-" in both HL and CA data (if any)
	hl[hl$SpecCode=="-", "SpecCode"] <- -9
	ca[ca$SpecCode=="-", "SpecCode"] <- -9

	##---------------------------------------------------------------------------------
	## hl and ca data cleaning

	# Find species with different SpecVal, if any of them have SpecVal == 1, delete any other records with different SpecVal
	# otherwise, use the lowest SpecVal value for all

	tmp <- aggregate(SpecVal ~ SpecCode + StNo, hl, FUN = function(x) length(unique(x)))
	tmp <- tmp[tmp$SpecVal>1, ]

	for( rownum in 1: nrow(tmp) ) {
		tmpSpecs <- hl[(hl$StNo==tmp$StNo[rownum] & hl$SpecCode==tmp$SpecCode[rownum]),]$SpecVal
		if(any(tmpSpecs == 1))
			hl <- hl[!(hl$StNo==tmp$StNo[rownum] & hl$SpecCode==tmp$SpecCode[rownum] & hl$SpecVal!=1),]
		else
			hl[(hl$StNo==tmp$StNo[rownum] & hl$SpecCode==tmp$SpecCode[rownum]), c("SpecVal")] <- min(tmpSpecs)		
	}

	## SpecVal Conditionals
	hl[hl$SpecVal==0, c("Sex", "TotalNo", "CatIdentifier", "NoMeas", "SubFactor", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", "HLNoAtLngt")] <- -9

	hl[hl$SpecVal==4, c("NoMeas", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", "HLNoAtLngt")] <- -9
	hl[hl$SpecVal==4, c("SubFactor")] <- 1

	hl[hl$SpecVal==5, c("TotalNo", "NoMeas", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", "HLNoAtLngt")] <- -9
	hl[hl$SpecVal==5, c("SubFactor")] <- 1

	hl[hl$SpecVal==6, c("TotalNo", "NoMeas", "LngtCode", "LngtClass", "HLNoAtLngt")] <- -9

	hl[hl$SpecVal==7, c("NoMeas", "LngtCode", "LngtClass", "HLNoAtLngt")] <- -9

	hl[hl$SpecVal==10, c("CatCatchWgt")] <- -9

	## will now get errors in DATRAS upload for duplicate records
	hl <- hl[!duplicated(hl),]

	## hl and ca contain 0-tow info - must throw these out
	hl<-hl[hl$StNo %in% hh$StNo,]
	ca<-ca[ca$StNo %in% hh$StNo,]
	# throw out ca records for Invalid hauls
	ca<-ca[!ca$StNo %in% hh$StNo[hh$HaulVal=='I'],]

	# Number formats
	hh$Distance<-round(hh$Distance)
	hh$Warpingt<-round(hh$Warpingt)
	hh$KiteDim<-round(hh$KiteDim, 1)
	hh$Netopening<-round(hh$Netopening, 1)
	hh$WingSpread<-round(hh$WingSpread, 1)
	hh$DoorSpread<-sprintf("%.1f", round(hh$DoorSpread, 1))
	hh$DataType<-as.character(as.factor(hh$DataType))
	hh$HaulVal<-as.character(as.factor(hh$HaulVal))
	hl$TotalNo<-sprintf("%.2f",round(as.numeric(as.character(hl$TotalNo)), 2))
	hl$HLNoAtLngt<-sprintf("%.2f", round(as.numeric(as.character(hl$HLNoAtLngt)), 2))
	hl$SubFactor<-sprintf("%.4f", round(as.numeric(as.character(hl$SubFactor)), 4))
	hl$SubWgt<-sprintf("%d", round(as.numeric(as.character(hl$SubWgt))))

	##########################################
	## Removing some benthos - this won't be needed in the future
	## keep 11725 138139 138482 138483 140600 140621 140624 140625 141443 141444 141449 153083 153131-- these are cephaolopods
	## required benthos: 107205
	hl<-hl[!hl$SpecCode %in% c(230,558,830,883,1302,1839,100635,100706,100930,103929,106048,106087,106204,106733,106791,
		            106854,106928,107044,107218,107230,107240,107273,107292,107318,107330,107346,107397,107398,107551,
		            107616,107643,111374,111597,111604,116986,117302,117809,117815,117890,123117,123867,123920,123970,
		            123987,124319,124418,124913,124929,124934,125128,125131,125134,129196,129229,130464,130867,132072,
		            132480,135144,135302,137704,137732,138223,138239,138760,138899,139004,139488,140299,140627,141753,
		            144129,150642,178639,181228,23986719494,21263,100817,100982,106738,107160,107232,107277,107322,
		            107323,107327,107387,107531,107552,107564,107649,107651,111367,123080,123083,123084,123776,123813,
		            124043,124154,124160,124287,124535,125166,125333,128517,129840,138802,138878,138920,140467,140717,
		            143755,145541,145546,145548,532031,589677,1762,123082,149),]

	ca<-ca[!ca$SpecCode %in% c(230,558,830,883,1302,1839,100635,100706,100930,103929,106048,106087,106204,106733,106791,
		            106854,106928,107044,107218,107230,107240,107273,107292,107318,107330,107346,107397,107398,107551,
		            107616,107643,111374,111597,111604,116986,117302,117809,117815,117890,123117,123867,123920,123970,
		            123987,124319,124418,124913,124929,124934,125128,125131,125134,129196,129229,130464,130867,132072,
		            132480,135144,135302,137704,137732,138223,138239,138760,138899,139004,139488,140299,140627,141753,
		            144129,150642,178639,181228,23986719494,21263,100817,100982,106738,107160,107232,107277,107322,
		            107323,107327,107387,107531,107552,107564,107649,107651,111367,123080,123083,123084,123776,123813,
		            124043,124154,124160,124287,124535,125166,125333,128517,129840,138802,138878,138920,140467,140717,
		            143755,145541,145546,145548,532031,589677,1762,123082,149),]


	#more benthods 10216 = skate egg case
	hl<-hl[!hl$SpecCode %in% c(443,938,1131,1292,1337,1360,19494,22988,100751,100757,100790,101054,103484,104062,
		            106122,106669,107011,107052,107148,107239,107388,107563,110690,110911,110956,111411,117136,
		            117258,123260,123276,123321,123335,123574,123593 ,123851,123922,123985,124085,125158,125269,
		            128506,130467,130987,131779,134591,137683,141872,146142 ,149864,445590,510534,105,175,927,1107,
		            1135,1267,100793),]
	hl<-hl[!hl$SpecCode %in% c(105,175,927,1107,1135,1267,100793,103443,103692,106057,106835,106903,107558,110908,111361,
		            117940,122348,123160,123426,124257,125027,125284,131495,135294,135301,135306,138992,140528,140687,
		            167882,178527,239867,291396,106763,137656,117225,100653,125125,100698,131774,134366,123386,117228,
		            117994,138923,123127,137701,123320,131629 ,152391,1363,214,103543,106994,103450,129400,140143,
		            146420,141905,22496,988,103717,107163,982,985,123622,102145,1082,10216,103483),]

	ca<-ca[!ca$SpecCode %in% c(443,938,1131,1292,1337,1360,19494,22988,100751,100757,100790,101054,103484,104062,
		            106122,106669,107011,107052,107148,107239,107388,107563,110690,110911,110956,111411,117136,
		            117258,123260,123276,123321,123335,123574,123593 ,123851,123922,123985,124085,125158,125269,
		            128506,130467,130987,131779,134591,137683,141872,146142 ,149864,445590,510534,105,175,927,1107,
		            1135,1267,100793),]
	ca<-ca[!ca$SpecCode %in% c(105,175,927,1107,1135,1267,100793,103443,103692,106057,106835,106903,107558,110908,111361,
		            117940,122348,123160,123426,124257,125027,125284,131495,135294,135301,135306,138992,140528,140687,
		            167882,178527,239867,291396,106763,137656,117225,100653,125125,100698,131774,134366,123386,117228,
		            117994,138923,123127,137701,123320,131629 ,152391,1363,214,103543,106994,103450,129400,140143,
		            146420,141905,22496,988,103717,107163,982,985,123622,102145,1082,10216,103483),]

	hl<-hl[!hl$SpecCode %in% c(-9, 101,106769,106782,107010,107726,122478,123506,12437,124951,128539,129402,196221,205077,124373, 123187, 124710),]
	ca<-ca[!ca$SpecCode %in% c(-9, 101,106769,106782,107010,107726,122478,123506,12437,124951,128539,129402,196221,205077,124373, 123187, 124710),]

	## IU: Filter out additional benthos: 
	benthosSpecCodes <- c(104,956,966,1128,1296,1367,1608,11707,100782,100839,100854,103439,103732,104040,105865,106041,106673,106702,106789,106834,107152,
				107205,107264,110749,110916,110993,111152,111355,111365,117093,117195,118445,122626,123204,123255,123613,124147,124151,124324,124670,
				128490,128503,129563,130057,134691,136025,137710,138018,138068,138477,138631,138749,138938,140166,140173,140480,140625,141904,141929,
				149854,152997,532035,816800)

	hl<-hl[!hl$SpecCode %in% benthosSpecCodes,]
	ca<-ca[!ca$SpecCode %in% benthosSpecCodes,]


	#########################################################
	


	#############################################
	## ca records with no HL records
	## these records are because there is no catch weight
	## DATRAS does not accept length info without catch weight
	## so create a line in the HL for each, but give SpecValue=4 and delete ca record

	#IU: Improved cleaning#
	# Use join to find missing value in HL
	testca <- unique(data.frame(StNo=ca$StNo, SpecCode=ca$SpecCode, ca=TRUE))
	testhl <- unique(data.frame(StNo=hl$StNo, SpecCode=hl$SpecCode, hl=TRUE))
	tt <- merge(testca, testhl, by = c("StNo","SpecCode"), all = TRUE)
	missingHL <- tt[is.na(tt$hl),]

	if(nrow(missingHL)>0) {
		# Populate missing value in HL
		for(idxHL in c(1:nrow(missingHL))){
			r <- missingHL[idxHL,]
			tmp<-hl[hl$StNo==r$StNo,][1,]
			tmp$SpecCode <- r$SpecCode
			tmp$SpecVal<- 4
			tmp$TotalNo<- c(hh$HaulDur[hh$StNo==r$StNo])
			tmp$CatCatchWgt <- -9
			hl<-rbind(hl,tmp)
		}
	}

	# Save output
	hl<-hl[order(hl$StNo),]

	row.names(ca)<-1:nrow(ca)
	row.names(hl)<-1:nrow(hl)
	row.names(hh)<-1:nrow(hh)

	# Format to preserve decimal places
	HL<-format(hl, trim=T, width=0)
	CA<-format(ca, trim=T, width=0)
	HH<-format(hh, trim=T, width=0)

	# Collate into a single list
	tmp<-list(HH=HH,HL=HL,CA=CA)

	# Prepare output file name and rename previous output file if exists
	reportFile <- paste0(file.path(getProjectPaths(datrasProject)$RReportDir, paste0(paste0(cruiseNo, ".", cruiseShip), ".DATRAS.export.txt")))
	if(file.exists(reportFile))
		file.rename(reportFile, paste0(reportFile,".old"))

	# Write it
	lapply(tmp, write.table, file = reportFile, append=TRUE, row.names = FALSE, quote = FALSE, sep = ",")

	# Return Datras data as well, overwrite old data
	rstox.data$outputData$DATRASConvert$DATRASConvert_BioticData_HH.txt <- HH
	rstox.data$outputData$DATRASConvert$DATRASConvert_BioticData_HL.txt <- HL
	rstox.data$outputData$DATRASConvert$DATRASConvert_BioticData_CA.txt <- CA

	return(rstox.data)
}
