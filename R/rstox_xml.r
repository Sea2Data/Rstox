#*********************************************
#*********************************************
#' Write biotic and acosutic XML files from data frames given a (preferably hierarcical) xsd (xml schema file).
#'
#' \code{writeBioticXML} Writes a data frame to a biotic XML file. \cr \cr
#' \code{writeAcousticXML} Writes a data frame to an acoustic XML file. \cr \cr
#' \code{readHIXSD} Function for reading an xsd (xml schema file) and returning the structure of the corresponding xml. The output is designed to be used to store the xsd info as colnames of a data frame for use in the funciton frame2nestedList(), but may be useful for reading an arbitraty xsd file. \cr \cr
#' \code{validateHIXSD} Function for validating the input x against an xsd. \cr \cr
#' \code{writeHIXML} Utility function of \code{data.frame2xml} for writing any data frame to an XML file given an XML root and an xsd. \cr \cr
#' \code{data.frame2nestedList} Utility function of \code{data.frame2xml}  for converting a data frame to a nested list. The data frame must have column names with prefixes such as Level4.Var or Level2.Attr or Level3.AttrReq followed by "." and the variable name (e.g., Level3.AttrReq.serialno). \cr \cr
#' \code{list2XML} Utility function for converting a list to an xml object. This function is a generalization of the funciton as_xml_document() in the package xml2, which turns a list into anxml object, but not for too deep lists. \cr \cr
#' \code{data.frame2xml} Utility function of \code{writeHIXML} for converting a data frame to an xml object. \cr \cr
#'
#' @param x				The data frame to validate against the xsd.
#' @param file			The path to the XML file to write.
#' @param xsd			The path to an xsd (xml schema) file, or the output from \code{readHIXSD}, or the version of XSDs used in StoX (attached to the Rstox package).
#' @param addVersion	Logical: If TRUE, add the version interpreted from the xsd.
#' @param na.rm			Logical: If TRUE, remove missing values in the XML file (otherwise save these values as NA in the file).
#' @param declaration	The declaration string heading the XML file.
#' @param strict		Logical: If TRUE remove columns with names that are not recognized in the xsd.
#' @param discardSimple	Logical: If TRUE, discard simplecontent from the xsd.
#'
#' @examples
#' \dontrun{
#' # Read a biotic xsd in the Rstox package:
#' ver <- "1.4"
#' filename <- paste0("biotic_v", ver, ".xsd")
#' xsd <- system.file("extdata", "xsd", "biotic", filename, package="Rstox")
#' xsd <- readHIXSD(xsd)
#' head(xsd$x)
#'
#' # Get the biotic data from the test project:
#' g <- getBaseline("Test_Rstox", input=FALSE, endProcess="FilterBiotic", proc="FilterBiotic")
#' 
#' # The variable 'weight' has different meaning for g$FilterBiotic_BioticData_CatchSample.txt 
#' # and g$FilterBiotic_BioticData_Individual.txt, so we rename for the merging below to work:
#' library(data.table)
#' setnames(g$FilterBiotic_BioticData_CatchSample.txt, "weight", "weight.catchsample")
#' setnames(g$FilterBiotic_BioticData_Individual.txt, "weight", "weight.individual")
#' 
#' # Merge the biotic data to one data frame, but exclude the comment column first:
#' by12 <- c("cruise", "serialno", "platform")
#' by23 <- c(by12, "SpecCat", "species", "noname", "aphia", "samplenumber")
#' d <- merge(merge(g[[1]], g[[2]], all=TRUE, by=by12), g[[3]], all=TRUE, by=by23)
#' # Revert the names of the weights back to the original 
#' # for comparison with a new project using the new biotic.xml file:
#' setnames(g$FilterBiotic_BioticData_CatchSample.txt, "weight.catchsample", "weight")
#' setnames(g$FilterBiotic_BioticData_Individual.txt, "weight.individual", "weight")
#'
#' # Lengths are given in cm in StoX and in m in the xml-files:
#' d$length <- d$length / 100
#' # Weights are given in g in StoX and in kg in the xml-files:
#' d$weight.individual <- d$weight.individual / 1000
#'
#' # The variables 'startdate' and 'stopdate' are given at 'fishstation' level in StoX, 
#' # and the corresponding values at 'mission' level are ignored:
#' setnames(d, "startdate", "startdate.fishstation")
#' setnames(d, "stopdate", "stopdate.fishstation")
#'
#' # The variable 'startlog' is stored as 'logstart' in StoX:
#' names(d)[names(d) == "logstart"] <- "startlog"
#'
#' # The variables 'producttype', 'sampleproducttype' and 'specimensamplecount' are stored as 
#' # 'measurement', 'samplemeasurement' and 'individualsamplecount', respectively, in StoX:
#' setnames(d, "measurement", "producttype")
#' setnames(d, "samplemeasurement", "sampleproducttype")
#' setnames(d, "individualsamplecount", "specimensamplecount")
#'
#' # Validate the data against the xsd:
#' v <- validateHIXSD(d, xsd)
#' # The column names in StoX differ somewhat from the xsd:
#' d$specimenno <- d$no
#' d$year <- format(as.Date(d $startdate, "%d/%m/%Y"), "%Y")
#' d$missionnumber <- d$cruise
#' v <- validateHIXSD(d, xsd)
#' 
#' # Try to write the biotic data to an XML file:
#' tempXML <- file.path(tempdir(), "biotic.xml")
#' # Writing XML files takes time (one minute):
#' system.time(writeBioticXML(d, tempXML))
#'
#' # Recreate the Test_Rstox project using the new biotic.xml file:
#' createProject("Test_Rstox", ow=TRUE, 
#'     ReadBioticXML=list(FileName1=tempXML, FileName2="", FileName3="", FileName4="", FileName5=""))
#' # Reopen and save the project to format the project.xml file properly:
#' reopenProject("Test_Rstox")
#' saveProject("Test_Rstox")
#' 
#' # Get the biotic data from the project with the new file:
#' g2 <- getBaseline("Test_Rstox", input=FALSE, endProcess="FilterBiotic", proc="FilterBiotic")
#'
#' # There might be some minor differences due to precision:
#' all.equal(g, g2)
#'
#' # Finally reset to the original tesp project:
#' createProject("Test_Rstox", ow=TRUE)
#' }
#'
#' @export
#' @rdname writeBioticXML
#' 
writeBioticXML <- function(x, file, xsd="1.4", addVersion=TRUE, na.rm=TRUE, declaration="<?xml version=\"1.0\" encoding=\"UTF-8\"?>", strict=TRUE, discardSimple=FALSE){
	if(!file.exists(xsd)){
		filename <- paste0("biotic_v", xsd, ".xsd")
		xsd <- system.file("extdata", "xsd", "biotic", filename, package="Rstox")
	}
	root <- "missions"
	writeHIXML(x=x, file=file, root=root, xsd=xsd, addVersion=addVersion, na.rm=na.rm, declaration=declaration, strict=strict, discardSimple=discardSimple)
}
#'
#' @export
#' @rdname writeBioticXML
#' 
writeAcousticXML <- function(x, file, xsd="1", addVersion=TRUE, na.rm=TRUE, declaration="<?xml version=\"1.0\" encoding=\"UTF-8\"?>", strict=TRUE, discardSimple=FALSE){
	if(!file.exists(xsd)){
		filename <- paste0("LUF20_v", xsd, ".xsd")
		xsd <- system.file("extdata", "xsd", "acosutic", filename, package="Rstox")
	}
	root <- "echosounder_dataset"
	writeHIXML(x=x, file=file, root=root, xsd=xsd, addVersion=addVersion, na.rm=na.rm, declaration=declaration, strict=strict, discardSimple=discardSimple)
}
#'
#' @importFrom XML xmlParse xmlToList
#' @export
#' @rdname writeBioticXML
#' 
readHIXSD <- function(xsd, discardSimple=FALSE){
	# Change this when building the package:
	data <- XML::xmlParse(xsd)
	xml_data <- XML::xmlToList(data)
	
	getField <- function(x, field="name"){
		if(is.list(x)) x$.attrs[field] else x[field]
	}
	
	# Function to extract character levels, either as given in the file or trimmed of the suffix "Type" and put to lowercase:
	extractLevel <- function(x, trim=TRUE){
		types <- sapply(x, "[[", ".attrs")
		# Convert to lower case as in the variable names:
		if(trim){
			types <- tolower(gsub("Type", "", types))
		}
		types
	}
	
	# Function to extract variable types:
	extractType <- function(x){
		extractOne <- function(x){
			unname(sapply(x$sequence, getField, "type"))
		}
		out <- lapply(x, extractOne)
		names(out) <- extractLevel(x, trim=TRUE)
		out
	}

	# Function to extract attributes to the levels:
	extractAttributes <- function(x, only.required=TRUE){
		extractOne <- function(x){
			getName <- function(y, only.required=TRUE){
				name <- getField(y)
				use <- getField(y, "use")
				if(only.required==TRUE){
					name <- name[use=="required"]
				}
				name <- name[!is.na(name)]
				name
			}
		
			if("simpleContent" %in% names(x)){
				out <- getName(x$simpleContent$extension$attribute)
			}
			else{
				out <- unname(unlist(lapply(x[names(x) == "attribute"], getName, only.required=only.required)))
			}
		}
		out <- lapply(x, extractOne)
		names(out) <- extractLevel(x, trim=TRUE)
		out
	}
	
	# Function for extracting the numeric levels, where 1 is the top level:
	extractLevelNum <- function(x){
		# Get the data types and the levels:
		type <- extractType(x)
		lev <- extractLevel(x, trim=FALSE)
	
		# Declare the numeric levels to output:
		levelNum <- double(length(type))
		
		# Get the links of levels in the types:
		loc <- rep(NA, length(levelNum))
		for(i in seq_along(levelNum)){
			# Locate the current level in the list of types:
			temp <- which(sapply(type, function(x) lev[i] %in% x))
			if(length(temp)){
				loc[i] <- head(temp, 1)
			}
		}
		
		# Reverse the order of the loop if the lowest level comes first:
		locseq <- seq_along(loc)
		if(sum(locseq[!is.na(loc)]) < sum(loc[!is.na(loc)])){
			locseq <- rev(locseq)
		}
		
		# The value of currentMax is used when the level is not found in the list 'type':
		currentMax <- 0
		
		# Run through 'lev' and find it in 'type':
		for(i in locseq){
			# Locate the current level in the list of types:
			loc <- which(sapply(type, function(x) lev[i] %in% x))
			# If not found, use the currentMax:
			if(length(loc)==0){
				levelNum[i] <- currentMax + 1
			}
			# Otherwise use the numeric level of the location:
			else{
				levelNum[i] <- levelNum[min(loc)] + 1
			}
			currentMax <- max(levelNum)
		}
		levelNum
	}

	# Function to extract the variable names:
	extractVariables <- function(x){
		extractOne <- function(x){
			#unname(sapply(x$sequence, function(y) if(is.list(y)) y$.attrs["name"] else y["name"]))
			unname(sapply(x$sequence, getField))
		}
		out <- lapply(x, extractOne)
		names(out) <- extractLevel(x, trim=TRUE)
		out
	}

	extractAllVariables <- function(x){
		vars <- extractVariables(complexType)
		attrs <- extractAttributes(complexType, only.required=FALSE)
		out <- lapply(seq_along(vars), function(i) c(vars[[i]], attrs[[i]]))
		names(out) <- names(vars)
		out
	}

	# Get the xml version:
	ver <- xml_data$.attrs["targetNamespace"]

	# Extract only complexType:
	complexType <- xml_data[names(xml_data)=="complexType"]
	# Discard the simpleContent:
	if(discardSimple){
		hasSimpleContent <- sapply(complexType, function(x) "simpleContent" %in% names(x))
		complexType <- complexType[!hasSimpleContent]
	}

	# Get the variables:
	vars <- extractAllVariables(complexType)

	# Get the types:
	#type <- extractType(complexType)
	
	# Get the attributes:
	attrs <- extractAttributes(complexType, only.required=FALSE)
	attrs_required <- extractAttributes(complexType)
	
	# Get the numeric levels:
	level <- extractLevel(complexType)
	levelNum <- extractLevelNum(complexType)
	
	# Create a data frame with with (1) variable names, ()2 variable names added type, (3) numeric level, (4) attribute/variable type, and (level string) as columns:
	
	# Unlist the variables 'vars':
	varsVec <- unname(unlist(vars))
	nvar <- sapply(vars, length)
	# Create level vectors of the same length as the number of variables:
	levelVec <- rep(level, nvar)
	levelNumVec = rep(levelNum, nvar)
	# Append the level to the var names to obtain unique names:
	vars.levelVec <- paste(varsVec, levelVec, sep=".")
	
	# Create the type vector, which has values "Var", "Attr" and "ReqAttr":
	isFirstInSecondList <- function(x, y){
		unlist(lapply(seq_along(x), function(i) x[[i]] %in% y[[i]]))
	}
	areAttr <- isFirstInSecondList(vars, attrs)
	areAllAttrs_required <- isFirstInSecondList(vars, attrs_required)
	typeVec <- rep("Var", length(levelNumVec))
	typeVec[areAttr] <- "Attr"
	typeVec[areAllAttrs_required] <- "AttrReq"
	
	
	# Order the lists by levelNum:
	vars <- vars[levelNum] 
	attrs <- attrs[levelNum] 
	attrs_required <- attrs_required[levelNum] 
	level <- level[levelNum] 
	levelNum <- seq_along(levelNum)
	
	# The data frame returned:
	x <- data.frame(
		Var = varsVec, 
		Var.Level = vars.levelVec, 
		Level = levelVec, 
		LevelNum = levelNumVec, 
		Type = typeVec, 
		String = paste0("Level", levelNumVec, ".", typeVec), 
		stringsAsFactors=FALSE
	)
	
	# Output:
	list(
		x = x, 
		vars = vars, 
		attrs = attrs, 
		attrs_required = attrs_required, 
		level = level, 
		levelNum = levelNum, 
		ver = ver)
}
#'
#' @export
#' @rdname writeBioticXML
#' 
validateHIXSD <- function(x, xsd, strict=TRUE, discardSimple=FALSE){
	
	# Function for detecting presence of required attributes:
	getMissingAttrReq <- function(y){
		missing <- NULL
		if(any(y$Present)){
			missing <- y$Type == "AttrReq" & !y$Present
			missing <- y$Var.Level[missing]
		}
		missing
	}
	# Get the row indices in the xsd for the variables in x:
	getRowInd <- function(x, xsd){
		# Assign levels and attribute/variable type to the column names:
		varNamesBeforeDotDot <- sapply(strsplit(names(x), "..", fixed=TRUE), head, 1)
		pmax(match(varNamesBeforeDotDot, xsd$x$Var), match(varNamesBeforeDotDot, xsd$x$Var.Level), na.rm=TRUE)
	}
	
		
	# Read the xsd:
	if(is.character(xsd) && file.exists(xsd)){
		xsd <- readHIXSD(xsd, discardSimple=discardSimple)
	}
	
	# Assign levels and attribute/variable type to the column names:
	rowIndInXSD <- getRowInd(x, xsd)
	# Order the data by the appearence in the XSD and then update the row indices:
	x <- x[,order(rowIndInXSD)]
	rowIndInXSD <- getRowInd(x, xsd)
	
	# Find indices and variable names of the variables that are not present in the XSD:
	indNotInXSD <- which(is.na(rowIndInXSD))
	NotInXSD <- names(x)[indNotInXSD]
	
	# Find indices of the variables that are present in the XSD:
	indInXSD <- which(!is.na(rowIndInXSD))
	# Get the XSD info of the variables present (including info about variable/attribute):
	rowIndInXSD_clean <- rowIndInXSD[!is.na(rowIndInXSD)]
	# Add a column with present/absence of the variables in the data:
	Present <- logical(nrow(xsd$x))
	Present[rowIndInXSD_clean] <- TRUE
	xsd$x <- cbind(xsd$x, Present=Present)
	
	# Detect presence in each category of data:
	MissingAttrReq <- unlist(by(xsd$x, xsd$x$Level, getMissingAttrReq))
	if(length(MissingAttrReq)){
		warning(paste0("The following required attributes are missing (given as Variable.Level): ", paste(MissingAttrReq, collapse=", ")))
	}
	
	# Add the "Level.Type" to the column names of x for the variables present in the XSD:
	names(x)[indInXSD] <- paste(xsd$x$String[rowIndInXSD_clean], names(x)[indInXSD], sep=".")
	# If there are variables/attributes that are present in the data but not in the XSD, issue a warning
	if(strict && length(indNotInXSD)){
		warning(paste0("The following variables are not specified in the xsd and were removed (use strict=FALSE to keep the variables, and in that case these need the prefix Level[num].[type], where num is the level in the hierarchy and type is either 'Var', 'Attr' or 'AttrReq'): ", paste(NotInXSD, collapse=", ")))
		x <- x[indInXSD]
	}
	
	# Return a list of the invalid variables, their indices in the list of variables, and the xsd: 
	list(
		indNotInXSD = indNotInXSD, 
		NotInXSD = NotInXSD, 
		indInXSD = indInXSD, 
		x = x, 
		xsd = xsd, 
		MissingAttrReq = MissingAttrReq
	)	
}
#'
#' @importFrom XML newXMLNode saveXML xmlAttrs
#' @export
#' @rdname writeBioticXML
#' @keywords internal
#' 
writeHIXML <- function(x, file, root, addVersion=TRUE, xsd=NULL, na.rm=TRUE, declaration="<?xml version=\"1.0\" encoding=\"UTF-8\"?>", strict=TRUE, discardSimple=FALSE){
	
	# Read the xsd:
	xsd <- readHIXSD(xsd, discardSimple=discardSimple)

	# Define the root:
	root <- XML::newXMLNode(root)
	if(addVersion && !is.na(xsd$ver)){
		XML::xmlAttrs(root) <- list(xmlns = xsd$ver)
	}

	# Write the biotic data to the tempfile_biotic:
	x <- data.frame2xml(x=x, root=root, xsd=xsd, na.rm=na.rm, strict=strict, discardSimple=discardSimple)

	# Save the xml file, adding the prefix (which does not work in saveXML( so we need to use cat())):
	XMLstring <- XML::saveXML(x)
	XMLstring <- paste0(declaration, "\n", XMLstring)
	cat(XMLstring, file=file) 
	
	return(file)
}
#'
#' @export
#' @rdname writeBioticXML
#' @keywords internal
#' 
data.frame2nestedList <- function(x, pre=NULL, levelnames=NULL, rename=NULL, na.rm=FALSE, lastlevel=NULL){
	
	# Small function for removing missing values if requested:
	rm.na <- function(x, na.rm=TRUE){
		if(na.rm){
			x[!is.na(x)]
		}
		else{
			x
		}
	}
	
	# Small function for removing missing values if requested:
	rm.empty <- function(x, keepAttr=TRUE){
		valid <- sapply(x, length)>0
		if(keepAttr){
			hasAttributes <- sapply(x, function(y) length(attributes(y))>0)
			valid <- valid | hasAttributes
		}
		#x[sapply(x, length)>0]
		x[valid]
	}
	
	# Function to set attributes 'att' to object 'x':
	setAttributes <- function(x, att){
		oldatt <- attributes(x)
		attributes(x) <- c(att, oldatt)
		x
	}
	
	# Function for appending a list 'add' to another list 'x', while keepeing the list names:
	appendToListKeepAttr <- function(x, add){
		# Store the attributes, and reinsert using the setAttributes() function after merging:
		att <- attributes(x)
		x <- c(x, add)
		setAttributes(x, att)
	}
	
	# Function for adding attributes stored in the names, which are ..AttrName.AttrValue:
	extraxtAndSetDotDotAttributes <- function(x){
		# Split the names by ".." and find those elements containing attributes:
		thisNamesSplit <- strsplit(names(x), "..", fixed=TRUE)
		indAtAttributes <- which(sapply(thisNamesSplit, length) > 1)
		
		if(length(indAtAttributes)){
			# Extract the attributes stored in the names, which are ..AttrName.AttrValue:
			newNames <- lapply(thisNamesSplit[indAtAttributes], "[", 1)
			dotDotAttrs <- lapply(thisNamesSplit[indAtAttributes], "[", 2)
			dotDotAttrsParts <- lapply(dotDotAttrs, function(x) strsplit(x, ".", fixed=TRUE)[[1]])
			dotDotAttrsNames <- sapply(dotDotAttrsParts, "[", 1)
			dotDotAttrsVals <- lapply(dotDotAttrsParts, "[", 2)
			names(dotDotAttrsVals) <- dotDotAttrsNames
			
			# Add the attributes:
			x[indAtAttributes] <- lapply(seq_along(indAtAttributes), function(i) setAttributes(x[[indAtAttributes[i]]], dotDotAttrsVals[i]))
			names(x)[indAtAttributes] <- newNames
		}
			
		return(x)
	}
	
	# Function used for extracting the variables and setting attributes to the list:
	getVarSetAttr <- function(x, Attr, Var, pre=NULL, onlyfirst=TRUE, method=c("include", "append"), rename=NULL, na.rm=FALSE, varAttr=NULL){
		
		# Function to extract the names (removing the key strings) and alter the remainder by the 'rename' list:
		alterNames <- function(x, keys=c(".Var.", ".Attr.", ".AttrReq.", fixed=TRUE), rename=NULL){
			# Function for replacing names in 'x' by the list 'rename', which is a list named by the old names and holding the new names:
			replaceNames <- function(x, rename){
				rename <- unlist(rename)
				namesInX <- names(rename)
				newNames <- rename
	
				present <- namesInX %in% names(x)
				if(any(present)){
					names(x)[(names(x) %in% namesInX[present])] <- newNames[present]
				}
	
				x
			}
	
			# Get the names of the data frame:
			n <- names(x)
			# Grep the keys:
			at <- lapply(keys, function(y) regexpr(y, n))
			# Get the position and length of the last key string:
			len <- do.call(pmax, lapply(at, attr, "match.length"))
			at <- do.call(pmax, at)
			# Extact the variable name:
			n <- substring(n, at + len)
			names(x) <- n
			# Rename:
			replaceNames(x, rename)
		}
		
		# Function for extracting the variables and setting attributes of one row:
		getVarSetAttrOne <- function(x, Var, Attr, na.rm=FALSE, varAttr=NULL, rename=NULL){
			# Remove NAs if requested:
			this <- as.list(rm.na(x[Var], na.rm=na.rm))
			
			# Add attributes stored in the names past the "..":
			this <- extraxtAndSetDotDotAttributes(this)
			# Extract, alter and set the attributes given as columns:
			att <- x[Attr]
			att <- alterNames(att, rename=rename)
			# Set the attributes:
			setAttributes(this, att)
		}
		
		# If onlyfirst=TRUE extract the first row only:
		if(onlyfirst){
			x <- x[1, , drop=FALSE]
		}
	
		# Get a list of the children:
		this <- apply(x, 1, getVarSetAttrOne, Var=Var, Attr=Attr, na.rm=na.rm, varAttr=varAttr, rename=rename)
		# Remove empty resulting from all NAs:
		this <- rm.empty(this)
		# Return if all elements were removed:
		if(length(this)==0){
			return(this)
		}
		
		# Alter names (see alterNames() for how the key strings "Var" and "Attr"/"AttrReq" are removed):
		this <- lapply(this, alterNames, rename=rename)
		#this[[1]] <- alterNames(this[[1]], rename=rename)
		if(onlyfirst && startsWith(method[1], "i")){
			this <- this[[1]]
		}
		
		# Append only if 'pre' is given:
		if(length(pre)){
			appendToListKeepAttr(pre, this)
		}
		return(this)
	}
	
	# Get the column names, which are of the form Level[num].[type].[name], where 'num' is the level of the variables, 'type' is one of "Attr" and "Var", and 'name' is the variable name:
	name <- names(x)
	
	# Remove any columns which do not start with "Level.":
	valid <- startsWith(name, "Level")
	if(!all(valid)){
		x <- x[, valid]
		name <- name[valid]
	}
	
	# Split by dot:
	name.parts <- strsplit(name, ".", fixed=TRUE)
	
	# Get the level string, and get the indices of the variables at the current highest level (ordered first in the data frame):
	levelstring <- sapply(name.parts, "[", 1)
	# length("level") = 5, so the integer is from character 6 and to the end of the string:
	levels <- as.numeric(substring(levelstring, 6))
	maxlevel <- max(levels)
	#thislevel <- levels[thisind[1]]
	thislevel <- min(levels)
	#nextlevel <- min(levels[levels!=thislevel])
	
	thisind <- which(levels == thislevel)
	
	# Get the variable types, either "Attr" or "Var":
	type <- sapply(name.parts, "[", 2)
	# Get the key:
	Attr <- thisind[startsWith(type[thisind], "Attr")]
	Var <- thisind[startsWith(type[thisind], "Var")]
	if(length(Attr)==0){
		warning(paste0("Level " , thislevel, " had no attributes specified in the xsd (", paste(name[levels==thislevel]), ")"))
	}
	# If we have not reached the maximum level og the data frame, split by the attributes, get the variables and add the attributes, and append the next level using data.frame2nestedList():
	if(thislevel<maxlevel){
		# Split by the attribute columns:
		if(length(Attr)){
			temp <- split(x, x[Attr], drop=TRUE)
		}
		else{
			temp <- list(x)
		}
		
		# Define the list of elements of the current level:
		out <- vector("list", length(temp))
		names(out) <- rep(levelnames[thislevel], length(out))
		
		# Append to the result so far in the recursion:
		out <- appendToListKeepAttr(pre, out)
		lpre <- length(pre)
		
		# Run through the data frames of unique attributes:
		for(i in seq_along(temp)){
			## Get the variables of the current data frame, and add the attributes:
			this <- getVarSetAttr(temp[[i]], Attr=Attr, Var=Var, rename=rename, na.rm=na.rm, varAttr=varAttr)
			
			# Run the appending funciton, but exclude the variables/attributes at the present level:
			out[[i+lpre]] <- data.frame2nestedList(temp[[i]][-thisind], pre=this, levelnames=levelnames, rename=rename, na.rm=na.rm, lastlevel=thislevel)
		}
		
		# If there are missing levels, add these here:
		if(length(lastlevel) && thislevel > lastlevel + 1){
			# The names of the missing levels:
			nameVec <- levelnames[seq(lastlevel + 1, thislevel - 1)]
			
			# Add the first missing level, and continue if there are more than one:
			temp <- setNames(list(out[seq_along(temp) + lpre]), nameVec[1])
			nameVec <- nameVec[-1]
			
			# Add the levels which are not present in the data but present in the sequence of levels:
			while(length(nameVec)){
				temp <- setNames(list(out[seq_along(temp) + lpre]), nameVec[1])
				nameVec <- nameVec[-1]
			}
			# Append to the pre data:
			out <- c(out[seq_along(lpre)], temp)
		}
		
	}
	# Otherwise, the lowest level has been reached, and we simply append 
	else{
		out <- getVarSetAttr(x, Attr=Attr, Var=Var, pre=pre, onlyfirst=FALSE, rename=rename, na.rm=na.rm, varAttr=varAttr)
		names(out) <- rep(levelnames[thislevel], length(out))
		out <- appendToListKeepAttr(pre, out)
	}
	out
}
#'
#' @export
#' @importFrom XML xmlAttrs newXMLNode xmlValue
#' @rdname writeBioticXML
#' @keywords internal
#' 
list2XML <- function(node, sublist){
	
	# Function stolen from the xml2 package:
	r_attrs_to_xml <- function(x) {
		special_attributes <- c("class", "comment", "dim", "dimnames", "names", "row.names", "tsp")
	
		if (length(x) == 0) {
			return(NULL)
		}
	
		# Drop R special attributes
		x <- x[!names(x) %in% special_attributes]
	
		# Rename any xml attributes needed
		special <- names(x) %in% paste0(".", special_attributes)
	
		names(x)[special] <- sub("^\\.", "", names(x)[special])
		x
	}
	
	# Function to add attributes:
	addAttributes <- function(child, x){
		attr <- r_attrs_to_xml(attributes(x))
		if(length(attr)){
			attr <- unlist(attr)
		}
		XML::xmlAttrs(child) <- attr
	}
	
    for(i in seq_along(sublist)){
	    child <- XML::newXMLNode(names(sublist)[i], parent=node);

		# If we are still in the list, recurse
        if(typeof(sublist[[i]]) == "list"){
            list2XML(child, sublist[[i]])
			
			# Add attributes of the group:
			addAttributes(child, sublist[[i]])
		}
        else{
            XML::xmlValue(child) <- sublist[[i]]
			
			# Add attributes of the value:
			addAttributes(child, sublist[[i]])
		}
    } 
	node
}
#'
#' @export
#' @rdname writeBioticXML
#' @keywords internal
#' 
data.frame2xml <- function(x, root, xsd, na.rm=FALSE, strict=TRUE, discardSimple=FALSE){
	
	# Validate the input by the xsd:
	temp <- validateHIXSD(x, xsd, strict=strict, discardSimple=discardSimple)
	x <- temp$x
	xsd <- temp$xsd
	
	# Prepare to rename all names of the form varname.level (the renaming appears in the resulting nested list):
	rename <- xsd$x$Var
	names(rename) <- xsd$x$Var.Level
	
	# Convert the data frame to a nested list:
	l <- data.frame2nestedList(x, levelnames=xsd$level, rename=rename, na.rm=na.rm)
	# Check that the top name of the nested list does not coincide with the root name:
	if(length(l)==1 && xmlName(root) == names(l)){
		l <- l[[1]]
	}

	# Convert to xml:
	x <- list2XML(root, l)
	x
}

