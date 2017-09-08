#*********************************************
#*********************************************
#' Create, open or save a StoX project
#'
#' \code{createProject} creates a new StoX project (from xml files). \cr \cr
#' \code{openProject} opens a StoX project. If the project has already been opened, \code{openProject} will only retrieve the project object from the RstoxEnv enviromnent. To force open the project use \code{reopenProject}. \cr \cr
#' \code{reopenProject} re-opens a StoX project, which is equivalent to closeing and then opening the project. \cr \cr
#' \code{getProject} gets the project object, either from the input if this is a baseline or project object, or from the project environment. \cr \cr
#' \code{updateProject} updates links to xml files in a project. \cr \cr
#' \code{saveProject} saves a StoX project. This implies to save to the project.XML file all changes that are made to the project environment, such as changes in parameter values through the "..." input to runBaseline(). Such changes are only implemented in the project environment (in R memory), and will not be saved to the project.XML file unless saveProject() is run. \cr \cr
#' \code{closeProject} removes the project from memory. \cr \cr
#' \code{isProject} checks whether the project exists on file. \cr \cr
#' \code{readXMLfiles} reads XML data via a temporary project. \cr \cr
#'
#' @param projectName   	The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param files   			A list with elements named "acoustic", "biotic", "landing", "process" (holding the project.xml file) or other implemented types of data to be copied to the project (available data types are stored in StoX_data_types in the environment "RstoxEnv". Get these by get("StoX_data_types", envir=get("RstoxEnv"))). These could be given as directories, in which case all files in those directories are copied, or as URLs. If given as a single path to a directory holding sub-directories with names "acoustic", "biotic", "landing", "process" or other implemented types of data, the files are copied from these directories. If files has length 0 (default), the files present in the project directory are used, if already existing (requires to answer "y" when asked to overwrite the project if ow=NULL, or alternatively to set ow=TRUE).
#' @param dir				The directory in which to put the project. The project is a directory holding three sub directories named "input", "output" and "process", where input, output and process files are stored.
#' @param model   			The model to use, either given as a string specifying a template, or a vector of process names or list of processes given as lists of parameter specifications (see \code{parlist}). Show available templates with createProject().
#' @param ow   				Specifies whether to ovewrite existing project: If TRUE, overwrite; if FALSE, do not overwrite; if NULL (default), aks the user to confitm overwriting.
#' @param open   			Logical: if TRUE (defalut) open the project in memory.
#' @param ignore.processXML	Logical: if TRUE do not copy any project.XML file given in \code{files} to the project.
#' @param parlist,...   	See \code{\link{runBaseline}}.
#' @param out   			One of "project", "baseline" or "name" (project name) (only first character used), specifying the output.
#' @param nchars			The number of characters to read when determining the types of the files in readXMLfiles().
#' @param msg				Logical: If TRUE print messages to the console.
#'
#' @examples
#' # Show templates:
#' createProject()
#' # Create the test project:
#' createProject("Test_Rstox", files=system.file("extdata", "Test_Rstox", package="Rstox"), ow=FALSE)
#'
#' # Read xml file directly from any location:
#' xmlfiles <- system.file("extdata", "Test_Rstox", package="Rstox", "input")
#' list.files(xmlfiles, recursive=TRUE)
#' dat <- readXMLfiles(xmlfiles)
#'
#' @return A project object
#' \code{createProject} returns the path to the StoX project directory. \cr \cr
#' \code{openProject} returns the object specified in 'out'. \cr \cr
#' \code{reopenProject} returns the object specified in 'out'. \cr \cr
#' \code{getProject} returns the project object. \cr \cr
#' \code{updateProject} returns TRUE for success and FALSE for no XML files linked to reading functions. \cr \cr
#' \code{saveProject} returns the project object. \cr \cr
#' \code{closeProject} returns TRUE if the project was open and FALSE if not. \cr \cr
#' \code{isProject} returns TRUE if the project exists on file and FALSE if not. \cr \cr
#' \code{readXMLfiles} returns the XML data. \cr \cr
#' 
#' @importFrom rJava J
#' @export
#' @rdname createProject
#' 
createProject <- function(projectName=NULL, files=list(), dir=NULL, model="StationLengthDistTemplate", ow=NULL, open=TRUE, ignore.processXML=FALSE, parlist=list(), ...){
	##### Functions: #####
	# Return available templates as default:
	getTemplates <- function(){
		templates <- J("no.imr.stox.factory.Factory")$getAvailableTemplates()$toArray()
		descriptions <- sapply(templates, J("no.imr.stox.factory.Factory")$getTemplateDescription)
		cbind(Template=templates, Description=unname(descriptions))
	}
	matchTemplates <- function(template, availableTemplates){
		availableTemplates[ which(tolower(substr(availableTemplates, 1, nchar(template))) == tolower(template)) ]
	}
	# Function used for detecting URLs:
	#isURL <- function(x, URLkeys=c("ftp:", "www.", "http:")){
	#	seq_along(x) %in% unique(unlist(lapply(URLkeys, grep, x=x, fixed=TRUE)))
	#}
	# Function used for copying data and process file to the project:
	getFiles <- function(files, StoX_data_types){
		if(length(files)==1 && is.character(files)){
			if(isTRUE(file.info(files)$isdir)){
				dirs <- list.dirs(files, full.names=TRUE)
				# Select the element with valid names:
				valid_dirs <- tolower(basename(dirs)) %in% StoX_data_types
				files <- lapply(dirs[valid_dirs], list.files, recursive=TRUE, full.names=TRUE)
				names(files) <- tolower(basename(dirs[valid_dirs]))
			}
		}
		files
	}
	# Function used for copying data and process file to the project:
	copyFilesToStoX <- function(data_types, files, dirs){
		# Select only the valid data types:
		for(i in seq_along(data_types)){
			x <- files[[data_types[i]]]
			if(length(x)){
				x <- c(x[file.info(x)$isdir %in% FALSE], list.files(x, full.names=TRUE, recursive=TRUE))
				file.copy(x, dirs[i])
			}
		}
	}
	##### End of functions: #####
	
	##############################################################################################
	##### 1. Initiate Rstox, get templates and project name, root and directory (full path): #####
	##############################################################################################
	# J() and reference to "no/imr/stox/model/Project" requires	Rstox.init():
	Rstox.init()
	
	availableTemplates <- getTemplates()
	if(length(projectName)==0){
		return(availableTemplates)
	}
	
	# Set the project name and the root directory of the project:
	projectPaths <- getProjectPaths(projectName, dir=dir)
	projectName <- projectPaths$projectName
	dir <- projectPaths$projectRoot
	projectPath <- projectPaths$projectPath
	##############################################################################################
	##############################################################################################
	
	
	
	#################################
	##### 2. Treat overwriting: #####
	#################################
	if(file.exists(projectPath)){
		if(length(ow)==0){
			ans <- readline(paste0("Project \"", projectPath, "\" already exists. Overwrite? (y/n)\n"))
			if(ans!="y"){
				cat("Not overwriting:", projectPath, "\n")
				return()
			}
		}
		else if(!ow){
			cat("Not overwriting:", projectPath, "\n")
			return()
		}
		# Delete the existing project if the function did not exit:
		unlink(projectPath, recursive=TRUE, force=TRUE)
		# Also remove the projet from R memory, that is delete the project environment:
		closeProject(projectName)
	}
	#################################
	#################################
	
		
	############################################
	##### 3. Apply the specified template: #####
	############################################
	userDefined <- is.list(model) || (length(model)>0 && !model[1] %in% availableTemplates[,1])
	# Select the template given the user input:
	if(userDefined){
		template <- matchTemplates("UserDefined", availableTemplates[,1])
	}
	else if(length(model)){
		# Find the templates that match the available tempaltes case insensitively and using abbreviation:
		template <- matchTemplates(model[1], availableTemplates[,1])
		if(length(template)>1){
			template <- template[1]
			warning(paste0("Multiple templates matched. The first used (", template, ")"))
		}
		 else if(length(template)==0){
			warning(paste0("'template' matches no templates. Run createProject() to get a list of available tempaltes. Default used (", "StationLengthDist", ")"))
			template <- matchTemplates("StationLengthDist", availableTemplates[,1])
		}
	}
	else{
		template <- matchTemplates("StationLengthDist", availableTemplates[,1])
	}
	############################################
	############################################
	
	
	########################################################
	##### 4. Generate folder structure and copy files: #####
	########################################################
	# Create the project, that is create in memory and set ut the folder structure. This writes folders to the directory 'dir'!:
	project <- J("no.imr.stox.factory.FactoryUtil")$createProject(dir, projectName, template)
	
	# Copy files to the project directory:
	StoX_data_types <- getRstoxEnv()$StoX_data_types
	StoXdirs <- file.path(projectPath, "input", StoX_data_types)
	# Add the process directory to allow for the project.xml file to be given in the input 'files':
	StoX_data_types <- c(StoX_data_types, "process")	 
	StoXdirs <- c(StoXdirs, file.path(projectPath, "process"))	 

	# Get the files:
	files <- getFiles(files, StoX_data_types)
	if(ignore.processXML){
		files$process <- NULL
		}
	# Copy the files 
	if(length(files) && is.list(files)){
		copyFilesToStoX(StoX_data_types, files, dirs=StoXdirs)
	}
	
	# Save the project if no project.xml file was given. This is done in order to open the project in the next step using openProject(), which is needed to create the project environment. Here we need to the project object, since that is what we wish to save. If we used the project name, getProject() used in saveProject() would look for the project object in the project environment, which we have not created yet:
	if(length(files$process)==0){
		saveProject(project)
	}
	
	# Open the project with the project.xml file copied to the prodect directory or generated from the template given in 'model':
	project <- openProject(projectPath, out="project")
	# Update the xml files containg the data. This is done to make sure the files are pointed to in the project even after moving files (for example if the full path was used in the project.xml file copied to the project). This is only effectice if the model includes StoX reading function such as readBioticXML:
	updateProject(projectPath)
	########################################################
	########################################################
	
	
	#####################################################
	##### 5. Add the processes specified in 'model' #####
	#####################################################
	# Specify the model if a valid template was not given:
	if(userDefined){
		if(length(files$process)){
			warning(paste0("a project.xml file was copied to the project ", projectPath, ", and any model specification given in 'model' is ignored (ignore.processXML = TRUE can be used to discard the project.xml file)"))
		}
		else{
			# Get the list of parameters specified either in 'model', or in 'parlist' or '...' in addition (usually only used then a template is used so that userDefined=FALSE, but kept here for robustness):
			parlist <- getParlist(parlist=c(model, parlist), ...)
			# Add the processes. Set the parameter values below:
			addProcesses(project, parlist)
		}
	}
	else{
		parlist <- getParlist(parlist=parlist, ...)
	}
	
	# Override parameters in the project:
	if(length(parlist)){
		# Re-open the project in order to sucessfully set the parameters in the 'parlist':
		saveProject(projectPath)
		reopenProject(projectPath)
		setBaselineParameters(projectPath, parlist=parlist, msg=FALSE, save.project=TRUE)
	}
	
	# Finally, save all changes to the project.xml file:
	saveProject(projectPath)
	#####################################################
	#####################################################
	
	
	# Return the project directory:
	projectPath
}
#' 
#' @importFrom rJava J .jnew
#' @export
#' @rdname createProject
#' 
openProject <- function(projectName=NULL, out=c("project", "baseline", "name"), msg=FALSE){
	# If nothing is given return a list of the projects in the StoX project directory:
	if(length(projectName)==0){
		Rstox.init()
		return(list.files(J("no.imr.stox.functions.utils.ProjectUtils")$getSystemProjectRoot()))
	}
	
	# Get the project Java object, possibly retrieved from the project environment (getProject() uses getProjectPaths() if a character is given):
	project <- getProject(projectName, msg=msg)
	

#	
#   - projectName can be the name of a folder in the workspace or a full path to the project
#
#   - Java$openProject() and Java$createProject() both take workspace and projectName as parameters, where projectName is really basename(projectName).
#
#   - getWorkspace() should take both a dir and projectName input, where dir is extracted from projectName by dirname(projectName) in the case that isTRUE(file.info(projectName)$isdir)
	
	
	# Otherwise, open the project, generate the project object, and save it to the RstoxEnv evnironment:
	if(length(project)==0){
		# If the project exists on file, open in Java and R memory:
		if(!isProject(projectName)){
			warning(paste0("The StoX project ", projectName, " does not exist"))
			return(NULL)
		}
		else{
			projectPaths <- getProjectPaths(projectName)
			projectName <- projectPaths$projectName
			projectRoot <- projectPaths$projectRoot
			projectPath <- projectPaths$projectPath
			########## Open the project in Java memory: ##########
			Rstox.init()
			project <- J("no.imr.stox.factory.FactoryUtil")$openProject(projectRoot, projectName)
			# This line was added on 2017-08-23 due to a bug discovered when estimating the area of polygons using the "accurate" method. When baseline is run, Java calls the function polyArea() when AreaMethod="accurate". However, whenever this failed, the simple method implemented in Java was used (which was a bug). The problem was that the path to the R-bin was not set in Rstox. To solve this, a file holding this path (or the command project$setRFolder(PATH_TO_R_BIN)) will be saved by StoX, and called every time a triggerscript is run. In Rstox we solve the problem by the below sequence:
			RFolder <- project$getRFolder()
			if(length(RFolder)==0 || (is.character(RFolder) && nchar(RFolder)==0)){
				project$setRFolder(R.home("bin"))
			}
			############################################### ######
			
			########## Open the project in R memory: ##########
			# Create a list for the project in the environment 'RstoxEnv':
			parameters <- readBaselineParameters(projectPath)
			# The first line (temp <- getRstoxEnv()) is needed to assure that the RstoxEnv environment is loaded:
			temp <- getRstoxEnv()
			RstoxEnv[[projectName]] <- list(originalParameters=parameters, javaParameters=parameters, lastParameters=parameters, projectObject=project, projectData=new.env())
			#assign(projectName, list(originalParameters=parameters, javaParameters=parameters, lastParameters=parameters, projectObject=project, projectData=new.env()), envir=getRstoxEnv())
			##assign(getRstoxEnv()[[projectName]], list(originalParameters=parameters, javaParameters=parameters, lastParameters=parameters, projectObject=project, projectData=new.env()))
			###getRstoxEnv()[[projectName]] <- list(originalParameters=parameters, javaParameters=parameters, lastParameters=parameters, projectObject=project, projectData=new.env())
			# Also add the last used parameters to the projectData, in order to save this to file. 
			##### NOTE (2017-08-28): This is different from the 'lastParameters' object in the project environment (rstoxEnv[[projectName]]), and is only used when writing project data such as bootstrap results to file: #####
			setProjectData(projectName=projectName, var=parameters, name="lastParameters")
	
			# As of version 1.4.2, create the new folder structure:
			suppressWarnings(dir.create(projectPaths$RDataDir, recursive=TRUE))
			suppressWarnings(dir.create(projectPaths$RReportDir, recursive=TRUE))
			###################################################
		}
	}
	
	# Return a baseline object:
	if(tolower(substr(out[1], 1, 1)) == "b"){
		return(project$getBaseline())
	}
	# Return the project object:
	else if(tolower(substr(out[1], 1, 1)) == "p"){
		return(project)
	}
	# Return the project name:
	else if(tolower(substr(out[1], 1, 1)) == "n"){
		return(project$getProjectName())
	}
	else{
		warning("Invalid value of 'out'")
		return(NULL)
	}
}
#' 
#' @export
#' @rdname createProject
#' 
reopenProject <- function(projectName, out=c("project", "baseline", "name")){
	closeProject(projectName)
	openProject(projectName,  out=out)
}
#' 
#' @importFrom rJava J .jnew
#' @export
#' @rdname createProject
#' 
getProject <- function(projectName, msg=FALSE){
	# Return immediately if a project or baseline object is given:
	if(class(projectName) == "jobjRef"){
		if(projectName@jclass=="no/imr/stox/model/Project"){
			return(projectName)
		}
		else if(projectName@jclass=="no/imr/stox/model/Model"){
			return(projectName$getProject())
		}
	}
	# Check for the existence of the project object in the RstoxEnv evnironment (getProjectPaths(projectName)$projectName assures that the project name is used and not the full project path if given in 'projectName'):
	#else if(is.character(projectName) && nchar(projectName)>0 && length(getRstoxEnv()[[getProjectPaths(projectName)$projectName]]$projectObject)>0){
	else if(is.character(projectName) && nchar(projectName)>0){
		projectName <- getProjectPaths(projectName)$projectName
		if(length(getRstoxEnv()[[projectName]]$projectObject)){
			if(msg){
				warning(paste0("Project retrieved from the RstoxEnv$", projectName, " enviroment. To reopen the project use reopenProject(", projectName, ")"))
			}
			return(getRstoxEnv()[[projectName]]$projectObject)
		}
		else{
			return(NULL)
		}
		#return(getRstoxEnv()[[projectName]]$projectObject)
	}
	else{
		return(NULL)
	}
}
#' 
#' @export
#' @rdname createProject
#' 
updateProject <- function(projectName){
	# Set the project name and the root directory of the project:
	projectPaths <- getProjectPaths(projectName)
	if(file.exists(projectPaths$projectPath)){
		pointToStoXFiles(projectName)
		TRUE
	}
	else{
		FALSE
	}
}
#' 
#' @export
#' @rdname createProject
#' 
saveProject <- function(projectName){
	project <- getProject(projectName)
	if(length(project)){
		project$save()
	}
	else{
		warning(paste("Project", projectName, "is not open, and cannot be saved."))
	}
	return(project)
}
#' 
#' @export
#' @rdname createProject
#' 
closeProject <- function(projectName){
	projectName <- getProjectPaths(projectName)$projectName
	# Remove the project list:
	if(length(getRstoxEnv()[[projectName]])){
		rm(list=projectName, envir=getRstoxEnv())
		TRUE
	}
	else{
		warnings(paste0("The project \"", projectName, "\" is not open"))
		FALSE
	}
}
#' 
#' @export
#' @rdname createProject
#' 
isProject <- function(projectName){
	#	1. Look for the project if given by the full path
	#	2. Look for the project in the default root
	#	3. Return if not
	# Function for checking whether all the folders given in getRstoxEnv()$StoX_data_types are present in the directory:
	hasStoX_data_types <- function(projectName){
		projectInfo <- file.info(projectName)
		if(isTRUE(projectInfo$isdir)){
			dirs <- list.dirs(projectName, full.names=FALSE, recursive=FALSE)
			if(all(getRstoxEnv()$StoXFolders %in% dirs)){
				return(TRUE)
			}
			else{
				warning(paste0("The path ", projectName, " does not contain the required folders (", paste(getRstoxEnv()$StoX_data_types, collapse=", "), ")"))
				return(FALSE)
			}
		}
		else{
			return(FALSE)
		}
	}	
	
	### # Check first the 'projectName' directly (which needs to be a full path, indicated by the !dirname(projectName) %in% c(".", "", "/")):
	### out <- FALSE
	### if(!dirname(projectName) %in% c(".", "", "/")){
	### 	out <- hasStoX_data_types(projectName)
	### }
	### # Then look for the project in the default workspace:
	### if(!out){
	### 	out <- hasStoX_data_types(getProjectPaths(projectName)$projectPath)
	### }
	### out
	
	hasStoX_data_types(getProjectPaths(projectName)$projectPath)
}
#'
#' @export
#' @rdname createProject
#'
readXMLfiles <- function(files, dir=tempdir(), model=list(), nchars=500){
	# Function for extracting the different file types from characteristic strings in the first 'n' characters:
	getFileType <- function(files, nchars=500){
		if(is.list(files)){
			return(files)
		}
		first <- sapply(files, readChar, nchars=nchars)
		out <- lapply(getRstoxEnv()$StoX_data_type_keys, grep, first, ignore.case=TRUE)
		if(sum(sapply(out, length))){
			out <- lapply(out, function(x) files[x])
			names(out) <- getRstoxEnv()$StoX_data_types
		}
		else{
			warning(paste0("No acoustic, biotic or landing XML files detected (using the characteristic strings ", paste(paste0("'", getRstoxEnv()$StoX_data_type_keys, "'"), collapse=", "), " as identifyers for the file types ", paste(paste0("'", getRstoxEnv()$StoX_data_types, "'"), collapse=", ") , ") "))
			out <- list()
		}
		return(out)
	}
	capitalizeFirstLetter <- function(x){
		first <- substring(x, 1, 1)
		rest <- substring(x, 2)
		paste0(toupper(first), rest)
	}
	# Get the list of files if given as a simple vector of file paths:
	if(!is.list(files)){
		if(length(files)==1 && isTRUE(file.info(files)$isdir)){
			dirs <- list.dirs(files, recursive=FALSE, full.names=FALSE)
			# Get the files if given as a directory holding sub directories named "biotic", "acoustic", or "landing":
			if(any(getRstoxEnv()$StoX_data_types %in% dirs)){
				presentDirs <- file.path(files, intersect(getRstoxEnv()$StoX_data_types, dirs))
				files <- lapply(presentDirs, list.files, recursive=TRUE, full.names=TRUE)
				names(files) <- basename(presentDirs)
				files <- files[unlist(lapply(files, length))>0]
			}
			else{
				files <- list.files(files, recursive=TRUE, full.names=TRUE)
			}
		}
		files <- getFileType(files, nchars=nchars)
	}
	
	# Keep only the valid file types:
	files <- files[getRstoxEnv()$StoX_data_types]
	# And only non-empty elements:
	files <- files[sapply(files, length)>0]
	# Expand all paths for StoX to recognize the files:
	files <- lapply(files, path.expand)
	# Abort if no valid files:
	if(length(files)==0){
		return(NULL)
	}
	
	# Convert to a model object as used in createProject():
	readmodel <- lapply(files, as.list)
	readmodel <- lapply(readmodel, function(x) setNames(x, paste0("FileName", seq_along(x))) )
	names(readmodel) <- paste0("Read", capitalizeFirstLetter(names(files)), "XML")
	model <- c(readmodel, model)
	
	# Create a temporary project:
	project <- createProject("tempProject", dir=path.expand(dir), model=model, ow=TRUE)
	
	out <- getBaseline(project, input=NULL, msg=FALSE)
	unlink(project)
	return(out)
}


#*********************************************
#*********************************************
#' Link to the files in a stox project
#'
#' Updates a project with the files located in the "input" directory. Used in updateProject().
#'
#' @param projectName   	The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param files   			A list with elements named "acoustic", "biotic", "landing", "process" (holding the project.xml file) or other implemented types of data to be copied to the project (available data types are stored in StoX_data_types in the environment "RstoxEnv". Get these by get("StoX_data_types", envir=get("RstoxEnv"))). These could be given as directories, in which case all files in those directories are copied, or as URLs. If given as a single path to a directory holding sub-directories with names "acoustic", "biotic", "landing", "process" or other implemented types of data, the files are copied from these directories. If files has length 0 (default), the files present in the project directory are used, if already existing (requires to answer "y" when asked to overwrite the project if ow=NULL, or alternatively to set ow=TRUE).
#'
#' @return A project object
#' 
#' @export
#' @keywords internal
#' 
pointToStoXFiles <- function(projectName, files=NULL){
	# Function used for extracting the files located in a StoX project (getFiles does lapply of getFilesOfDataType):
	getFilesOfDataType <- function(data_type, projectPath){
		# Get the input data folder of the specified data type, and the files in that folder:
		dir <- file.path(projectPath, "input", data_type)
		files <- list.files(dir, full.names=TRUE)
		# Remove project path (2016-11-08):
		gsub(projectPath, "", files, fixed=TRUE)
	}
	getFiles <- function(projectPath, StoX_data_types, files=NULL){
		if(length(files)==0){
			files <- lapply(StoX_data_types, getFilesOfDataType, projectPath)
			names(files) <- StoX_data_types
		}
		if(!all(names(files) %in% StoX_data_types)){
			warning(paste0("'files' must be a list with one or more of the names ", paste(StoX_data_types, collapse=", "), ". Each element of the list must contain a vector of file paths."))
			files <- files[names(files) %in% StoX_data_types]
		}
		lapply(files, path.expand)
	}
	# Function that points to the files[[data_type]] in the project. Lapply this:
	pointToStoXFilesSingle <- function(data_type, project, files){
		# Get the files of the specified type:
		thesefiles <- files[[data_type]]
		# Get the StoX-function name for reading these files:
		fun <- paste0("Read", toupper(substr(data_type, 1, 1)), substring(data_type, 2), "XML")
		for(i in seq_along(thesefiles)){
			proc <- project$getBaseline()$findProcessByFunction(fun)
			if(length(names(proc))){
				proc$setParameterValue(paste0("FileName",i), thesefiles[i])
			}
		}
		thesefiles
	}
	
	#  # Get the project name (possibly interpreted from a project or baseline object):
	#  projectName <- getProjectPaths(projectName)$projectName
	# Open the project:
	project <- openProject(projectName, out="project")
	# Get the currently defined StoX data types:
	StoX_data_types <- getRstoxEnv()$StoX_data_types
	# Get the files if not specified in the input:
	files <- getFiles(project$getProjectFolder(), StoX_data_types, files)
	# Point to the files, save and return:
	out <- lapply(StoX_data_types, pointToStoXFilesSingle, project, files)
	names(out) <- StoX_data_types
	project$save()
	out
}


#*********************************************
#*********************************************
#' Run a StoX baseline model
#' 
#' \code{runBaseline} runs a StoX baseline model possibily overriding parameters. \cr \cr
#' \code{getBaseline} returns input and output data from the StoX baseline model. \cr \cr
#' 
#' @param projectName   The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param startProcess	The name or number of the start process in the list of processes in the model (use info=TRUE to return a list of the processes). The use of startProcess and endProcess requres that either no processes in the given range of processes depends on processes outside of the range, or that a baseline object is given in the input.
#' @param endProcess	The name or number of the end process in the list of processes in the model (use info=TRUE to return a list of the processes).
#' @param reset			Logical; if TRUE rerun the baseline model even if it has been run previously.
#' @param save			Logical; if TRUE changes to the project specified in parlist and "..." are saved in Java and to the object javaParameters in the project list in the RstoxEnv environment.
#' @param out			The object to return from runBaseline(), one of "name" (projectName), "baseline" (Java baseline object) or "project" (Java project object, containing the baseline object).
#' @param msg			Logical: if TRUE print information about the progress of reading the data.
#' @param exportCSV		Logical: if TRUE turn on exporting csv files from the baseline run.
#' @param warningLevel	The warning level used in the baseline run, where 0 stops the baseline for Java warnings, and 1 continues with a warning.
#' @param parlist		List of parameters values overriding existing parameter values. These are specified as processName = list(parameter = value), for example AcousticDensity = list(a = -70, m = 10), BioStationWeighting = list(WeightingMethod = "NASC", a = -70, m = 10). Logical parameters (given as strings "true"/"false" in StoX) can be given as logical TRUE/FALSE.
#' @param ...			Same as parlist, but can be specified separately (not in a list but as separate inputs).
#' @param input			The input data requested in getBaseline(). This is a string vector naming baseline processes and process data. The key words "par" and "proc" returns all parameters and process data, respectively.
#' @param proc			A string vector naming processes from which data should be returned.
#' @param par			A list of the same length as \code{fun} giving parameter values to uniquely identify processes. The list names are the names of the baseline process parameters, and the values are the baseline process values.
#' @param drop			Logical: if TRUE drop empty list elements (default).
#'
#' @return For \code{\link{runBaseline}} theproject name, and for \code{\link{getBaseline}} a list of three elements named "parameters", "output", "processData", where empty elements can be dropped.
#'
#' @examples
#' # Create the test project:
#' projectName <- "Test_Rstox"
#' createProject(projectName, files=system.file("extdata", "Test_Rstox", package="Rstox"), ow=FALSE)
#'
#' # Get output from the baseline:
#' system.time(baselineData <- getBaseline(projectName))
#' # Check the structure of the output from getBaseline():
#' ls.str(baselineData)
#'
#' # Override parameters in the baseline:
#' system.time(baselineDataMod <- getBaseline(projectName, 
#'     AcousticDensity = list(a = -70, m = 10),
#'     BioStationWeighting = list(WeightingMethod = "NASC", Radius=100, a = -70, m = 10)))
#'
#' # Check differences in parameters and data saved by the baseline model:
#' all.equal(baselineData, baselineDataMod)
#'
#' @export
#' @rdname runBaseline
#'
runBaseline <- function(projectName, startProcess=1, endProcess=Inf, reset=FALSE, save=FALSE, out=c("name", "baseline", "project"), msg=TRUE, exportCSV=FALSE, warningLevel=0, parlist=list(), ...){
	# Open the project (avoiding generating multiple identical project which demands memory in Java):
	projectName <- getProjectPaths(projectName)$projectName
	# If reset==TRUE allow for the warning in getProject():
	baseline <- openProject(projectName, out="baseline", msg=reset)
	baseline$setBreakable(jBoolean(FALSE))
	baseline$setWarningLevel(jInt(warningLevel))
	if(!exportCSV){
		baseline$setExportCSV(jBoolean(FALSE))
	}
	# Remove processes that saves the project.xml file, which is assumed to ALWAYS be the last process. Please ask Åsmund to set this as a requirement in StoX:
	numProcesses <- baseline$getProcessList()$size() - length(baseline$getProcessByFunctionName("WriteProcessData"))
	currentEndProcess <- baseline$getRunningProcessIdx() + 1

	# Run only if it is necessary:
	run <- FALSE

	# First, make sure that the process indices are within the valid range:
	startProcess <- getProcess(baseline, proc=startProcess)
	endProcess <- getProcess(baseline, proc=endProcess)
	
	# (1) If 'reset' is given as TRUE, run the baseline model between the given start and end processes:
	if(reset){
		run <- TRUE
	}
	else{
		# Detect changes to the baseline parameters compared to the last used parameters. This is done only to determin whether the baseline should be rerun:
		currentpar <- getBaselineParameters(baseline, type="current")
		newpar <- modifyBaselineParameters(currentpar, parlist=parlist, ...)$parameters
		lastpar <- getBaselineParameters(baseline, type="last")
		changedProcesses <- which(sapply(seq_along(newpar), function(i) !identical(newpar[[i]], lastpar[[i]])))
		
		# (2) If the requested parameters (either through parlist and ..., or by default those in the baseline model) differ from the parameters of the last run, rerun the baseline model:
		if(length(changedProcesses)){
			run <- TRUE
			startProcess <- min(currentEndProcess + 1, changedProcesses)
			endProcess <- max(changedProcesses, endProcess)
		}
		# (3) If the current run did not extend to requested end process, run all processes from 'currentEndProcess' to 'endProcess'
		else if(currentEndProcess<endProcess){
			run <- TRUE
			startProcess <- currentEndProcess + 1
		}
	}

	
	# Do not run if the start process is later than the end proces, indicating that the model has been run before:
	if(startProcess > endProcess){
		run <- FALSE
	}
	
	# Override parameters in the baseline:
	if(run){
		#if(msg)	{cat("Running baseline process ", startProcess, " to ", endProcess, " (out of ", numProcesses, " processes, excluding save processes)\n", sep="")}
		if(msg)	{cat("Running baseline process ", startProcess, " to ", endProcess, " (out of ", numProcesses, " processes)\n", sep="")}
		parlist <- getParlist(parlist=parlist, ...)

		# If parameters are given, override the current parameters in memory, and store the current (if save=TRUE) and last used parameters:
		if(length(parlist)){
			# Get the current and the new parameters:
			currentpar <- getBaselineParameters(baseline, type="current")
			newpar <- setBaselineParameters(baseline, parlist=parlist, msg=FALSE)
			# The first line (temp <- getRstoxEnv()) is needed to assure that the RstoxEnv environment is loaded:
			temp <- getRstoxEnv()
			# Set the 'lastParameters' object in the poject list and in the processData environment:
			RstoxEnv[[projectName]]$lastParameters <- newpar
			#assign("lastParameters", newpar, envir=getRstoxEnv()[[projectName]])
			##assign(getRstoxEnv()[[projectName]]$lastParameters, newpar)
			###getRstoxEnv()[[projectName]]$lastParameters <- newpar
			##### NOTE (2017-08-28): This is different from the 'lastParameters' object in the project environment (rstoxEnv[[projectName]]), and is only used when writing project data such as bootstrap results to file: #####
			setProjectData(projectName=projectName, var=newpar, name="lastParameters")
	
			# Run the baseline:
			baseline$run(jInt(startProcess), jInt(endProcess), jBoolean(FALSE))

			# Change the 'javaParameters' object and keep the last used parameters in Java memory (do nothing compared to using setBaselineParameters() below):
			if(save){
				# The first line (temp <- getRstoxEnv()) is needed to assure that the RstoxEnv environment is loaded:
				temp <- getRstoxEnv()
				RstoxEnv[[projectName]]$javaParameters <- newpar
				#assign("javaParameters", newpar, envir=getRstoxEnv()[[projectName]])
				##assign(getRstoxEnv()[[projectName]]$javaParameters, newpar)
				###getRstoxEnv()[[projectName]]$javaParameters <- newpar
			}
			# Else return to original parameter values:
			else{
				setBaselineParameters(baseline, parlist=currentpar, msg=FALSE)
			}
		}
		else{
			# Run the baseline:
			baseline$run(jInt(startProcess), jInt(endProcess), jBoolean(FALSE))
		}
	}

	# Return a baseline object:
	if(tolower(substr(out[1], 1, 1)) == "b"){
		return(baseline)
	}
	# Return the project object:
	if(tolower(substr(out[1], 1, 1)) == "p"){
		return(baseline$getProject())
	}
	# Return the project name:
	else{
		return(projectName)
	}
}
#'
#' @export
#' @rdname runBaseline
#' 
getBaseline <- function(projectName, input=c("par", "proc"), proc="all", drop=TRUE, msg=TRUE, startProcess=1, endProcess=Inf, reset=FALSE, save=FALSE, parlist=list(), ...){
	# Locate/run the baseline object. If rerun=TRUE or if parameters are given different from the parameters used in the last baseline run, rerun the baseline, and if the :
	baseline <- runBaseline(projectName, startProcess=startProcess, endProcess=endProcess, reset=reset, save=save, out="baseline", msg=msg, parlist=parlist, ...)

	if(msg){ cat("Reading:\n")}
	processes <- getBaselineParameters(baseline, type="last")
	processNames <- names(processes)
	### functionNames <- sapply(processes, "[[", "functionName")
	matchedProcesses <- processNames[getProcess(baseline, proc=proc)]

	######################################################
	##### (1) Get a list of processes with paramers: #####
	if("par" %in% input){
		input <- c(processNames, input)
	}
	# Using input = FALSE, NULL, or "" suppresses returning parameters of the baseline:
	if(!any(identical(input, FALSE), length(input)==0, nchar(input)==0)){
		if(msg){ cat("Baseline parameters\n")}
		parameters <- processes[intersect(input, processNames)]
	}
	else{
		parameters <- NULL
	}
	######################################################

	###########################################
	##### (2) Get output from processes: #####
	if(length(matchedProcesses)){
		outputData <- lapply(matchedProcesses, function(xx) {if(msg) {cat("Process output", xx, "\n")}; suppressWarnings(getDataFrame(baseline, processName=xx))})
		names(outputData) <- matchedProcesses
	}
	else{
		outputData <- NULL
	}
	###########################################

	###########################################
	##### (3) Get a list of process data: #####
	processdataNames <- baseline$getProject()$getProcessData()$getOutputOrder()$toArray()
	if("proc" %in% input){
		input <- c(processdataNames, input)
	}
	processdataNames <- intersect(processdataNames, input)
	processData <- lapply(processdataNames, function(xx) {if(msg) {cat("Process data", xx, "\n")}; suppressWarnings(getProcessDataTableAsDataFrame(baseline, xx))})
	names(processData) <- processdataNames
	###########################################

	# Return the data:
	out <- list(parameters=parameters, outputData=outputData, processData=processData)
	if(drop){
		out <- out[sapply(out, length)>0]
		while(is.list(out) && length(out)==1){
			out <- out[[1]]
		}
	}
	invisible(out)
}


#*********************************************
#*********************************************
#' Run (or get without running, nostly used internal in Rstox) a StoX baseline model
#' 
#' Gets the indices of processes in the baseline model, where both process name and function name are accepted. The process WriteProcessData is ignored.
#' 
#' @param projectName   The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param proc			A string vector naming processes/function to find.
#'
#' @return Index number of the process.
#'
#' @examples
#' getProcess("Test_Rstox", c("SuperIndAbundance", "StratumArea"))
#'
#' @export
#' @keywords internal
#'
getProcess <- function(projectName, proc="all"){
	if(length(proc)==0 || identical(proc, FALSE)){
		return(NULL)
	}
	
	matchOne <- function(proc, procNames, funNames){
		matchedProcName <- which(procNames == proc)
		matchedFunName <- which(funNames == proc)
		# 1. Match process name:
		if(length(matchedProcName)==1){
			if(length(setdiff(matchedFunName, matchedProcName))==1){
				warning(paste0("One process AND one funciton used by another process use the same name: ", proc, ". The process name has presidence."))
			}
			return(matchedProcName)
		}
		# 2. Match function name:
		else if(length(matchedFunName)==1){
			return(matchedFunName)
		}
		# 3. Match multiple function names:
		else if(length(matchedFunName)>1){
			warning(paste0("Multiple processes use the function ", proc, " (", paste(procNames[matchedFunName], collapse=", "), "). The first returned (", procNames[matchedFunName[1]], ")."))
			return(matchedFunName[1])
		}
		else{
			warning(paste0("The process ", proc, " does not match any process or funciton name."))
			return(NA)
		}
	}
	
	# Locate/run the baseline object. If rerun=TRUE or if parameters are given different from the parameters used in the last baseline run, rerun the baseline, and if the :
	processes <- getBaselineParameters(projectName, type="last")
	procNames <- names(processes)
	funNames <- sapply(processes, "[[", "functionName")
	
	# Given as FALSE, og length 0, or as a empty character string, return NULL:
	if(any(identical(proc, FALSE), length(proc)==0, nchar(proc)==0)){
		out <- NULL
	}
	# Discard the WriteProcessData process (which should already be removed in readBaselineParameters()), and crop to the indices of the existing processes:
	else if(is.numeric(proc)){
		numProcesses <- length(processes) - sum(funNames == "WriteProcessData")
		proc <- pmax(proc, 1)
		out <- pmin(proc, numProcesses)
	}
	# Returning all processes:
	else if(isTRUE(proc) || identical(proc, "all")){
		out <- seq_along(procNames)
	}
	# Match with the input 'proc':
	else{
		out <- sapply(proc, matchOne, procNames, funNames)
	}
	
	# Remove NAs:
	out <- out[!is.na(out)]
	# Output:
	return(out)
}


#*********************************************
#*********************************************
#' Set, read, get or modify baseline parameters.
#' 
#' 
#' \code{setBaselineParameters} Sets baseline parameters in memory to new values specified in \code{parlist} or \code{...}. \cr \cr
#' \code{readBaselineParameters} Reads the baseline parameters from the project.xml file \cr \cr
#' \code{getBaselineParameters} Gets either original, current or last used baseline parameters \cr \cr
#' \code{modifyBaselineParameters} Only modifies the parameters in \code{parameters} using those in \code{parlist} and \code{...}. This function does not change the values other than in the return of the function (not in the RstoxEnv environment or in the project file). \cr \cr
#' 
#' @param projectName   The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param msg			Logical; if TRUE print old and new parameters.
#' @param parlist		List of parameters values overriding existing parameter values. These are specified as processName = list(parameter = value), for example AcousticDensity = list(a = -70, m = 10), BioStationWeighting = list(WeightingMethod = "NASC", a = -70, m = 10). Numeric parameters must be given as numeric, string parameters as string, and logical parameters (given as strings "true"/"false" in StoX) can be given as logical TRUE/FALSE. New parameters can be set by setBaselineParameters() but not removed in the current version.
#' @param save.project	Logical: if TRUE save the changes to the project.xml file.
#' @param ...			Same as parlist, but can be specified separately (not in a list but as separate inputs).
#' @param rver			The version of the stox library.
#' @param type			The type of baseline parameter list, one of  "original", "current" and "last".
#' @param parameters	A list of the baseline parameters to modify using \code{parlist} or \code{...}.
#'
#' @return The original parameters
#'
#' @export
#' @rdname setBaselineParameters
#'
setBaselineParameters <- function(projectName, msg=TRUE, parlist=list(), save.project=FALSE, ...){
	# Get the baseline object, asuming the project is already open, thus the suppressWarnings():
	suppressWarnings(baseline <- openProject(projectName, out="baseline"))
	# Include both parameters specified in 'parlist' and parameters specified freely in '...':
	parlist <- getParlist(parlist=parlist, ...)

	# Get current parameters:
	currentpar <- getBaselineParameters(baseline, type="current")
	
	# Override parameters in the baseline:
	if(length(parlist)>0){

		# Get changed parameters, and discard ..., since it has been accounted for in getParlist(). This function simpy returns a list of the new parameters and indices for which parameters have been changed, and does not actually alter the parameters in Java memory:
		newpar <- modifyBaselineParameters(currentpar, parlist=parlist)
	
		# Change the parameter values in Java memory and return the original values:
		for(i in seq_along(newpar$changeProcessesIdx)){
			# get the parameter value from Java:
			temp <- baseline$getProcessList()$get(as.integer(newpar$changeProcessesIdx[i]))$getParameterValue(newpar$changeParameters[i])
			# Warning if the parameter was previously not set:
			#if(length(temp)==0){
				#warning(paste("The parameter", newpar$changeParameters[i], "of process", newpar$changeProcesses[i], "was not defined in the original baseline model, and cannot be changed in the current version of Rstox."))
				#}
			# Change the parameter value in Java:
			baseline$getProcessList()$get(as.integer(newpar$changeProcessesIdx[i]))$setParameterValue(newpar$changeParameters[i], newpar$changeValues[i])
		}
		if(msg){ print(list(old=currentpar[newpar$changeProcesses], new=newpar$parameters[newpar$changeProcesses]))}
		# Save only if specified. Otherwise the changes are only made in memory:
		if(save.project){
			saveProject(projectName)
		}
		# Return the new parameters:
		return(newpar$parameters)
	}
	else{
		# Return the current parameters, that is the parameters stored in Java memory:
		return(currentpar)
	}
}
#'
#' @importFrom XML xmlDoc
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
#' @importFrom XML xpathSApply
#' @importFrom XML xmlGetAttr
#' @importFrom XML xmlRoot
#' @export
#' @rdname setBaselineParameters
#' 
readBaselineParameters <- function(projectName, rver="1"){
	# Function for extracting a parameter name and value:
	getParamPV <- function(x){
		ret <- list()

		# Get a node name
		#paramName<- xmlGetAttr(xmlRoot(x[[1]]), 'name')
		paramName<- xmlGetAttr(x, 'name')
		
		# Get a node value
		paramVal <- xmlValue(x[[1]])
		# Convert missing values to empty strings:
		if(is.na(paramVal)){
			paramVal <- ""
		}

		# Construct a vector of name -> value
		ret[[paramName]] <- paramVal

		return(ret)
	}
	# Function for extracting one baseline process:
	getbaselineProcess <- function(x, namespace){
		ret <- list()

		# Process as a standalone XML doc
		subDoc <- xmlDoc(x)

		# Check whether a process is enabled
		r <- getNodeSet(subDoc, "//ns:enabled", namespaces = c(ns=namespace))
		isEnabled <- xmlValue(r[[1]])

		# If a process is marked as disabled, stop and return NA instead
		if(toupper(isEnabled)!="TRUE"){
			return(c(DISABLED=NA))
		}
		
		# Extract the attribute 'name' from the process node
		processName<- xmlGetAttr(xmlRoot(subDoc), 'name')

		# Extract the function node's value
		r <- getNodeSet(subDoc, "//ns:function", namespaces = c(ns=namespace))
		functionName <- xmlValue(r[[1]])

		# Extract parameters name and value
		r <- xpathSApply(subDoc, "//ns:parameter", getParamPV , namespaces = c(ns=namespace))

		# Append function name and parameters
		ret[[processName]] <- c(list(functionName=functionName), r)

		return(ret)
	}
	
	# Construct the used namespace
	selectedNS = paste0("http://www.imr.no/formats/stox/v", rver)

	# Read project.xml:
	projectPaths <- getProjectPaths(projectName)

	# Parse the XML
	doc <- xmlParse(projectPaths$projectXML)
	#doc <- xmlParse(projectXML)

	# Get the the baseline model's process nodes
	baselineProcess <- getNodeSet(doc, "//ns:model[@name='baseline']//ns:process", namespaces = c(ns=selectedNS))
	
	# Process the baseline nodes and extract name and parameters
	baselineProcessFunctions <- sapply(baselineProcess, getbaselineProcess, selectedNS)

	# Filter out write process data
	baselineProcessFunctions[["WriteProcessData"]] <- NULL

	# Remove all disabled process
	baselineProcessFunctions <- baselineProcessFunctions[!is.na(baselineProcessFunctions)]

	return(baselineProcessFunctions)
}
#'
#' @export
#' @rdname setBaselineParameters
#' 
readBaselineParametersJava <- function(projectName){
	getParametersOfProcess <- function(processNr, project){
		# Number of parameters:
		L = project$getBaseline()$getProcessList()$get(as.integer(processNr))$getMetaFunction()$getMetaParameters()$size()
		if(L==0){
			return()
		}
		parameterNames = unlist(lapply(seq(0,L-1), function(j) project$getBaseline()$getProcessList()$get(as.integer(processNr))$getMetaFunction()$getMetaParameters()$get(as.integer(j))$getName()))
		parameterValues = lapply(seq(1,L), function(j) project$getBaseline()$getProcessList()$get(as.integer(processNr))$getParameterValue(parameterNames[j]))
		empty = sapply(parameterValues, length)==0
		if(sum(empty)){
			parameterValues[empty] = rep(list(NA),sum(empty))
		}
		cbind(parameterNames, unlist(parameterValues))
	}
	project <- getProject(projectName)
	processList <- project$getBaseline()$getProcessList()$toString()
	processList <- JavaString2vector(processList)
	out <- lapply(seq_along(processList) - 1L, getParametersOfProcess, project)
	names(out) <- processList
	out
}
#'
#' @export
#' @rdname setBaselineParameters
#'
getBaselineParameters <- function(projectName, type=c("original", "current", "last")){
	projectName <- getProjectPaths(projectName)$projectName
	if(tolower(substr(type[1], 1, 1)) == "o"){
		type <- "originalParameters"
	}
	else if(tolower(substr(type[1], 1, 1)) == "c"){
		type <- "javaParameters"
	}
	else{
		type <- "lastParameters"
	}
	getRstoxEnv()[[projectName]][[type]]
}
#'
#' @export
#' @rdname setBaselineParameters
#' 
modifyBaselineParameters <- function(parameters, parlist=list(), ...){
	# Include both parameters specified in 'parlist' and parameters specified freely in '...':
	parlist <- getParlist(parlist=parlist, ...)
	# If nothing has changed these wiil appear as NULL in the output:
	changeProcesses <- NULL
	changeProcessesIdx <- NULL
	changeParameters <- NULL
	changeValues <- NULL
	changed <- NULL
	if(length(parlist)){
		# Update process names:
		processNames <- names(parameters)

		# Get names and indices of processes to change, and names and values of the parameters to change in those processes:
		changeProcesses <- names(parlist)
		numParameters <- unlist(lapply(parlist, length))
		changeProcesses <- rep(changeProcesses, numParameters)
		changeProcessesIdx <- match(changeProcesses, processNames)
		changeParameters <- unlist(lapply(parlist, names))

		# Unlist using recursive=FAKSE since it can hold different types (logical, string), and collapse to a vector after converting to logical strings as used in StoX: 
		changeValues <- unlist(parlist, recursive=FALSE)
		# Set logical values to "true"/"false":
		logicalValues <- sapply(changeValues, is.logical)
		changeValues[logicalValues] <- lapply(changeValues[logicalValues], function(xx) if(xx) "true" else "false")
		# Collapse to a vector as the other three change vectors:
		changeValues <- as.character(unlist(changeValues, use.names=FALSE))

		# Issue a warning if there are non-existent processes, and remove these:
		nonExistent <- is.na(changeProcessesIdx)
		if(any(nonExistent)){
			warning(paste0("The following processes are present in parlist or ... but not in the project, and were thus removed):\n", paste(processNames[nonExistent], collapse=", ")))
			if(all(nonExistent)){
				return(list())
			}
			changeProcesses <- changeProcesses[!nonExistent]
			changeProcessesIdx <- changeProcessesIdx[!nonExistent]
			changeParameters <- changeParameters[!nonExistent]
			changeValues <- changeValues[!nonExistent]
		}
		
		# Discard the parameter 'functionName' which was is not present in the parameters in memory but was added in for convenience:
		valid <- changeProcesses != "functionName"
		if(any(!valid)){
			changeProcesses <- changeProcesses[valid]
			changeProcessesIdx <- changeProcessesIdx[valid]
			changeParameters <- changeParameters[valid]
			changeValues <- changeValues[valid]
		}
		
		changed <- logical(length(changeValues))
		# Change the parameters in the list of parameters:
		for(i in seq_along(changeProcessesIdx)){
			# Changed on 2017-08-28 to comparing old and new parameter value (which must have been the intension):
			#if(length(parameters[[changeProcessesIdx[i]]] [[changeParameters[i]]])){
			if(!identical(parameters[[changeProcessesIdx[i]]] [[changeParameters[i]]], changeValues[i])){
				parameters[[changeProcessesIdx[i]]] [[changeParameters[i]]] <- changeValues[i]
				changed[i] <- TRUE
			}
		}
	}
	# Subtract 1 form the 'changeProcessesIdx' to prepare for use in java:
	list(parameters=parameters, changeProcesses=changeProcesses, changeProcessesIdx=changeProcessesIdx - 1, changeParameters=changeParameters, changeValues=changeValues, changed=changed)
}


#*********************************************
#*********************************************
#' Merges a list and the following list elements given as "...".
#' 
#' 
#' @param parlist		List of parameters values specified as processName = list(parameter = value)
#' @param ...			Same as parlist, but can be specified separately (not in a list but as separate inputs).
#'
#' @return Merged list of parameters, duplicates not removed.
#'
#' @export
#' @keywords internal
#'
getParlist <- function(parlist=list(), ...){
	# Get the specifications given as '...':
	dotlist <- list(...)
	# Check whether parlist is a string vector, in which case it is coerced to a list:
	if(!is.list(parlist) && is.character(parlist)){
		parlist <- as.list(parlist)
	}
	# Merge the inputs:
	parlist <- c(parlist, dotlist)
	
	if(length(parlist)){
		# Convert strings to list:
		namesParlist <- names(parlist)
		if(length(namesParlist)==0){
			namesParlist <- character(length(parlist))
		}
		areString <- which(sapply(parlist, is.character) & nchar(namesParlist)==0)
		strings <- unlist(parlist[areString])
		parlist[areString] <- lapply(seq_along(areString), function(i) list(functionName=strings[i]))
		names(parlist)[areString] <- strings
		
		# Remove empty elements:
		parlist <- parlist[sapply(parlist, length)>0]
	}
	return(parlist)
}


#*********************************************
#*********************************************
#' Get joined table of trawl assignments, psu and stratum
#'
#' Get trawl assignments from baseline in StoX Java memory.
#'
#' @param baseline	A StoX Java baseline object.
#'
#' @return Dataframe with trawl assignments merged with psu and stratum
#'
#' @examples
#' # Create the test project:
#' createProject("Test_Rstox", files=system.file("extdata", "Test_Rstox", package="Rstox"), ow=FALSE)
#' baseline <- openProject("Test_Rstox", out="baseline")
#' assignments <- getBioticAssignments(baseline)
#'
#' @export
#' @rdname getBioticAssignments
#' 
getBioticAssignments <- function(baseline) {
	ta <- getProcessDataTableAsDataFrame(baseline, 'bioticassignment')
	pa <- getProcessDataTableAsDataFrame(baseline, 'suassignment')
	ps <- getProcessDataTableAsDataFrame(baseline, 'psustratum')
	out <- merge(x=merge(x=ps, y=pa, by.x='PSU', by.y='SampleUnit'), y=ta, by='AssignmentID')
}


#*********************************************
#*********************************************
#' Functions to return various paths and file names.
#'
#' \code{getProjectPaths} returns ta list of projectName, projectRoot, projectPath, RDataDir, RReportDir and projectXML. \cr \cr
#' \code{getProjectDataEnv} gets the project environment. \cr \cr
#'
#' @param projectName  	The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param dir			The directory holding the project(s).
#'
#' @return Various names and directories
#' 
#' @importFrom rJava .jnew
#' 
#' @export
#' @rdname getProjectPaths
#' 
getProjectPaths <- function(projectName=NULL, dir=NULL){
	# Return the default workspace immediately if nothing is given:
	if(length(projectName)==0){
		return(.jnew("no/imr/stox/model/Project")$getRootFolder())
	}
	##################################################
	##### 1. Get the project name: #####
	if(any(class(projectName)=="jobjRef")){
		# If a baseline object is given:
		type <- tolower(projectName$getClass()$toString())
		if(endsWith(type, "model")){
			projectName <- projectName$getProject()$getProjectName()
		}
		# If a StoX project object is given:
		else if(endsWith(type, "project")){
			projectName <- projectName$getProjectName()
		}
		else{
			warning("Invalid projectName (must be a character sting or a baseline or project object)")
			projectName <- NA
		}
	}
	else if(!is.character(projectName)){
		warning("Invalid projectName (must be a character sting or a baseline or project object)")
		projectName <- NA
	}
	# Use the basename:
	if(length(dir)==0){
		projectRoot <- dirname(projectName)
	}
	else{
		projectRoot <- path.expand(dir)
	}
	projectName <- basename(projectName)
	##################################################
	
	##################################################
	##### 2. Get the project root: #####
	##################################################
	if(projectRoot %in% c(".", "", "/") && length(dir)==0){
		# The functions J and .jnew and other functions in the rJava library needs initialization:
		Rstox.init()
		projectRoot <- .jnew("no/imr/stox/model/Project")$getRootFolder()
	}
	
	##################################################
	##### 3. Get the project path: #####
	##################################################
	projectPath <- file.path(projectRoot, projectName)
	
	##################################################
	##### 4. Get the project R data directory: #####
	##################################################
	RDataDir <- file.path(projectPath, "output", "r", "data")
	
	##################################################
	##### 5. Get the project R report directory: #####
	##################################################
	RReportDir <- file.path(projectPath, "output", "r", "report")
	
	##################################################
	##### 6. Get the project XML file: #####
	##################################################
	projectXML <- file.path(projectPath, "process", "project.xml")
	
	# Output a list of the objects formerly retrieved by individual functions:
	list(projectName=projectName, projectRoot=projectRoot, projectPath=projectPath, RDataDir=RDataDir, RReportDir=RReportDir, projectXML=projectXML)
}
#' 
#' @export
#' @rdname getProjectPaths
#' 
getProjectDataEnv <- function(projectName){
	projectName <- getProjectPaths(projectName)$projectName
	# Do not issue a warning if the project is already open, since getProjectDataEnv() is intended to get data from the project enviroment, assuming it is already open. 
	openProject(projectName)
	getRstoxEnv()[[projectName]]$projectData
}


#*********************************************
#*********************************************
#' Convert list to matrix and generate missing values
#'
#' Convert a list of vectors with variable length to a matrix with all possible variables in the columns, and with missing values
#'
#' @param x	A list of vectors of one dimension with column names or names.
#'
#' @export
#' @keywords internal
#' 
as.matrix_full <- function(x){
	# Scan for the field names:
	if(length(colnames(x[[1]]))==0){
	x <- lapply(x, t)
	}
	unames <- unique(unlist(lapply(x, colnames)))
	# Get elements that have all fields, and use the first one to define the order of the field names:
	fullLength <- sapply(x, length) == length(unames)
	if(any(fullLength)){
	unames <- colnames(x[[which(fullLength)[1]]])
	}
	# Fill inn the data:
	for(i in seq_along(x)){
	one <- rep(NA, length(unames))
	names(one) <- unames
	one[colnames(x[[i]])] <- x[[i]]
	x[[i]] <- one
	}
	out <- matrix(unlist(x, use.names=FALSE), ncol=length(unames), byrow=TRUE)
	rownames(out) <- NULL
	colnames(out) <- unames
	out
}


#*********************************************
#*********************************************
#' Get subsequently plotting variables used in plotting functions in Rstox (using abbreviated string mathes obtained by abbrMatch())
#'
#' These functions are used in the plotting and reporting functions getPlots() and getReports() and dependent functions.
#'
#' @param x				A single string naming a variable.
#' @param table			A vector of stings to match \code{x} against.
#' @param ignore.case	Logical: if TRUE, ignore case when matching.
#'
#' @export
#' @keywords internal
#'
abbrMatch <- function(x, table, ignore.case=FALSE){
	inputTable <- table
	if(ignore.case){
		table <- tolower(table)
		x <- tolower(x)
	}
	table <- as.list(table)
	names(table) <- table
	string <- do.call("$", list(table, x))
	hit <- table == string
	string <- inputTable[hit]
	ind <- which(hit)
	list(string=string, ind=ind, hit=hit)
}


#*********************************************
#*********************************************
#' Get a specific variable of a list by its name, and issue an error if the variable has zero length
#' 
#' This funciton is used to ensure that the requested variable has positive length. If not and error is issued, which may happen it there is a naming discrepancy between Rstox and StoX.
#' 
#' @param x		The names of variables.
#' @param var	A list or data frame of data from a project.
#'
#' @return The requested variable as returned using "$".
#'
#' @export
#' @keywords internal
#'
getVar <- function(x, var){
	getVarOne <- function(var, x){
		if(var %in% names(x)){
			x[[var]]
		}
		else{
			stop(paste0("Variable ", var, " not present in the data frame \"", deparse(substitute(x)), "\""))
		}
	}
	if(length(var)==1){
		getVarOne(var, x)
	}
	else{
		as.data.frame(sapply(var, getVarOne, x=x))
	}
}


#*********************************************
#*********************************************
#' Set, get, save or load project data.
#' 
#' \code{setProjectData} Assigns an object to the projectData environment of the project. \cr \cr
#' \code{getProjectData} Gets an object from the projectData environment of the project. \cr \cr
#' \code{saveProjectData} Saves some or all of the objects in the projectData environment of the project to files in the output/r/data directory. \cr \cr
#' \code{loadProjectData} Loads some or all of the objects in the output/r/data directory to the projectData environment of the project. \cr \cr
#' \code{saveRImage} (Old function, kept for backwards compatibility) Saves the contents of the projectData environment of the project (RestoEnv[[projectName]]$projectData). \cr \cr
#' \code{loadEnv} (Old function, kept for backwards compatibility) Loads previously saved data to the projectData environment of the project (RestoEnv[[projectName]]$projectData). \cr \cr
#' 
#' @param projectName   The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param var   for \code{setProjectData}, a project data object to be assigned to the projectData environment of the project, and for \code{getProjectData}, \code{saveProjectData} and \code{loadProjectData}, a vector of project data names to get from, save to file from, or load from file to the projectData environment, respectively.
#' @param name			Only used in setProjectData(). The name of the project data object (such as \code{lastParameters}) to assign \code{var} to in the projectData environment of the project (overriding the name of the object \code{var}). If a specific list element inside \code{name} should be set, specify this in '...'.
#' @param ow			Logical: if TRUE overvrite objects existing in the projectData environment of the project.
#' @param ...			Used for backwards compatibility.
#'
#' @export
#' @rdname setProjectData
#'
setProjectData <- function(projectName, var, name=NULL){
	# Get the project data environment to which to assign values:
	projectDataEnv <- getProjectDataEnv(projectName)
	
	if(length(name)==0){
		name <- deparse(substitute(var))
	}
	projectDataEnv[[name]] <- var
}
#' 
#' @importFrom tools file_path_sans_ext
#' @export
#' @rdname setProjectData
#' 
getProjectData <- function(projectName, var=NULL){
	# Try to get the data from the project environment, and if it does not exist, read from the output/r/data, and if missing there too, issue a warning and return NULL:
	projectDataEnv <- getProjectDataEnv(projectName)
	if(length(projectDataEnv)){
		availableVar <- ls(projectDataEnv)
		if(length(var)==0){
			return(availableVar)
		}
		if(!var %in% availableVar){
			# Try reading form the files:
			files <- list.files(getProjectPaths(projectName)$RDataDir, full.names=TRUE)
			availableVar <- basename(file_path_sans_ext(files))
			if(var %in% availableVar){
				loadProjectData(projectName, var=var)
			}
			else{
				warning("'var' not available either in the project environment (see getProjectDataEnv()) or in output/r/data")
				return(NULL)
			}
		}
		projectDataEnv[[var]]
	}
}
#' 
#' @export
#' @rdname setProjectData
#' 
saveProjectData <- function(projectName, var="all", ...){
	projectDataEnv <- getProjectDataEnv(projectName)
	if(length(projectDataEnv)==0){
		warning("Project not open")
		return(FALSE)
	}
	if(identical(tolower(var), "all")){
		var <- ls(projectDataEnv)
	}
	else{
		var <- intersect(var, ls(projectDataEnv))
	}
	if(length(var)==0){
		warning(paste0("'var' not matching any of the available data objects (", paste(ls(projectDataEnv), collapse=", "), "). No data saved"))
		return(FALSE)
	}
	
	# Get the project RData directory and the trash sub directory:
	projectPaths <- getProjectPaths(projectName)
	trashDir <- file.path(projectPaths$RDataDir, "trash")
	#Empty trash, but only those:
	#unlink(trashDir, recursive=TRUE, force=TRUE)
	# Move files to the trash:
	suppressWarnings(dir.create(trashDir, recursive=TRUE))
	files <- file.path(projectPaths$RDataDir, paste0(var, ".RData"))
	existingFiles <- file.exists(files)
	file.copy(files[existingFiles], trashDir, overwrite=TRUE, recursive=TRUE)
	unlink(files, recursive=TRUE, force=TRUE)
	
	# Save files:
	lapply(var, function(x) save(list=x, file=file.path(projectPaths$RDataDir, paste0(x, ".RData")), envir=projectDataEnv))
	invisible(files)
}
#' 
#' @export
#' @rdname setProjectData
#' 
saveRImage <- saveProjectData
#' 
#' @export
#' @rdname setProjectData
#' 
loadProjectData <- function(projectName, var="all", ow=FALSE, ...){
	# Simple function for loading Rstox data into R, treating overwriting.
	.loadToRstox <- function(file, envir, ow=FALSE){
		# Check for existance of the data in memory:
		if(!ow){
			var <- basename(file_path_sans_ext(file))
			if(var %in% ls(envir)){
				return(var)
			}
		}
		# Load the data:
		#print(file)
		load(file=file, envir=envir)
	}
	
	projectDataEnv <- getProjectDataEnv(projectName)
	
	# If not overwriting, and specific variables are requested, chech for the existence of these in memory if the project environment exists:
	if(!ow && !identical(tolower(var), "all")){
		available <- ls(projectDataEnv)
		var <- setdiff(var, available)
	}
	
	# If all variables are present in memory, return the project data environment:
	if(length(var)==0){
		return(projectDataEnv)
	}
	
	# Else try reading from files:
	projectPaths <- getProjectPaths(projectName)
	# Load the requested files in the output/r/data directory:
	if(file.exists(projectPaths$RDataDir)){
		filelist <- list.files(projectPaths$RDataDir, full.names=TRUE)
		# Remove directories:
		filelist <- filelist[!file.info(filelist)$isdir]
		# Get the files to read:
		if(!identical(tolower(var), "all")){
			filelist <- unlist(lapply(var, function(x) filelist[startsWith(basename(filelist), x)]))
		}
		if(length(filelist)==0){
			#warning(paste0("None of the requested data (", paste(var, collapse=", "), ") are present in the directory output/r/data"))
			return()
		}
		lapply(filelist, .loadToRstox, envir=projectDataEnv, ow=ow)
		return(projectDataEnv)
	}
	else{
		warning(paste0("The directory \"", projectPaths$RDataDir, "\" is missing and can be generated by running runBootstrap() and imputeByAge()."))
		return()
	}
	# If a project created prior to 2016-11-30 is used, use the old method where two directories are present with one RData file in each directory:
	#else{
	#	loadEnv(projectName, level=level, fileBaseName=fileBaseName, outputFolder=outputFolder, fileName=fileName)
	#}
}
#' 
#' @export
#' @rdname setProjectData
#' 
loadEnv <- loadProjectData



#*********************************************
#*********************************************
#' Move existing files to the trash directory in the directory of the first element (all file should be in the same directory).
#' 
#' @param x	A vector of file paths.
#'
#' @export
#' @keywords internal
#'
moveToTrash <- function(x){
	if(length(x)==0 || is.na(x)){
		return(NULL)
	}
	dir <- dirname(x[1])
	trashdir <- file.path(dir, "trash")
	trashx <- file.path(trashdir, basename(x))
	suppressWarnings(dir.create(trashdir, recursive=TRUE))
	existing <- file.exists(x)
	file.copy(x[existing], trashx[existing], copy.date=TRUE)
	unlink(x[existing])
	trashx[existing]
}


#*********************************************
#*********************************************
#' Set up the RstoxEnv
#'
#' \code{getRstoxEnv} initiates the RstoxEnv environment if not existing.
#'
#' @export
#' @keywords internal
#' @rdname getRstoxEnv
#'
getRstoxEnv <- function(){
	# Regenerate the RstoxEnv is missing:
	if(!exists("RstoxEnv")){
		initiateRstoxEnv()
	}
	RstoxEnv
}
#' 
#' @export
#' @keywords internal
#' @rdname getRstoxEnv
#' 
initiateRstoxEnv <- function(){
	# Create a Rstox environment in which the baseline objects of the various projects are placed. This allows for a check for previously run baseline models and avoids memory leakage:", 
	assign("RstoxEnv", new.env(), envir=.GlobalEnv)
	# Assign fundamental variables to the RstoxEnv:
	assign("StoXFolders", c("input", "output", "process"), envir=get("RstoxEnv"))
	assign("NMD_data_types", c("echosounder", "biotic", "landing"), envir=get("RstoxEnv"))
	assign("StoX_data_types", c("acoustic", "biotic", "landing"), envir=get("RstoxEnv"))
	assign("StoX_data_type_keys", c(acoustic="echosounder_dataset", biotic="missions xmlns", landing="Sluttseddel"), envir=get("RstoxEnv"))
	assign("bootstrapTypes", c("Acoustic", "SweptArea"), envir=get("RstoxEnv"))
	assign("processLevels", c("bootstrap", "bootstrapImpute"), envir=get("RstoxEnv"))
}

