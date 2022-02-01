# @title The function to fill the single run object
#
# @description This function TODO
# @param mainDir a character string indicating the path to the main directory
# @param typeList a character vector with the outputs to be analyzed.
#  Default value is all outputs.
# @param settings additional parameters
# @seealso  \code{\link{runLPJ}}
# @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig, Johannes Oberpriller
# @return TODO
createSingleObject <- function(mainDir, typeList, settings){

  defaultSettings <- list(gridList= NULL, scale = NULL, mode = NULL,
                        file.co2 = NULL, file.cru = NULL, file.cru.misc = NULL,
                        file.ndep= NULL, file.temp = NULL, file.prec = NULL,
                        file.insol = NULL, file.wetdays = NULL, file.minTemp = NULL,
                        file.soildata = NULL,
                        file.maxTemp = NULL, variable.temp = NULL, variable.ndep = NULL,
                        variable.prec = NULL, variable.insol = NULL, variable.wetdays = NULL,
                        variable.minTemp = NULL, variable.maxTemp = NULL, template1 = NULL,
                        template2=NULL, plot.data = FALSE, save.plots = FALSE, processing = FALSE,
                        delete = TRUE, save= TRUE, runID = "", parallel = "auto",
                        checkParameters = "serial", design = NULL, defaultlist = NULL)
  #, fun = NULL) # This would be to allow havin own functions in parallel.

  settings <- c(settings[names(settings) %in% names(defaultSettings)],
                defaultSettings[ !names(defaultSettings) %in% names(settings)])

  if (is.null(typeList) || !class(typeList) == "character"){
    settings$typeList <-  typelist.default
    message("Output typeList has not been provided")
    message("Setting typeList to default values")
  }else{
    settings$typeList <- typeList
  }
  if ( settings[["parallel"]] != "auto" & settings[["parallel"]] != "grids" & settings[["parallel"]] != "parameters"  & settings[["parallel"]] != "both"){ # this is relevant if getting template
    stop("Please provide a valid parallel value")
  }
#  # will potential provide more parameters
#  if (!is.null(settings[["fun"]])){
#
#    if (class(settings[["fun"]]) == "character"){
#      if (settings[["fun"]] == "met"){
#        settings$fun  <- calculateMet
#      }else{
#        settings$fun <- NULL
#      }
#    }else if(class(settings[["fun"]]) == "function"){
#      message("\nAdded user defined fun")
#    }else{
#      warning("The prodived fun argument is not provided")
#      setings$fun <- NULL
#    }
#  }


  # checking template1
  if (is.null(settings[["template1"]])){
    # writing out template and storing name
    settings$template1 <- getTemplate (settings[["scale"]], outputDir = mainDir)
    message("Using package template (template 1)")
    message("Saving package template in the mainDir")
  }else if (!file.exists(file.path(mainDir, settings[["template1"]]))){
    warning ("The provided template (template1) does not exist")
    stop("Please provide a valid template name")
  }
  # checking template 2: either cru or cf
  if ( is.null(settings[["template2"]])){
    # writing out template and storing name
    settings$template2 <- getTemplate (type = paste(settings[["scale"]],"_", settings[["mode"]], sep = ""),
                                       outputDir = mainDir)
    message("Using package template (template 2)")
    message("Saving package template in the mainDir")
  }else if (!file.exists(file.path(mainDir, settings[["template2"]]))){
    warning ("The provided template (template2) does not exist")
    stop("Please provide a valid template name")
  }

  # Pack up all files that user should have provided
  # Get the default list from internal data , that contains the characters stings
  # to replace in the template
  # Go throught the files and check whether they provided, if so add them to
  # default list, otherwise stop the function

  # Johannes: Depending on the version of LPJ different parameters are
  # required to run the model.
  # I think, every LPJ user has working instruction files,
  # which allow to infer all required files and it is enough to provide
  # the functionality to substitute them in the main file if they want to change
  # this from the wrapper

  files <- settings[grepl("file", names(settings))]
  variables <- settings[grepl("variable", names(settings))]
  filesandvariables = c(files,variables)

  files.names <- names(settings$defaultlist)

  for (i in 1:length(files.names)){
    if (is.null(filesandvariables[[files.names[i]]])){
      next
    }else if(!file.exists(filesandvariables[[files.names[i]]])){
      warning(paste("The", files.names[i], "does not exist", sep = " "))
    }else{
      #print(i)
      #print(filesandvariables[[files.names[i]]])
      settings$defaultlist[[files.names[i]]][2] <- filesandvariables[[files.names[i]]]

    }
  }


  singleObject <- settings[!grepl("file", names(settings))]
  singleObject <- singleObject[!grepl("variable", names(singleObject))]
  singleObject$filesNames <- settings$defaultlist
  singleObject$mainDir <- mainDir
  singleObject$runInfoDir <-  file.path(singleObject$mainDir,
                                        paste("runInfo",
                                              format(Sys.time(), "%Y_%m_%d_%H%M%S"),
                                              sep = "_"))
  # Read template one and replace design
  singleObject$template1Mem <- readLines(file.path(singleObject$mainDir, singleObject$template1))

  # Check the design
  #settings$design <- checkDesign(settings$scale , settings$design)

  designNames <- names(settings$design)

  for(i in 1:length(settings$design)){
    singleObject$template1Mem <- gsub(paste0(" ",designNames[[i]]," "),
                                      paste0(" ",settings$design[[i]]," "),
                                       singleObject$template1Mem)
  }
  if(settings$design[["run_ifcalcsla"]]==as.character(0)){
    singleObject$template1Mem <- sub("!sla", "sla", singleObject$template1Mem)
  }

  singleObject$template2Mem <- readLines(file.path(singleObject$mainDir,singleObject$template2))

  return(singleObject)
}


