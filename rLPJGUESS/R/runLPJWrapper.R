# @title A wrapper to run the LPJ-GUESS and processed the ouputs
#
# @description This function modifies the lpj template with the desired parameter
#  values and then does a system call for running the lpj modell.
#  The modell outputs are processed, plotted (if set to TRUE)
# @param runObject a named list created by the runLPJParallel function containing
# the following: mainDir, template1, template2, gridList, runDir, outDir, mode,
# scale, typeList, runID,  gridFilename, plot.data and save.plots
# @return a a data class object with the slots runInfo and dataTypes. The runInfo
# slot contains the provided run information and parameters; the dataTypes holds
# the processed output data from the run.
# @keywords rLPJGUESS
# @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig
# @note Based an older code of Istem Fer, Uni Potsdam
# @examples \dontrun{
# runLPJwrapper(runObject)
# }
runLPJWrapper <- function(runObject){
  #----------------------------------------------------------------------------#
  # CHECK INPUTS AND EXIT IF ANY ERROR
  #----------------------------------------------------------------------------#
  # checking conditions
  if (is.null(runObject[["runDir"]]) || !file.exists(runObject$runDir)){
    stop("Please provide a valid run directory")
  }
  if (is.null(runObject[["outDir"]]) || !file.exists(runObject$outDir)){
    stop("Please provide a valid output directory")
  }
  if (is.null(runObject[["template1"]])){
    stop("Please provide a valid template1 name")
  }
  if (is.null(runObject[["template2"]])){
    stop("Please provide a valid template2 name")
  }
  #if (is.null(runObject[["parameterList"]])){ No because it could run with default values
  #  stop("Please provide a valid parameter list.")
  #}
  if(is.null(runObject[["plot.data"]])){
    warning("The plot.data boolean has not been provided. It will be set to FALSE")
    runObject$plot.data <- FALSE
    runObject$save.plots <- FALSE
  }
  if(is.null(runObject[["save.plots"]])){
    warning("The save.plots boolean has not been provided. It will be set to FALSE")
    runObject$save.plots <- FALSE
  }
  if(is.null(runObject[["typeList"]])){
    runObject$typeList <- typelist.default
    warning("The output type list has not been provided")
    warning("Setting type list to default values")
  }
  #----------------------------------------------------------------------------#
  # WRAP THE FUNCTIONS
  #----------------------------------------------------------------------------#
  # checking directory existence
  # starting the function itself
  # print out info message
  message(paste("Starting run ", runObject$runID, sep = ""))
  # set the wd
  previouswd <- getwd()
  setwd(runObject$runDir)
  # write out files
  runObject$template1Mem <- sub("run_outputdirectory",
                                paste("\"",runObject$outDir, "/","\"", sep =""), runObject$template1Mem)

  ## we have to do an exact search, this here has the problem of
  ## cmass and cmass_st would both be activated
  for (j in 1:length(runObject$typeList)) {
    runObject$template1Mem <- sub(paste("!file", paste0(runObject$typeList[j]," ") ,sep="_"),
                                  paste("file",  paste0(runObject$typeList[j], " "),sep="_") , runObject$template1Mem)
  }

  writeLines(runObject$template1Mem,file.path(runObject$runDir,runObject$template1))

  #runObject$template2Mem <- readLines(file.path(runObject$mainDir,runObject$template2))
  runObject$template2Mem <- gsub("path_to_globalTemplate",
    paste(runObject$runDir, "/", runObject$template1, sep=""),
     runObject$template2Mem)
  runObject$template2Mem  <- sub("_file_gridlist_",
                                 paste(runObject$runDir,"/", runObject$gridList,sep=""),
                                 runObject$template2Mem )

  for (j in 1:length(runObject$filesNames)){
    if(is.na(runObject$filesNames[[j]][2])){
      runObject$template2Mem  <- gsub(paste("\\<",runObject$filesNames[[j]][1], "\\>", sep=""),
                                      "",runObject$template2Mem)
    }else{
      runObject$template2Mem  <- gsub(paste("\\<",runObject$filesNames[[j]][1], "\\>", sep=""),
                                      runObject$filesNames[[j]][2],
                                      runObject$template2Mem)
    }
  }
  # for ( j in 1:length(runObject$variablesNames)){
  #   if (!is.na(runObject$variablesNames[[j]][2])){
  #     runObject$template2Mem[grep(paste("\\<",runObject$variablesNames[[j]][1], "\\>", sep=""),
  #                                 runObject$template2Mem, value=F)]  <- gsub("! param", "param",
  #                                     grep(paste("\\<",runObject$variablesNames[[j]][1], "\\>", sep=""),
  #                                          runObject$template2Mem, value=T))
  #     runObject$template2Mem  <- gsub(paste("\\<",runObject$variablesNames[[j]][1], "\\>", sep=""),
  #                                     runObject$variablesNames[[j]][2],
  #                                     runObject$template2Mem)
  #   }
  # }

  writeLines(runObject$template2Mem,file.path(runObject$runDir,runObject$template2))
  writeLines(runObject$gridListCell, file.path(runObject$runDir, runObject$gridList))
  # writing template
  #message("\n\n Writing template")
  writeTemplate(runObject$template1, runObject$parameterList, runObject$runDir, check = runObject$checkParameters)
  # calling the model
  #message("\n\n Call LPJ")
  callLPJ(runObject$mainDir, runObject$runDir,runObject$template2, runObject$mode)
  # getting data
  #message("\n\n GetData")
  LPJout <- getLPJData(runObject$outDir, runObject$typeList, runObject,
                     runObject$processing)
                    #, runObject$fun)


  #----------------------------------------------------------------------------#
  # CLEAN UP RUNDIR
  #----------------------------------------------------------------------------#
  # delete all files
  if (runObject$delete){
    files.delete <- list.files(runObject$runDir, full.names = TRUE, recursive = TRUE)
    #files.delete <- files.delete[!grepl("runInfo", files.delete)]
    files.delete <- files.delete[!grepl("png", files.delete)]
    do.call("unlink", list(files.delete))
  }
  #----------------------------------------------------------------------------#
  # SAVE RUNINFO AND PLOT
  #----------------------------------------------------------------------------#
  #runObject$output <- LPJout@dataTypes
  # Sav the run info
  #For example, make a list with all the info provided to runLPJ, and store it with save()
  if (runObject$save){
    save(LPJout, file = file.path(runObject$runInfoDir,
                                  paste("runInfo", runObject$runID,".RData", sep = "")))
  }

  #  # Calculate additional outputs
  #  if (!is.null(runObject[["fun"]])){
  #    message("\n Apllying own functions")
  #    LPJout <- applyFun(LPJout, fun)
  #  }

  if (runObject$plot.data){
    plotLPJData(x = LPJout, typeList = runObject$typeList,
             outDir = runObject$outDir, save.plots = runObject$save.plots,
             prefix = paste("run",runObject$runID, "_", sep=""))
  }
   message(paste("Finished run ", runObject$runID, sep = ""))
  #----------------------------------------------------------------------------#
  # END
  #----------------------------------------------------------------------------#
  setwd(previouswd)
  return (LPJout)
}



