# @title The function to create the runParameters
#
# @description This function create the runObjects for the cluster submission
# @param x a LPJ setup object
# @param singleRun see createSingleObject.
# @param parameterList parameterList either a named list containing the parameters to be calibrated
# or a matrix.
# @seealso  \code{\link{runLPJ}}
# @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig
# @return TODO

createRunParameters <- function(x, singleRun, parameterList){

  # Check the parameters: length, type and names
  #parameterList <- try(checkParameters.matrix(singleRun$scale, parameterList), FALSE)
  parameterList <- checkParameters.matrix(singleRun$scale, parameterList)
  if ('try-error' %in% class(parameterList)){ stop("Invalid parameterList provided")  }

  if (class(parameterList[[1]])== "list"){
    runsParameters <- length(parameterList)
    parameterNames <- lapply(parameterList, function(x){
      parNames <-names(x)
      return(parNames)
      })
    parameterNames <- unique(unlist(parameterNames))

  }else if (class(parameterList)== "list"){
    runsParameters  <- 1
    parameterNames <- names(parameterList)
  }else{
    stop("Invalid parameterList")
  }

  # Based on parameter Names write the general template
  # and if no parameter is present raise and error?

  #parameterCommon <- checkParameters.names(scale = singleRun$scale, parameterNames)
  #print(parameterList)
  # parameterCommon <- parameterList[[1]]
  #
  # if(length(parameterCommon) > 0){
  #   #parameterCommon <- checkParameters.rootDist(parameterCommon)
  #   #write common template
  #   parameterCommonNames <- names(parameterCommon)
  #   for(i in 1:length(parameterCommon)){
  #     singleRun$template1Mem <- sub(paste0(" ",parameterCommonNames[i]," "), paste0(" ",parameterCommon[[i]]," "), singleRun$template1Mem)
  #   }
  # }


  # Check the grids
  gridListCell <- readLines(file.path(singleRun$mainDir,singleRun$gridList))
  gridListCell <- gridListCell[!grepl("!", gridListCell)]
  gridListCell <- gridListCell[!is.na(gridListCell)]
  runsGrids <- length(gridListCell)

  # FIGURE OUT HOW MANY PARAMETERS AND GRIDS
  #----------------------------------------------------------------------------#
  parallel <- try(checkParallel(singleRun$parallel, runsGrids, runsParameters), FALSE)
  if ('try-error' %in% class(parallel)){ stop("Parallelization is not feasible")  }

  if (parallel == "both"){
    numberRuns <- runsParameters * runsGrids
  }else if (parallel == "grids"){
    numberRuns <- runsGrids
    runsParameters <- 1
  }else if (parallel == "parameters"){
    numberRuns <- runsParameters
    runsGrids <- 1
  }else{
    stop("BUG")
  }

  if (numberRuns < x@numCores){
    stop("The number of cores requested exceeds the number of runs")
  }
  # So far so good,
  singleRun$checkParameters <- "parallel"
  # Create an output folder named after ID
  if (singleRun$save){
    dir.create(singleRun$runInfoDir, showWarnings = FALSE)
  }
  # Distirbute directories along runs
  runDir <- vector("character", numberRuns)
  outDir <- vector("character", numberRuns)
  for (i in 1:x@numCores) {
    for (index in seq(i, numberRuns, x@numCores )){
      runDir[index] <- x@runDir[i]
      outDir[index] <- x@outDir[i]
    }
  }
  # CREATE SINGLE RUN OBJECTS
  #----------------------------------------------------------------------------#
  runParameters <- vector("list", numberRuns)
  if (parallel == "parameters"){
    message("Parallelization of parameters")
    message("Creating the single run objects")#single run objects
    progessBar <- txtProgressBar(min = 0, max = numberRuns, style = 3)
    for (i in 1:numberRuns){
      setTxtProgressBar(progessBar, i)
      singleRun$runID <- i
      singleRun$runDir <- runDir[i]
      singleRun$outDir <- outDir[i]
      singleRun$parameterList <- parameterList[[i]]
      singleRun$gridListCell <- gridListCell
      singleRun$gridListName <- paste(unlist(strsplit(gridListCell, " ")), collapse = "_")
      runParameters[[i]] <- singleRun
    }
    close(progessBar)
  }else if (parallel == "both"){
    message("Parallelization of both parameters and grids")
    message("Creating the single run objects")#single run objects
    progessBar <- txtProgressBar(min = 0, max = numberRuns, style = 3)
    for (i in 1:numberRuns){
      setTxtProgressBar(progessBar, i)
      singleRun$runID <- i
      singleRun$runDir <- runDir[i]
      singleRun$outDir <- outDir[i]
      singleRun$parameterList <- parameterList[[ceiling(i/length(gridListCell))]]
      singleRun$gridListCell <- gridListCell[ceiling(i/length(parameterList))]
      singleRun$gridListName <- paste(unlist(strsplit(gridListCell[ceiling(i/length(parameterList))][i], " ")), collapse = "_")
      runParameters[[i]] <- singleRun
    }
    close(progessBar)
  }else if (parallel == "grids"){
    message("Parallelization of grids")
    message("Creating the single run objects")#single run objects\
    #progessBar <- txtProgressBar(min = 0, max = numberRuns, style = 3)
    singleRun$parameterList <- parameterList
    for (i in 1:numberRuns){
      #setTxtProgressBar(progessBar, i)
      singleRun$runID <- i
      singleRun$runDir <- runDir[i]
      singleRun$outDir <- outDir[i]
      singleRun$gridListCell <- gridListCell[i]
      singleRun$gridListName <- paste(unlist(strsplit(gridListCell[i], " ")), collapse = "_")
      runParameters[[i]] <- singleRun
    }
    #close(progessBar)
  }
  return(runParameters)
}



