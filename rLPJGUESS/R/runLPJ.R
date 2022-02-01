#' @title The function to run the LPJ-GUESS in parallel
#' @description This function allows to run the LPJ-GUESS model serial
#' or parallel. It returns the model outputs as a \linkS4class{LPJData} object,
#'  which can also be stored as RData.
#' @param x either a LPJSetup object created with the setupLPJParallel function or a
#'  character string indicating the path to the directory where
#'  the model link and template are located, and in which the function will create
#'  the directory structure for the outputs.
#' @param parameterList a named list containing the parameters (when not run in parallel) to be calibrated
#' or a matrix (when run in parallel). If running in parallel, parameter list should be either a list of of list or
#' a matrix where each row is a parameter combination and the column names should be named
#' after the parameters. See  fucntion \code{\link{getParameterList}}) for default values.
#' @param typeList a character vector with the outputs to be analyzed.
#' Default value is all outputs.
#' @param settings additional parameters \itemize{
#' \item  gridList a character string providing the name of the text file with
#' the grids to be included in the model, e.g, gridlist.txt. It must be in the mainDir.
#' Provide only the file name, not the path.
#' \item mode a character string indicating whether using cru or cf data
#' \item scale a character string indicating whether the model runs global or
#' for europe
#' \item file.co2 a character string providing the absolute path to the C02 input file
#' \item file.cru a character string providing the absolute path to the cru input file
#' \item file.cru.misc a character string providing the absolute path to the cru
#'  misc  input file
#' \item file.ndep a character string providing the absolute path to the nitrogen
#'  deposition input file
#' \item file.temp a character string providing the absolute path to the temperature
#'  input file
#' \item file.prec a character string providing the absolute path to the
#' precipitation input file
#' \item file.insol a character string providing the absolute path to the
#'  insolation input file
#' \item file.wetdays a character string providing the absolute path to the
#'  wetdays input file
#' \item file.minTemp a character string providing the absolute path to the
#'  minimum temperature input file
#' \item file.maxTemp a character string providing the absolute path to the
#'  maximum temperature input file
#' \item variable.temp a character string providing the variable name of the
#' temperature input file
#' \item variable.prec a character string providing the variable name of the
#' precipitacion input file
#' \item variable.insol a character string providing the variable name of the
#' insolation input file
#' \item variable.wetdays a character string providing the variable name of the
#' wetdays input file
#' \item variable.minTemp a character string providing the variable name of the
#' minimum temperature input file
#' \item variable.maxTemp a character string providing the variable name of the
#' maximum temperature input file
#' \item template1  character string providing the general model template,
#'  e.g, global.ins. It must be in the mainDir. Provide only the file name,
#'   not the path. If not provided, package templates will be used.
#' \item template2 a character string providing the  "specific" model template,
#'  e.g, global_cf.ins or global_cru.ins. It must be in the mainDir. Provide
#'  only the file name, not the path. If not provided, package templates will be
#'   used.
#' \item plot.data  a boolean indicating whether the ouput data will be plotted
#'  (default FALSE)
#' \item save.plots  a boolean indicating whether the plots will be saved (default
#'  FALSE)
#' \item processing a boolean indicating whether output files will be turned into zoo
#'  time series (default FALSE). This is only supported when running the model
#'  for one grid cell. For several grid cells, please set processing to FALSE.
#' \item parallel a character string providing the parallel strategy. If grids, it will
#' parallelize grids. If parameters, it will parallelize parameters. If both, it will
#' parallelize both grids and parameters. If auto, it will decided the strategy based
#' on the provided parameterList and gridList. Default value is auto
#' \item delete a boolean indicating whether output files should be deleted after
#'  processing (default TRUE). Saved plots will not be deleted.
#' \item save a boolean indicating whether function outputs should be saved as RData
#' into an output directory named (runInfoDir_DATE). Default is TRUE.
#' \item runID an integer after which the output directory will be named (default empty).
#' If parallel TRUE, ID is ignored and defined by setupLPJParallel.
#' \item design a named list containing the general parameters for LPJ-GUESS.
#' See function \code{\link{getDesign}} for default values and examples.
#' }
#' @return an object of class \linkS4class{LPJData} The \linkS4class{LPJData} object
#'  will be automatically stored as RData in a folder in the mainDir. The folder
#'  will be named as runInfo plus the date in format %Y_%m_%d_%H%M%S.
#' @export
#' @section Model templates:
#' It is not mandatory to provide the model templates. The package contais model templates
#' (see \code{\link{getTemplate}}) and will write them with the specified information
#'  (input files, design, parameters).
#' If you decided to provide templates to \code{\link{runLPJ}}, you can either use
#'  the package ones or a self edited templates. The package assumes a specific
#'  coding for writing the parameters values, design options and input files paths.
#'  For this reason, we recommend to use the package templates.
#' Yet, if you want to use self edited templates, please take the package templates
#'  as a reference (see \code{\link{getTemplate}}).
#' @section Warning:
#' When using MPI clusters, please call the function \code{\link{exitMPI}}
#' before terminating your R session.
#' @details The runLPJ in parallel assumes the existence of a folder the model templates
#'  for LPJ-GUESS (optional) and link to the model executable: mainDir.
#' Running the LPJ-GUESS in parallel involves two steps. First, to create a parallel
#' setup (\code{\link{setupLPJParallel}}), and second, to actually run the model
#' (\code{\link{runLPJ}}).  The parallelization requires the package \emph{snow} for SOCK clusters or
#' the package \emph{Rmpi} for MPI clusters.
#' @seealso  \url{https://cran.r-project.org/web/packages/Rmpi/Rmpi.pdf},
#'  \url{https://cran.r-project.org/web/packages/snow/snow.pdf},
#'  \code{\link{setupLPJParallel}}, \code{\link{exitMPI}}, \linkS4class{LPJData},
#'  \linkS4class{LPJSetup}, \code{\link{getParameterList}}, \code{\link{getDesign}}
#' @export
#' @keywords rLPJGUESS
#' @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig, Johannes Oberpriller
#' @example /inst/examples/runLPJHelp.R
runLPJ <-  function(x, settings, typeList=NULL, parameterList=NULL){

  if (is.null(x)){
    stop("Please provide a valid value for x")

  }else if (class(x) == "character"){
  #----------------------------------------------------------------------------#
  # SERIAL RUNLPJ
  #----------------------------------------------------------------------------#
    if (is.null(settings) || !class(settings) == "list"){ stop("Invalid settings provided")  }

    if(!file.exists(x)){stop("Invalid main directory") }

    # do the settings check
    singleRun <- try(createSingleObject(x, typeList, settings), FALSE)
    if ('try-error' %in% class(singleRun)){ stop("Invalid settings provided")  }

    # Check the parameters
    parameterList <- try(checkParameters.matrix(singleRun$scale, parameterList), FALSE)
    if ('try-error' %in% class(parameterList)){ stop("Invalid parameterList provided")  }

    singleRun$parameterList  <-  parameterList

    # Need to create an output folder named after ID
    if (singleRun$save){
      dir.create(singleRun$runInfoDir, showWarnings = FALSE)
    }

    # Create runDir
    singleRun$runDir <- file.path(x, paste("runDirectory", singleRun$runID, sep=""))
    singleRun$outDir <- file.path(x, paste("runDirectory", singleRun$runID, sep=""),
                                  paste("outDirectory", singleRun$runID, sep=""))
    dir.create(singleRun$runDir, showWarnings = FALSE)
    dir.create(singleRun$outDir, showWarnings = FALSE)

    # Read in the templates and gridcell
    gridListCell <- readLines(file.path(singleRun$mainDir,singleRun$gridList))
    gridListCell <- gridListCell[!grepl("!", gridListCell)]
    singleRun$gridListCell <- gridListCell[!is.na(gridListCell)]

    #singleRun$template1Mem <- readLines(file.path(singleRun$mainDir, singleRun$template1))
    # template 2: the cru or cf template
    #singleRun$template2Mem <- readLines(file.path(singleRun$mainDir,singleRun$template2))
    result <- runLPJWrapper(singleRun)
    if ('try-error' %in% class(result)){
      stop("Error when running the model")
    }
    return(result)

  #----------------------------------------------------------------------------#
  # PARALLEL RUNLPJ
  #----------------------------------------------------------------------------#
  }else if(class(x) == "LPJSetup"){

    if (is.null(settings) || !class(settings) == "list"){ stop("Invalid settings provided") }

    # do the settings check
    singleRun <- try(createSingleObject(x@mainDir, typeList, settings), FALSE)
    if ('try-error' %in% class(singleRun)){
      stop("Invalid settings provided")
    }
    # Checking packages availability
    if (!requireNamespace("snow", quietly = TRUE)){
      stop("Can't load required library 'snow', runLPJparallel will now exit")
    }
    if (x@clusterType=="MPI"){
      if (!requireNamespace("Rmpi", quietly = TRUE)){
        stop("Can't load required library 'Rmpi', runLPJparallel will now exit")
      }else{
        # check cluster size
        numCores.available <- Rmpi::mpi.universe.size() - 1
        if ( numCores.available == 0) {
          stop("There are not enough available cores to create a cluster")
        }else if ( numCores.available != x@numCores) {
          message(paste("There are", numCores.available,"cores available ", sep = " "))
          message(paste("You requested", x@numCores,  "cores", sep = " "))
          message("The number of cores will be set to meet the available resources")
          x@numCores <- numCores.available
        }
      }
    }
  # CREATE THE RUN PARAMETERS
  #----------------------------------------------------------------------------#
  message("\n\nReading the parallel object structure")
  # do the settings check
  #runParameters <- try(createRunParameters(x, singleRun, parameterList), FALSE)
  runParameters <- createRunParameters(x, singleRun, parameterList)
  if ('try-error' %in% class(runParameters)){
    stop("Invalid settings provided")
  }
  # SOCK CLUSTER
  #----------------------------------------------------------------------------#
  # Create cluster
  if (x@clusterType =="SOCK"){
    message( paste ("Creating a", x@clusterType, "cluster with",
                x@numCores, " cores", sep = " " ))
    cl <-  snow::makeSOCKcluster(x@numCores,useXDR = F)
    # Exporting needed data and loading required
    # packages on workers. --> If daa is loaded firs it can be exporte to all workers
    snow::clusterEvalQ(cl, library(rLPJGUESS))
    snow::clusterEvalQ(cl, "runParameters")
    # Distribute calculation: will return values as a list object
    message ("Sending tasks to the cores")
    # Try catch prevent the package for crashing
    # the implemented try catch in snow is not satisfactory
    result <- try(snow::clusterMap(cl, runLPJWrapper,  runParameters), FALSE)
    if ('try-error' %in% class(result)){
      stop("Error when running the model")
    }
    # Destroy cluster
    snow::stopCluster(cl)
    # Snow's close command, shuts down and quits from script

  # MPI CLUSTER
  #----------------------------------------------------------------------------#
  }else if (x@clusterType =="MPI"){
    # Use Rmpi to spawn and close the slaves
    # Broadcast the data to the slaves and
    # Using own MPISapply with mpi.parsSapply. mpi.parSapply takes a list
    # "cores", so that there is one task for each core.
    # Then each core is aware of how many task he has to carry and applies
    # MPISapply on its tasks. Result is a list of list, thus, it must be
    # unlisted
    # needlog avoids fork call
    if(is.loaded ("mpi_initialize")){
      if (Rmpi::mpi.comm.size() < 1 ){
        message( paste ("Creating a", x@clusterType, "cluster with",
                    x@numCores, "cores", sep = " " ))
        message("Please call exit_mpi at the end of you script")
        Rmpi::mpi.spawn.Rslaves(nslaves = x@numCores, needlog = FALSE)
      }else{
        message(paste("Using the existing", x@clusterType, "cluster with",
                  x@numCores, " cores", sep = " " ))
      }
    }
    cores <- rep(x@numCores, x@numCores)
    Rmpi::mpi.bcast.Robj2slave(cores)
    Rmpi::mpi.bcast.Robj2slave(runParameters)
    Rmpi::mpi.bcast.cmd(library(rLPJGUESS))
  # try-catch does not help in mpi. Rmpi hanldes itself
   # result <- try( Rmpi::mpi.parSapply(cores, MPISapply, runParameters = runParameters), FALSE)
   # if ('try-error' %in% class(result)){
   #   stop("Error when running the model")
   #  }
    result <- Rmpi::mpi.parSapply(cores, MPISapply, runParameters = runParameters)
  }
  # END
  #----------------------------------------------------------------------------#
  return(unlist(result))
  }else{
    stop("Please provide a valid value for x")
  }
}


