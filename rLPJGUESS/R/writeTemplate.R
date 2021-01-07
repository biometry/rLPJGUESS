#' @title A writing template function for LPJ-GUESS
#' @description  This function reads a template, and replaces parameters with the
#' provides parameters list. If no parameter values are provided, the function will
#' set them to the default values. The function assumes that a copy of the template
#'  is already placed in the run directory.
#' @param template1 a character string providing the general model template,
#'  e.g, global.ins. Provide only the file name, not the path
#' @param parameterList  a named list containing the parameters to be calibrated
#' @param runDir a character string indicating path to the run directory
#' @param check a character string indicating how to check the provided parameterList. Default
#' value is serial. Other possible value is parallel.
#' @return none
#' @section Warning:  The provided template can be either one provided by the package or
#' a self edited template. The function assumes a specific coding for writing the
#' parameters values. For this reason, we recommend to use the package templates.
#' If using self edited templates, please take the package templates as a reference.
#' @details  Please note that this function is integrated in \code{\link{runLPJ}}.
#'
#' Check: If check is serial, it will return the complete and checked parameterList.
#' If parallel, it would only check the provided parameters. Please only use serial,
#' other options are handled internally in the parallelization.
#'
#' @seealso \code{\link{getTemplate}}, \code{\link{getParameterList}}
#' @export
#' @keywords rLPJGUESS
#' @author  Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig,
#' @note Based an older code of Istem Fer, Uni Potsdam
#' @example /inst/examples/writeTemplateHelp.R
writeTemplate <- function(template1, parameterList, runDir, check = "serial"){
  # Checking provided parameters
  # if (is.null(runDir) || !file.exists(runDir) ){
  #   stop("Please provide a valid run directory")
  # }
  # if (is.null(template1) || !file.exists(file.path(runDir, template1))){
  #   stop("The provided template does not exits. Please provide a template name")
  # }
  if (is.null(parameterList) || !class(parameterList) =="list"){
    stop("Please provide a parameterList")
  }
#   # call the function
#   if (grepl("global",template1)){
#       # Checking paramaterList and if null, setting to default values
#       # 256 physiological parameters can be calibrated right now. Exceptions are the fine roots distributions for all PFTs
#       parameterList <- checkParameters(scale= "global", parameterList, type = check )
#   }else if (grepl("europe", template1)){
#       # Checking paramaterList and if null, setting to default values
#       # 585 physiological parameters, both general and species-specific, can be calibrated right now.
#       # Exceptions are the fine roots distributions for all species and PFTs
#       parameterList <- checkParameters(scale = "europe", parameterList,  type = check)
#   }else{
#     stop("Cannot recognize the template: neither global nor europe")
#   }
#   parameterList <- checkParameters.rootDist(parameterList)
#   # getting parameters names
  parameterNames <- names(parameterList)
  #message(parameterNames)
  # looping over parameters ## Faster
  #print(str(parameterList))
  template <- readLines(file.path(runDir, template1))
  for(i in 1:length(parameterNames)){
    template <- gsub(paste0(" ",parameterNames[i]," "), paste0(" ",parameterList[[i]]," "), template)
  }
  writeLines(template, file.path(runDir, template1))
}


