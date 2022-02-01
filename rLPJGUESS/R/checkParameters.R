# @title A parameter list function
# @description  This function checks the provided parameter list against the
# complete parameters list. If any parameter is not provided, the function will
# add it to the parameter list and return a complete parameter list with the values
# provided as input and the default values for those parameters that were not provided.
# @param scale a character string indicating whether the model runs global or for europe.
# @param parameterList a named list holding the parameter or combination or parameters
#  to be tested.
# @param type serial, parallel
# In this case, parameterList is a vector of names.
# @return a named list holding the values of the template parameter.
# @keywords rLPJGUESS
# @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig
# @examples \dontrun{
# parameterList <- list(run_emax = 4, run_lambda_max = 1)
# parameterList.checked <- checkParameters(scale= "global", parameterList)
# }
checkParameters <- function(scale, parameterList = NULL, type = "serial"){
  # include check
  # if ( scale != "global" & scale != "europe"){
  #   stop("Invalid scale: neither global nor europe")
  # }
  if (type != "parallel" & type != "serial" ){
    stop("Invalid check type")
  }
  # call the default template
  default <- getParameterList(scale, list = T)

  if(is.null(parameterList)){
    parameterList  <- default
  }else if(class(parameterList) == "list" ){
      # Throw an error if
      # no parameter names
    if(is.null(names(parameterList))){stop("Invalid parameters: no names provided")}
  }else{
      stop("Please provide a valid parameterList")
  }
  #  Throw an error if wrong parameter names

  dummyCheck <- !names(parameterList) %in% names(default)
  if(any(dummyCheck)){
    warning(paste("Wrong parameterList in ", paste(names(parameterList)[dummyCheck], collapse = ", " )))
    stop("Invalid parameterList")
  }

  if (type == "parallel"){
    parameterList <- parameterList[names(parameterList) %in% names(default) ]
  }else if (type == "serial"){
    parameterList <- c(parameterList[ names(parameterList) %in% names(default) ],
                         default[ !names(default) %in% names(parameterList)] )
  }
  return(parameterList)
}

# @description  This function takes a character string vector with parameter names
# and returns a parameterLsit holding the parameters non present in the passed vector.
# @param scale a character string indicating whether the model runs global or for europe.
# @param parameterNames
# @return a named list holding the parameters
# @keywords rLPJGUESS
# @author Ramiro Silveyra Gonzalez
# @examples \dontrun{
# parameterNames <- c("Que_rob_gdd5min_est", "Que_rob_tcmin_surv"
# parameterList <- checkParameters(scale= "europe", parameterNames)

checkParameters.names <- function(scale, parameterNames){
  # include check
  # if ( scale != "global" & scale != "europe"){
  #   stop("Invalid scale: neither global nor europe")
  # }
  # call the default template

  default <- getParameterList(scale, list = T)

  # if(is.null(parameterNames)){
  #   parameterList  <- default
  # }
  #  Throw an error if wrong parameter names
  dummyCheck <- !parameterNames %in% names(default)
  if(any(dummyCheck)){
    warning(paste("Wrong parameterList in ", paste(parameterNames[dummyCheck], collapse = ", " )))
    stop("Invalid parameterList")
  }
  # if inverse I am passing only names
  parameterList <- default[ !names(default) %in% parameterNames]
  return(parameterList)
}


# @description  This function checks the provided parameter list. If the parameterList
# is of class matrix, the fucntion converts it to list.
# @param scale a character string indicating whether the model runs global or for europe.
# @param parameterList a named list  or matrix with dimnames holding the parameters
# or combination or parameters
# @return a named list holding the parameters
# @keywords rLPJGUESS
# @author Ramiro Silveyra Gonzalez
# @examples \dontrun{
# par <- seq(500, 3000, len = 3)
# parameterList <- as.matrix(par)
# colnames(parameterList) <- "Que_rob_gdd5min_est"
# rownames(parameterList) <- NULL
# parameterList <- checkParameters(scale= "europe", parameterList)
# }
checkParameters.matrix <- function(scale, parameterList = NULL, type = "normal"){

  if (is.null(parameterList)){
    message("You have not provided a parameter list")
    message("Model will run with default parameter values")
    parameterList <- getParameterList(scale, list = T)

  }else if (class(parameterList)== "matrix"){

    if(is.null(dimnames(parameterList))){stop("Invalid parameters: no names provided")}

    # wrong parameter
    if(is.null(rownames(parameterList))){
      parameterList.names <- colnames(parameterList)
      indices <- c(1:nrow(parameterList))
      parameterList <- lapply(indices, function(x){
          parComb <- as.list(parameterList[x,])
          names(parComb) <-parameterList.names
          return(parComb)
        })
    }else{
      parameterList.names <- rownames(parameterList)
      indices <- c(1:ncol(parameterList))
      parameterList <- lapply(indices, function(x){
        parComb <- as.list(parameterList[,x])
        names(parComb) <- parameterList.names
        return(parComb)
      })
    }

    if (length(parameterList)==1){ parameterList <- parameterList[[1]] }

  }else if(!class(parameterList) == "list" ){
    stop ("Invalid parameter list")
  }
  return(parameterList)
}


# @description  This function checks the parameter root_distance and replace by the
# complete values. Root dist in the package is provided as the perecentage of roots
# in the upper layer, while LPJ needs both values.
# @param parameterList a named list holding the parameters
# or combination or parameters
# @return a named list holding the parameters wiht corrected root distance
# @keywords rLPJGUESS
# @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig
# @examples \dontrun{
# parameterList <-list("tree_rootdist" = 0.6)
# parameterList <- checkParameters.rootDist(parameterList)
# parameterList
# $tree_roodist
# "0.6 0.4"
# }
checkParameters.rootDist <- function(parameterList){

  root_distance <- names(parameterList)[grep("_rootdist", names(parameterList))]
  if(length(root_distance) > 0){
    for (i in 1:length(root_distance)){
      parameterList[[root_distance[i]]]  <- paste(parameterList[[root_distance[i]]], 1-parameterList[[root_distance[i]]])
    }

  }
  return(parameterList)
}



