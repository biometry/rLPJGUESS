#' @title A get design function
#' @description  This function returns the LPJ-GUESS default design for running the model
#' @param scale a character string indicating whether the parameters are for the global or europe template
#' @param list a character boolean to specify the returned format. If TRUE the parameter will be returned
#' as a list, otherwise as a matrix.
#' @return a list or a matrix with design parameter names and their values
#' @export
#' @author Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @example /inst/examples/getDesignHelp.R
getDesign <- function(scale, list =  F){
  # if ( is.null(scale) || scale != "global" & scale != "europe"){
  #   stop("Please provide a valid scale: global or europe")
  # }
  tmp <- design.default$scale
  if (list){
    tmp.names <- rownames(tmp)
    tmp <- as.vector(tmp, mode = "list")
    names(tmp) <- tmp.names
  }

  return(tmp)
}
