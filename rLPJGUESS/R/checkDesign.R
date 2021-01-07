
# @title A check design function
# @description  This function checks the provided design against the
# default values, and returns a complete design. If wrong design is passed,
# the fucntion will raise an error.
# @param scale a character string indicating whether the model runs global or for europe.
# @param design a named list or matrix holding the design
# @return a named list holding the designr.
# @keywords rLPJGUESS
# @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig
# @examples \dontrun{
#
# }
checkDesign <- function(scale, design = NULL){
  # include check
  default <- getDesign(scale, list = T)

  if (is.null (design)){
    design <- default
  }else{
    if(class(design) == "list" ){
      # Throw an error if
      # no parameter names
      if(is.null(names(design))){stop("Invalid design: no names provided")}
    }else if(class(design) == "matrix" ){
      # Throw an error if
      # no parameter names
      if(is.null(dimnames(design))){stop("Invalid design: no names provided")}
      # wrong parameter
      if(is.null(rownames(design))){
        design.names <- rownames(design)
      }else{
        design.names <- colnames(design)
      }
      design <- as.vector(design, mode = "list")
      names(design) <- design.names
    }else{
      stop("Please provide a valid design")
    }
    #  Throw an error if wrong parameter names
    dummyCheck <- !names(design) %in% names(default)
    if(any(dummyCheck)){
      warning(paste("Wrong design in ", paste(names(design)[dummyCheck], collapse = ", " )))
      stop("Invalid design")
    }
    design <- c(design[names(design) %in% names(default) ],
                default[ !names(default) %in% names(design)])
  }
  return(design)
}





