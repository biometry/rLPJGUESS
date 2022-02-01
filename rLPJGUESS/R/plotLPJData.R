#' @title A plot function for LPJData objects
#' @description  This function reads data from a \linkS4class{LPJData} object and plots the
#'  variables against time. If the save.plots is set to TRUE, plots are saved in the output folder.
#' @param x a \linkS4class{LPJData} object
#' @param outDir a character string indicating the folder where the plots will be
#' saved, if save.plot set to TRUE
#' @param save.plots a boolean indicating whether the plots are saved in the outDir.
#'  Plots will be saved as pdf.
#' @param typeList a character vector with the outputs to be plotted
#' @param prefix a character string specifying the prefix to be added to the plots files.
#' Only relevant if saving plots is TRUE
#' @return time series plots for the data types included in typeList.
#' The grid cells will be plotted independently.
#' @details Please note that this function is integrated in \code{\link{runLPJ}}.
#' The call to \code{\link{runLPJ}} can plot straigthaway the outputs (see examples below).
#' @seealso \code{\link{runLPJ}}, \linkS4class{LPJData},
#'  \url{https://cran.r-project.org/web/packages/zoo/zoo.pdf}
#' @export
#' @keywords rLPJGUESS
#' @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig, Johannes Oberpriller
#' @example /inst/examples/plotLPJDataHelp.R
plotLPJData <- function(x, typeList = NULL, gridlist = NULL, outDir= NULL,
                        save.plots = FALSE, prefix = "", plot_avg = F){

  # checking input parameters
  if(is.null(x)){
    stop("No data has been provided")
  }
  if(!class(x)=="LPJData"){
    stop("Invalid data has been provided")
  }
  if (save.plots){
    if( is.null(outDir) || !file.exists(outDir)){
      stop("No outDir has been provided")
    }
  }
  if(!requireNamespace("zoo", quietly = TRUE)){
    stop("Can't load required library 'zoo'")
  }

  data <- x@dataTypes
  # Plot from
  gridlist.available <- sort(names(data))
  typeList.available <- names(data[[1]][which(names(data[[1]]) %in%
                                                gridlist.available == F)])
  if(is.null(typeList) || !class(typeList) == "character"){

    message("No typeList has been provided. Plotting all data")
    typeList.valid <- typeList.available
  }else{
    keep <- rep(FALSE, length(typeList))
    for(i in 1:length(typeList)){
      if(typeList[i] %in% typeList.available){
        keep[i] <- TRUE
      }
    }
    if(any(keep)){
      typeList.valid <-typeList[keep]
    }else{
      stop("None of the requested output types exists")
    }
  }

  if(is.null(gridlist)){
    message("No gridlist has been provided. Plotting all gridcells")
    gridlist.valid <- gridlist.available
  }else{
    gridlist.check = apply(X = gridlist,MARGIN = 1, FUN = function (x) {
      y = paste0("gridcellc(",x[1], ", ", x[2],")")
      return(y)
    })
    keep <- rep(FALSE, length(gridlist.available))
    for(i in 1:length(gridlist.check)){
      if(gridlist.check[i] %in% gridlist.available){
        keep[i] <- TRUE
      }
    }
    if(any(keep)){
      gridlist.valid <-gridlist.check[keep]
    }else{
      stop("None of the requested gridcells exists")
    }
  }
  average = matrix(data = 0,ncol = length(gridlist.valid),
                   nrow =  nrow(data[[gridlist.valid[[1]]]][[typeList.valid[[1]]]]))
  colnames(average) = typeList.valid
  for (i in 1:length(gridlist.valid)){
    df <- data[[gridlist.valid[[i]]]]
    coordinates = get_gridlist_values(gridlist.valid[[i]])
    if(zoo::is.zoo(df) == FALSE){

      for (k in 1:length(typeList.valid)){
        values <- df[[typeList.valid[[k]]]]
        average[,k] = average[,k] + values[,ncol(values)]
        if(save.plots){
          pdf(file.path(outDir, paste(prefix, gridlist.valid[[i]],typeList.valid[[k]], ".pdf", sep="")))#width=1000,height=750
          if(length(colnames(values))==1){
            plot(values, main =paste("Gridcell Lon:",  coordinates[1],"Lat", coordinates[2],
                                     "Variable:", typeList.valid[[k]]),xlab="Years")
          }else{
            plot(values, main =paste("Gridcell Lon:",  coordinates[1],"Lat", coordinates[2],
                                     "Variable:", typeList.valid[[k]]),xlab="Years")
          }
          dev.off()
        }else{
          plot(values,  main =paste("Gridcell Lon:",  coordinates[1],"Lat", coordinates[2],
                                    "Variable:", typeList.valid[[k]]),xlab="Years")
        }


          }
    }
    else{
      values <- df
      # something like is zoo
      if (save.plots){
        pdf(file.path(outDir, paste(prefix, typeList.valid[[i]], ".pdf", sep="")))#width=1000,height=750
        plot(values, main =paste("Variable:", typeList.valid[[i]]),xlab="Years")
        dev.off()
      }else{
        plot(values,  main =paste("Variable:", typeList.valid[[i]]),xlab="Years")
      }
    }
  }
    if(plot_avg == T){
      for(k in 1:length(typeList.valid)){
      plot(y = average[,typeList.valid[[k]]]/length(gridlist.valid), x = as.numeric(rownames(values)),
           main = paste("Average of", "Variable:",typeList.valid[[k]]),
           xlab = "Years",ylab = "total", type = "l")
    }


  }
}



get_gridlist_values <- function(gridcell){
  coordinates = strsplit(gridcell, "c")[[1]][3]
  coordinates = unlist(strsplit(coordinates, ","))
  coordinates = sub("\\(","", coordinates)
  coordinates = sub("\\)","", coordinates)
  return(coordinates)
}



