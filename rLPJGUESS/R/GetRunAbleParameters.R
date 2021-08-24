#' @title A function to separate all default values into parameters, design parameters and input files.
#' @description This function organizes the parameters extracted from the instruction files in order to run the model.
#' @param defaultobjects R object produced by \code{\link{InferParameterAndDesignList}}. Its first list contains the data frame with default parameters extracted from the instruction files
#' @param PFTs the PFTs or species, which should be included into the model run
#' @keywords rLPJGUESS
#' @author Johannes Oberpriller
#' @return list consisting of a data.frame for runable parameters, a data.frame for design parameters and a data.frame for the default input files.
#' @export

GetRunAbleParameters <- function(defaultobjects,PFTs){


  LPJparameters_PFT <- matrix(defaultobjects$defaultparameters[,4])
  rownames(LPJparameters_PFT) = defaultobjects$defaultparameters[,3]
  LPJrunParameters  = matrix(LPJparameters_PFT[-which(substr(rownames(LPJparameters_PFT),1,3)== "run"),])
  rownames(LPJrunParameters) = rownames(LPJparameters_PFT)[-which(substr(rownames(LPJparameters_PFT),1,3)== "run")]

  designLPJ = LPJparameters_PFT[which(substr(rownames(LPJparameters_PFT),1,3)== "run"),]
  designLPJ = designLPJ[-which(names(designLPJ) == "run_outputdirectory")]

  lpjvalues = as.matrix(defaultobjects$defaultlist[,c(2,3)])

  filelist = split(t(lpjvalues),rep(1:nrow(lpjvalues),each = ncol(lpjvalues)))

  names(filelist) = gsub("_", ".",defaultobjects$defaultlist[,1])

  spp <- as.matrix(LPJrunParameters[grep("_include", rownames(LPJrunParameters )),])
  PFTsRows <- c()
  for(i in PFTs) PFTsRows <- c(PFTsRows, grep(i, rownames(spp)))
  spp[-PFTsRows, 1] <- 0
  spp[PFTsRows,1] <- 1
  print(spp)

  parameterStandard_PFT <- as.matrix(LPJrunParameters[-grep("_include",
                                                    rownames(LPJrunParameters)),])

  parameterStandard_PFT <- rbind(parameterStandard_PFT, spp)


  tmp <- as.list(parameterStandard_PFT)
  namestmp <- rownames(parameterStandard_PFT)
  names(tmp) <- namestmp

  parameterStandard_PFT <- tmp


  return(list(runParameters = parameterStandard_PFT, design = designLPJ, defaultfiles = filelist))
}

