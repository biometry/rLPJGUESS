#' @title A function to separate all default values into parameters, design parameters and input files
#' @description This function organizes the parameters extracted from the instruction files in order to run the model
#' @param defaultparameters the default parameters extracted from the instruction files
#' @param PFTs the species one wants to run the model with
#' @keywords rLPJGUESS
#' @author Johannes Oberpriller
#' @return list of runable parameters, design parameters and the files containing the default inputs
#' @export

GetRunAbleParameters <- function(defaultparameters,PFTs){


  LPJparameters_PFT <- matrix(defaultparameters$defaultparameters[,4])
  rownames(LPJparameters_PFT) = defaultparameters$defaultparameters[,3]
  LPJrunParameters  = matrix(LPJparameters_PFT[-which(substr(rownames(LPJparameters_PFT),1,3)== "run"),])
  rownames(LPJrunParameters) = rownames(LPJparameters_PFT)[-which(substr(rownames(LPJparameters_PFT),1,3)== "run")]

  designLPJ = LPJparameters_PFT[which(substr(rownames(LPJparameters_PFT),1,3)== "run"),]
  designLPJ = designLPJ[-which(names(designLPJ) == "run_outputdirectory")]

  lpjvalues = as.matrix(defaultparameters$defaultlist[,c(2,3)])

  filelist = split(t(lpjvalues),rep(1:nrow(lpjvalues),each = ncol(lpjvalues)))

  names(filelist) = gsub("_", ".",defaultparameters$defaultlist[,1])

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

