#' @title This function writes the two main templates in the correct order
#' @description This function takes the defaultparameters and a defaultlist of
#' of files and writes the main and pft instruction file into the defined destinations
#' which are MainTemplateDestination and PftTemplateDestination
#' @param defaultparameters a matrix with the defaultparameters which should be
#' used in rlpjguess runs
#' @param defaultlist a matrix with the design parameters which should be used
#' in rlpjguess runs
#' @param MainTemplateDestination a character string indicating the space where
#' one should write the main file
#' @param PftTemplateDestination a character string indicating the space where
#' one should write the pft file
#' @param NameMainFile a character string with the name of the main file
#' which was produced with the default parameters before
#' @param NamePftFile a character string with the name of the pft file
#' which was produced with the default parameters before
#' @return writes the two files which serve as templates
#' @author Johannes Oberpriller
#' @export


AdjustTemplates <- function(defaultparameters,defaultlist,
                            MainTemplateDestination = NULL,
                            PftTemplateDestination = NULL,
                            NameMainFile = NULL,
                            NamePftFile = NULL){

  # check if there is a destination to save the main template

  if(is.null(MainTemplateDestination)){
    stop("Please provide a destination for the Main template")
  }

  #check if there is a destination to save the pft/species template

  else if(is.null(PftTemplateDestination)){
    stop("Please provide a destination for the PFT template")
  }

  else if(is.null(NameMainFile)){
    stop("Please provide a valid main template")
  }

  else if(is.null(NamePftFile)){
    stop("Please provide a valid pft template")
  }

  ## TO-DO: Dokumentation
  ## TO-DO: Maybe give another return than the function does now



  tx  <- readLines(NamePftFile)
  for(i in 1:nrow(defaultparameters)){
    if(defaultparameters[i,"group"] == "run"){
      tx <- gsub(paste0(defaultparameters[i,"name"]," "),
                 paste0(defaultparameters[i,"name"]," ",
                        defaultparameters[i,"rlpjname"]," !"), x = tx)
    }
    else{
      linenumber_group = grep(paste0("group \"",defaultparameters[i,"group"],"\""),
                              x = tx, fixed = T)
      linenumber_pft = grep(paste0("pft \"",defaultparameters[i,"group"],"\""),
                            x = tx, fixed = T)
      linenumber_st = grep(paste0("st \"",defaultparameters[i,"group"],"\""),
                           x = tx, fixed = T)
      linenumber = c(linenumber_group, linenumber_pft, linenumber_st)
      for(j in 1:length(linenumber)){
        endofgroup = grep(")", x = substring(tx[(linenumber[j]+1):length(tx)],1,1))[1]
        exchange = grep(paste0(as.character(defaultparameters[i,"name"])),
                        x = tx[(linenumber[j]+1):(linenumber[j]+1+endofgroup+1)], fixed = T)
        tx[(linenumber[j])+exchange] <- gsub(paste0(defaultparameters[i,"name"]," "),
                                             paste0(defaultparameters[i,"name"]," ",
                                                    defaultparameters[i,"rlpjname"]," ","!"),
                                             x = tx[(linenumber[j])+exchange])
      }
    }
  }



  writeLines(tx, con = PftTemplateDestination)


  files <- defaultlist
  #change the actual file names with the rlpjnames

  maintemplate <- readLines(NameMainFile)

  for(i in 1:nrow(files)){
    maintemplate <- gsub(paste0("\"",files[i,"name"],"\""),
                         paste0("\"",files[i,"name"],"\"",
                                " ","(str ", "\"_",files[i,"name"],"_\")","!"),
                         x = maintemplate)
  }

  #writes the Template to the given MainTemplateDestination
  writeLines(maintemplate, con = MainTemplateDestination)


}
