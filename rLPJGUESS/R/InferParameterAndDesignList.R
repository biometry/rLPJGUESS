#' @title A function to infer the parameters and design parameters from the
#' instruction file
#' @description This function returns a list of parameters and design parameters
#'  used for the later model runs. It also allows the user to see a summary
#'  of all his parameters
#' @param MainInputFile a list with one element named "main" which is the main
#' instruction file given to lpj-guess when running it in the command line
#' @param NameMainFile a character string with the name of the main file. In
#' this file this function will write all input parameters. When no value is
#' provided the default main instruction file while be main.ins
#' @param NamePftFile a character string with the name of the pft file. In this
#' file the function will write all parameters specific for pfts and design.
#' When no value is provided the default main instruction file while be pft.ins
#' @param vectorvaluedparams a vector of character strings providing the vector
#' valued parameters. rlpjguess can in the moment not handel vectorvalued
#' parameters, which can not be computed from the first element. Typically
#' for this is the rootdist, which depends on the different layers in the soil.
#' @return a list with a matrix for the defaultparameter values and a matrix
#' with defaultdesign values
#' @author Johannes Oberpriller
#' @export


InferParameterAndDesignList <- function(MainInputFile,NameMainFile = "main.ins",
                                        NamePftFile = "pft.ins",
                                        vectorvaluedparams){

  ## get the main and pft template from the origin main instruction files

  runlist <- write_main_templates(MainInputFile, NameMainFile = NameMainFile,
                                  NamePftFile = NamePftFile)

  # set up matrix to save all defaultparameters and give them names

  defaultparameters <- data.frame(matrix(nrow = 1, ncol = 5))
  colnames(defaultparameters) = c("type","name", "rlpjname","value", "group")

  for(i in 1:length(runlist$design)){
    parametersnew = vector(length = 4)
    parametersnew[1] = "design"
    parametersnew[2] = strsplit(runlist$design[[i]]," ")[[1]][1]
    parametersnew[3] = paste0("run_",parametersnew[2])
    parametersnew[4] = gsub("[[:space:]]", "", strsplit(runlist$design[[i]]," ")[[1]][2])
    parametersnew[4] = gsub("!","",parametersnew[4])
    parametersnew[5] = "run"
    defaultparameters = rbind(defaultparameters, parametersnew)
  }
  defaultparameters = defaultparameters[-1,]

  runlist_pfts = unlist(runlist$pfts)
  # handel all possible ways the instruciton files are
  # ordered or the code is saved
  for(i in 1:length(runlist$pfts)){
    linenumber = c()
    linenumber_group = c()
    linenumber_pft =c()
    linenumber_st = c()
    # get if line is beginning of group,pft or stand
    linenumber_group = grep("group ", x = substr(runlist_pfts[i],
                                                 start = 1, stop = 6))
    linenumber_pft = grep("pft ", x = substr(runlist_pfts[i],
                                             start = 1, stop = 4))
    linenumber_st = grep("st ", x = substr(runlist_pfts[i],
                                           start = 1, stop = 3))
    linenumber = c(linenumber_group, linenumber_pft, linenumber_st)
    linename = strsplit(runlist_pfts[i]," ")[[1]][2]
    if(length(linenumber)!=0){
      endoufgroup = c()
      # get the end of the group
      endofgroup = grep(")", x = substr(runlist_pfts[(i+1):length(runlist_pfts)],1,2), fixed = T)[1]
      # put all lines between beginning and end into the parametermatrix
      for(j in 1:(endofgroup-1)){
        parametersnew = vector(length = 4)
        parametersnew[1] = "pft"
        parametersnew[2] = gsub("[[:space:]]","",strsplit(runlist_pfts[i+j]," ")[[1]][1])
        parametersnew[3] = paste0(gsub("\"","",linename),"_", parametersnew[2])
        parameterposition = 2
        parametersnew[4] = ""
        while(grepl("^\\s*$", parametersnew[4])){
          parametersnew[4] = gsub("[[:space:]]"," ",
                                  strsplit(runlist_pfts[i+j]," ", fixed = T)[[1]][parameterposition])
          parameterposition = parameterposition + 1
          if(grepl("^!", parametersnew[4])){
            parametersnew[4] = ""
            break
          }
        }
        parametersnew[4] = gsub("^!","",parametersnew[4])
        parametersnew[5] = gsub("\"","",linename)
        defaultparameters = rbind(defaultparameters, parametersnew)
      }
    }
  }
  #print(defaultparameters)
  # handle possible errors
  defaultparameters = defaultparameters[which(defaultparameters[,2] != ""),]
  defaultparameters = defaultparameters[which(defaultparameters[,2] != "!"),]
  defaultparameters = defaultparameters[which(defaultparameters[,4] != "NA"),]
  # get ride of spaces and comments for possible multiplications later
  for(i in 1:nrow(defaultparameters)){
    startofcomments = gregexpr("!",defaultparameters[i,4])[[1]][1]
    if(startofcomments != "-1"){
      defaultparameters[i,4] = sub("[[:space:]]+","",substr(defaultparameters[i,4], start = 1, stop = startofcomments -1))
    }
  }
  # exclude all vectorvalued parameters from the list
  for(i in 1:length(vectorvaluedparams)){
    defaultparameters = defaultparameters[which(defaultparameters[,2] != vectorvaluedparams[i]),]
  }



  ### Infer the Design list

  # unpack all variables and climate input files

  runlist_variables = unlist(runlist$variables,use.names = F)
  runlist_inputfiles = unlist(runlist$files, use.names = F)

  # define matrix to store them

  defaultlist = data.frame(matrix(nrow = 1, ncol = 3))
  colnames(defaultlist) = c("name","rlpjnames","value")

  # handle all possible ways stored in file
  for(i in 1:length(runlist_variables)){
    new_variable = vector(length = 3)
    new_variable[1] = gsub("\"","",strsplit(runlist_variables[i]," ")[[1]][2])
    beginn = gregexpr("\\(",runlist_variables[i])
    end = gregexpr("\\)", runlist_variables[i])
    parameterpart = gsub("\"", "",strsplit(substr(runlist_variables[i], start = beginn[[1]], stop = end[[1]]-1), " ")[[1]][2])
    new_variable[2] = paste0("_",new_variable[1],"_")
    new_variable[3] = parameterpart
    defaultlist = rbind(defaultlist, new_variable)
  }

  defaultlist = defaultlist[-1,]
  # handle all possible ways stored in file
  for(i in 1:length(runlist_inputfiles)){
    new_variable = vector(length = 3)
    new_variable[1] = gsub("\"","",strsplit(runlist_inputfiles[i]," ")[[1]][2])
    beginn = gregexpr("\\(",runlist_inputfiles[i])
    end = gregexpr("\\)", runlist_inputfiles[i])
    parameterpart = gsub("\"", "",strsplit(substr(runlist_inputfiles[i], start = beginn[[1]], stop = end[[1]]-1), " ")[[1]][2])
    new_variable[2] = paste0("_",new_variable[1],"_")
    new_variable[3] = parameterpart
    defaultlist = rbind(defaultlist, new_variable)
  }

  # get ride of empty lines
  #print(defaultparameters)
  defaultlist = defaultlist[which(defaultlist[,1] != ""),]

  return(list(defaultparameters = defaultparameters, defaultlist = defaultlist))
}


# @description This function takes the main input input files and desired file
# names and returns a list with files, variables, parameters and design parameters
# @param MainInputFile is the main input file see InferParameterAndDesignList
# @param NameMainFile is the main file after reordering see InferParameterAndDesignList
# @param NamePftFile is the pft file after reordering see InferParameterAndDesignList
# @return returns a list with the entries parameters,files, variables and design
# which are all themselves matrices
# @author Johannes Oberpriller

write_main_templates <- function(MainInputFile,NameMainFile, NamePftFile){

  # Infer the correct order of instruction files

  insfilelist = infer_instruction_order(MainInputFile = MainInputFile)

  # Create the temporary files to get all lines

  file.create("./main_tryout.ins")
  outcon <- file("./main_tryout.ins", "w")
  for(i in 1:length(insfilelist)){
    lines <- readLines(insfilelist[[i]])
    writeLines(lines,outcon)
  }
  close(outcon)

  # Create the Main and Ins file

  file.create(paste0("./",NameMainFile))
  file.create(paste0("./",NamePftFile))
  lines <- readLines("./main_tryout.ins")

  # delete all import files

  importlines = grepl("import",substr(lines, start = 1, stop = 7))
  lines = lines[-which(importlines)]

  # Grep all "param" lines and the ndep_timeseries, delete duplicates
  # and write them to the parameterfiles

  paramlines = grepl("param", substr(lines, start = 1, stop = 7))
  parameters = lines[which(paramlines)]

  parameternames = sapply(strsplit(parameters," "), function (x) x[2])
  duplicates_parameters = which(duplicated(parameternames) ==T)

  while(length(duplicates_parameters) != 0 ){
    first_apperance = c()
    for(i in duplicates_parameters){
      first_apperance <- c(first_apperance, which(parameternames == parameternames[i])[1])
    }
    parameters = parameters[-first_apperance]
    parameternames = sapply(strsplit(parameters," "), function (x) x[2])
    duplicates_parameters = which(duplicated(parameternames) ==T)
  }

  ndep_time = grepl("!ndep_timeseries", substr(lines, start = 1, stop = 16))
  if(length(which(ndep_time ==T)) ==0){
    ndep_time = grepl("ndep_timeseries", substr(lines, start =1, stop =15))
  }
  parameters = c(parameters, lines[which(ndep_time)])
  parameternames = sapply(strsplit(parameters," "), function (x) x[2])
  parameterfiles = grepl("file", parameternames)
  parametervariables = parameters[-which(parameterfiles)]
  parameterfiles = parameters[parameterfiles]

  # Write the main Files and delete all lines which are written there

  FileCon = file(NameMainFile)
  mainimport = paste("import ",paste0("\"", "path_to_globalTemplate", "\"", "\n"), sep =" ")
  writeLines(c(mainimport,parameters), FileCon)
  close(FileCon)
  lines = lines[-c(which(paramlines),which(ndep_time))]
  headingfileline <- paste("\n", "!Which files to include in the output","\n")

  ## Get the Title, the Outputfiles and the outcommented lines

  titleline <- grepl("title",substr(lines, start = 1, stop = 10))
  filelines <- grepl("file", substr(lines, start = 1, stop = 5))
  outcommented <- grepl("!", substr(lines, start = 1, stop = 1))

  ## get the lines where pfts and/or species parameters are specified

  pftlines = c()
  for(i in 1:length(lines)){
    linenumber = c()
    linenumber_group = c()
    linenumber_pft =c()
    linenumber_st = c()
    linenumber_group = grep("group ", x = substr(lines[i], start = 1, stop = 6))
    linenumber_pft = grep("pft ", x = substr(lines[i], start = 1, stop = 4))
    linenumber_st = grep("st ", x = substr(lines[i], start = 1, stop = 3))
    linenumber = c(linenumber_group, linenumber_pft, linenumber_st)
    if(length(linenumber)!=0){
      endoufgroup = c()
      endofgroup = grep(")", x = substr(lines[(i+1):length(lines)],1,2), fixed = T)[1]
      pftlines = c(pftlines, lines[i:(i+endofgroup)],"\n")
    }
  }

  titel = lines[titleline]
  files = lines[filelines]
  files = gsub("^file","!file", files)

  # delete lines which are titlelines, filelines ( which we alter to be
  # all outcommented), and outcommented lines and pftlines

  lines = lines[-c(which(titleline),which(filelines),
                   which(outcommented),which(lines %in% pftlines))]

  # grep empty and tabed lines and delete them

  tablines = grepl("\t", x = substr(lines, start =1, stop = 2))
  emptylines = grepl(" ", substr(lines, start = 1, stop =2))
  lines = lines[-c(which(tablines), which(emptylines))]

  # get the unique designnames from the parameters

  designnames = sapply(strsplit(lines," "), function (x) x[1])
  duplicates_design = which(duplicated(designnames) ==T)

  first_apperance = c()
  for(i in duplicates_design){
    first_apperance <- c(first_apperance, which(designnames == designnames[i])[1])
  }
  design = lines[-first_apperance]

  # write the PFT or Species file which includes the title, the files,
  # the designvariables and all pft/speciesspecific lines into the
  # main Pft file

  Pftcon <- file(NamePftFile)
  writeLines(c(titel,headingfileline,files,"\n",design, "\n",pftlines),Pftcon)
  close(Pftcon)

  # remove the main file written in the beginning because no more need

  file.remove("./main_tryout.ins")

  return(list(files = parameterfiles, variables = parametervariables,
              pfts = pftlines, design = design ))

}

# @description This function infers the correct order of the instruction files
# and returns it
# @param MainInputFile is the main input file see InferParameterAndDesignList
# @return returns a vector with the names of correct ordered instruction files
# @author Johannes Oberpriller

infer_instruction_order <- function(MainInputFile){

  # get the main directory where the instruction file is stored

  dashes = gregexpr("/",MainInputFile[["main"]])

  lastdash = dashes[[1]][length(dashes[[1]])]

  maindirectory = substr(MainInputFile[["main"]],start = 1,
                         stop = lastdash)

  # get all imports from the file

  imports = get_imports(MainInputFile[["main"]],maindirectory = maindirectory)

  # order the imports like they are in the main instruction file

  ordered_imports = imports
  i = 1
  while(i <= length(ordered_imports)){
    new_imports = get_imports(ordered_imports[[i]], maindirectory = maindirectory)
    if(any((new_imports %in% ordered_imports))){
      i = i+1
    }
    else{
      if(length(new_imports) == 0){
        i = i+1
      }
      else{
        number_new = length(new_imports)
        number_old = length(ordered_imports)
        if((i+1)>number_old){
          ordered_imports[number_new + number_old] = ordered_imports[number_old]
          ordered_imports[i:(i+number_new-1)] = new_imports
        }
        else{
          ordered_imports[(i+number_new):(number_new+number_old)] = ordered_imports[i:number_old]
          ordered_imports[(i):(i+number_new-1)] = new_imports
        }
      }
    }
  }
  # return the correct order
  return(c(MainInputFile, ordered_imports))
}

# @description this function takes the main file and the main directroy
# and returns the imported files
# @param a file with all instruction files below each other
# @param maindirectory the main directory for the analysis
# @return returns the ordered imports
# @author Johannes Oberpriller


get_imports <- function(file,maindirectory){

  # read main file

  linesfile = readLines(file)
  imports = list()
  import_number = c()

  # loop over files and get instruction order
  for(i in 1:length(linesfile)){
    is_in = grepl("import", linesfile[i])
    out_commented = grepl("!import",substr(linesfile[i], start = 1, stop = 8))
    if(is_in == T){
      if(out_commented == T){
        next
      }
      else{
        import_number =c(import_number, i)
      }
    }
    else
    {
      next
    }
  }
  # check that the imports have all the same apperance in code
  ins_order = 1
  for(i in import_number){
    imports_file = substr(x=linesfile[i],
                          start = gregexpr("\"",linesfile[i])[[1]][1]+1,
                          stop = gregexpr("\"",linesfile[i])[[1]][2]-1)
    if(substr(imports_file,1,2) =="./"){
      imports_file = substr(imports_file,3,stop =100)
    }
    imports[ins_order] = paste0(maindirectory,imports_file)
    ins_order = ins_order +1
  }
  return(imports)
}
