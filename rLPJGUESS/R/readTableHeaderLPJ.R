# @title A function to read LPJ-GUESS ASCII outputs
# @description This function reads the ASCII outputs produced by LPJ-GUESS in the
# case that the files have columns names not separated by space.
# @param x a character string indicating path to the output file to be read
# @return a data frame with the right headers
# @author Ramiro Silveyra Gonzalez
# @note Based on http://stackoverflow.com/questions/7988959/splitting-string-based-on-letters-case
readTableHeaderLPJ <- function(x){
  headerLine <- readLines(x,n=1)
  headerLineClean <- gsub('^|([[:lower:]])([[:upper:]])', '\\1 \\2', headerLine)
  headerLineClean  <- gsub('([[:upper:]]+[[:upper:]])([[:upper:]]+[[:lower:]])', '\\1 \\2\\3',
                           headerLineClean, perl = TRUE)
  splitted_clean <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", headerLineClean, perl=TRUE)
  columnNames <- unlist(strsplit(splitted_clean, " "))
  df <- read.table(x, header =F, skip = 1)

  if(ncol(df) != length(columnNames)){
    stop("Model output is not readable")
  }else{
    names(df) <- columnNames
  }
  return(df)
}



