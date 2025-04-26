#In order to turn the .clm file within each zip file into a data frame, I knew 
#that I would need to first deal with the zipped files
setwd("/Users/abigarn/Desktop/DavisWork/Yr1Q3/STA141B/HMW/Assignment3")

#We utilize our functions below

#Preparing the data
#We identify the zipped file that we will be working with
zippedFiles = ("./alert.zip"); 



readFile <- function(file){
  allLines = readLines(file)
  return(allLines)
}
readFile.2 <- function(allLines){
  #Keep track of where lines are blank
  emptyLines.loc = cumsum(allLines == '');
  #Split up lines into groups using blank lines
  lines = by(allLines, emptyLines.loc, paste, collapse=" ");
  return(trimws(lines))
}
listOfFiles = unzip(zippedFiles); 




allLines = lapply(listOfFiles,readFile); allLines

theLines.grouped = lapply(allLines,readFile.2); unlist(theLines.grouped[1])[2]
result = unlist(theLines.grouped); result[2]




































links = getLinks(result);unique(links)



getLinks <- function(result){
  loc = (grepl("http", result)); loc
  wow = result[loc]
  
  first = gsub(".*http", "", wow); first
  second = gsub("].*", "", first);second
  new = paste0("http", second); new
  return(new)
}

























