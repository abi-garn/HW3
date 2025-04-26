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

a = c("cool", "all httpabbba all", "abbb", "abb http all")
a = unlist(a); a
loc = (grepl("http", a)); 
relevantStrings = a[loc]; relevantStrings

first = gsub(".*http", "", relevantStrings); first
second = gsub("].*", "", first);second
new = paste0("http", second); new

spacingloc = cumsum(grepl(" ", relevantStrings)); spacingloc
result = relevantStrings[spacingloc]; result




allLines = lapply(listOfFiles,readFile); allLines

theLines.grouped = lapply(allLines,readFile.2); unlist(theLines.grouped[1])[2]
result = unlist(theLines.grouped[2]); result[100]

loc2 = (grepl("http", result)); loc2
result[loc2]


loc = lapply(result, function(x) (grepl("http", x))); loc
result[loc]
relevantStrings = result[loc]; relevantStrings


View(unlist(allLines[1])[2])

allLines.file1 = unlist(theLines.grouped[1])
linkStart = cumsum(grep("http:", allLines.file1)); table(linkStart)
linkStart = cumsum(grepl("http:", allLines.file1)); table(linkStart)
regex(allLines.file1)
linkStart
linkEnd = table(cumsum(grepl("^\\s*$", allLines.file1))); linkEnd

#I need to find where "http" is
  #Can I make 

b = strsplit(allLines.file1, " ")


getLinks <- function(theChunk){
  splitLines = strsplit(theChunk, " ")
  theIndex = lapply(splitLines, function(x) which(grepl("http", x) == TRUE))
  return(splitLines[theIndex])
}


lapply(allLines.file1, getLinks)


lapply(b[100], function(x) which(grepl("http", x) == TRUE))

which(allLines.file1 == "http")
regex("http")










a = c("a", "all", "b")
as.character(a)
cumsum(grepl("l", a)) #Yay!
cumsum(grepl("\\A", allLines.file1[2])) #Yay!



# a = c(1,2)
# grep(1,a)





#table(sapply(else, length))
#table(grepl("^#", ll[nels==1]))
#gsub("^t=", "", list[1]) --> Replaces AnythingStartingWitht with ""
#do.call(Function, args)
#str_detect(pattern1, regex(.pattern., dotall = TRUE)) --> Resukt:True
#str_extract(object, patternToFind) = str_extract(object, regex(patternToFind))
#Match one parentheses --> Use: "\\" in reg expressions
#Checking if strings match
  #str_detect(anObject, paste0(arg, pattern)) --> 'pattern' does not have to be found 
    #EX: aeb and a.b.c --> Both evalaute true when pattern="a.b"
  #str_detect(anObject, paste0("^\\Q", pattern, "\\E")) --> 'pattern' must be found exactly in 'anObject'

#\d --> Matches any character
  #EX: str_detect(1, "\\d)
#\s --> Matches any whitespace
  #EX: str_detect("a a", "\\s+")

#[] --> Allows you to create character class
  #EX: [\^\-] --> Matches ^ or -
  #EX: [\#b] --> Matches # or b
  #EX: [ab] --> Matches a or b
  #EX: [:upper:] --> Matches uppercase digits
  #EX: [:punct:] --> Matches punctuation
  #EX: [:alpa:] --> Matches letters

#^ --> A string's beginning
#$ --> A string's end 
  #str_extract(x, "a$")

#operators get changed by regex(multiline=TRUE)
  #\A --> matches the start of the input
  #\z --> Matches the end of the input

dataFrame = data.frame("TitleOfEvent", "Classification", "Priority", "Snort ID", "Date-Time", , "TTL", "TOS", "ID", "IpLen", "DgmLen", "DgmLen-Text", "SeqAckWinTcpLen", "Other")






#Reading our data:
#Function1: Create our dataframe
#Function2: Create our dataframe


# a = seq(1,2,1); a
# b = lapply(b, function(x) doIt(a)); b
# a = lapply(b, function(x) print(x))

# doIt <- function(d){
#   b = d*2
#   return(b)
# }
