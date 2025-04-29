library(stringr)

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
  emptyLines.loc = cumsum(allLines == "");
  #Split up lines into groups using blank lines
  lines = by(allLines, emptyLines.loc, paste, collapse=" ");
  return(trimws(lines))
}
listOfFiles = unzip(zippedFiles); 

22
theDataFrame = data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA); View(theDataFrame)
names(theDataFrame) = c("Title Of Event", "Classification", "Priority", "SnortID", "Date-Time", "SourceIP", "SourceIP : Port", "DestinationIP", "DestinationIP : Port", "Protocol", "TTL", "TOS", "ID", "IpLen", "DgmLen", "Extra-AfterDgmLen", "Combo", "Additional Lines", "Error Type & Code", "Error Name", "Data Dump")


allLines = lapply(listOfFiles,readFile); allLines

theLines.grouped = lapply(allLines,readFile.2); unlist(theLines.grouped[1])[2]
result = unlist(theLines.grouped); result[2]
resultLength = length(result)



#Header-based extractions
headers = sub("(\\[*\\*].*?\\[*\\*]).*", "\\1", result); headers[1]

snortIDs = sub(".*(\\[[0-9\\:]+\\]).*", "\\1", headers); snortIDs[100]
titlesOfEvents = sub(".*\\]\\s(.*)\\s\\[.*", "\\1", headers); titlesOfEvents[1]


#Second Line extractions
secondLines = sub(".*(\\[Classification:.*Priority:[^]]*]).*", "\\1", result);secondLines[1]
classifications = sub("\\[Classification:\\s([^]]*).*", "\\1", secondLines); classifications[1]
priorities = sub(".*\\[Priority:\\s([^]]*).*", "\\1", secondLines); priorities[1]

#Final extractions
#Third Line extractions
remainingLines = sub(".*([0-9]+\\/.*).*", "\\1", result);remainingLines[1]
thirdLine = sub("([^a-zA-Z]*).*", "\\1", remainingLines);thirdLine[1]
thirdLine = trimws(thirdLine)

dateTimes = sub(".*([0-9]+\\/[0-9]+\\-[0-9]+\\:[0-9]+\\:[0-9]+\\.[0-9]+).*", "\\1", thirdLine); dateTimes[1]
sourceIP = sub(".*(\\s[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", thirdLine); sourceIP[5]
sourceIP = trimws(sourceIP); sourceIP[1]
sourceIP.port = sub(".*\\.[0-9]+\\s[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\:([0-9]+).*", "\\1", thirdLine); sourceIP.port[2]


destinationIP = sub(".*\\>(\\s[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", thirdLine); destinationIP[1]

destinationIP.port = sub(".*\\:([0-9]+).*", "\\1", thirdLine); destinationIP.port[1]

#Fourth Line extractions
fourthLine = sub(".*\\s[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\:[0-9]+(.*DgmLen\\:[0-9]+).*", "\\1", remainingLines);fourthLine[1]

protocol = sub("([A-Za-z]+).*", "\\1", fourthLine);protocol[1]
protocol = trimws(protocol)

ttl = sub(".*TTL\\:([0-9]+).*", "\\1", fourthLine);ttl[1]
tos = sub(".*TOS\\:([0-9]+x[0-9]+).*", "\\1", fourthLine);tos[1]
id = sub(".*ID\\:([0-9]+).*", "\\1", fourthLine);id[1]
iplength = sub(".*IpLen\\:([0-9]+).*", "\\1", fourthLine);iplength[1]
dgmlength = sub(".*DgmLen\\:([0-9]+).*", "\\1", fourthLine);dgmlength[1]


for (i in 1:resultLength){
  theDataFrame$"SnortID" =  snortIDs[i]
  theDataFrame$"Title Of Event" =  titlesOfEvents[i]
  theDataFrame$"Classification" = classifications[i]
  theDataFrame$"Priority" = priorities[i]
  theDataFrame$"Date-Time" = dateTimes[i]
  theDataFrame$"SourceIP" = sourceIP[i]
  theDataFrame$"SourceIP : Port" = sourceIP.port[i]
  theDataFrame$"DestinationIP" = destinationIP[i]
  theDataFrame$"DestinationIP : Port" = destinationIP.port[i]
  theDataFrame$"Protocol" = protocol[i]
  theDataFrame$"TTL" = ttl[i]
  theDataFrame$"TOS" = tos[i]
  theDataFrame$"ID" = id[i]
  theDataFrame$"IpLen" = iplength[i]
  theDataFrame$"DgmLen" = dgmlength[i]
}



#Last Line extractions
  #I need to get all info after dgmlength
lastLines = sub(".*DgmLen\\:[0-9]+\\s(Type.*)", "\\1", remainingLines);
lastLines.replacement = sub(".*(DgmLen\\:[0-9]+\\s.*)", "\\1", remainingLines);


for (i in 1:length(lastLines)){
  element = lastLines[i]
  condition = grepl("^Type", element)
  if (!condition){
    lastLines[i] = lastLines.replacement[i]
  }
}


for (i in 1:length(lastLines)){
  element = lastLines[i]
  condition = grepl("^Type", element)
  ifelse(condition, forRegularLines(element,i), forErrors(element,i))
}

forRegularLines <- function(line, index){
  anyExtraInfoAfterDgm = grepl("DgmLen\\:[0-9]+(\\s[A-Za-z]+\\s)\\*\\*\\*.*", line)
  if (anyExtraInfoAfterDgm){
    extraInfoAfterDgm = sub("DgmLen\\:[0-9]+(\\s[A-Za-z]+\\s)\\*\\*\\*.*", "\\1", line)
    extraInfoAfterDgm = trimws(extraInfoAfterDgm)
    theDataFrame$"Extra-AfterDgmLen" = extraInfoAfterDgm
  }
  attachCombinedData(line, index)
  extraLines = sub(".*TcpLen\\:\\s(.*)", "\\1", line())
  theDataFrame$"Additional Lines" =  extraLines
}

attachCombinedData <- function(line, index){
  tcpflag = sub(".*\\*\\*\\*(.*)\\*\\*\\*.*", "\\1", line)
  seq = sub(".*Seq\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  ack = sub(".*Ack\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  window = sub(".*Win\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  tcpLen = sub(".*TcpLen\\:\\s([0-9]+).*", "\\1", line)
  info = paste(tcpflag, seq, ack, window, tcpLen, ", ")
  theDataFrame$"Combo" =  info
  
}

forErrors <- function(line, index){
  errorType = sub(".*Type\\:([0-9]+).*", "\\1", line); 
  errorCode = sub(".*Code\\:([0-9]+).*", "\\1", line);
  errorTypeAndCode = paste(errorType, errorCode, tcpLen, ", ")
  theDataFrame$"Error Type & Code" =  errorTypeAndCode
  errorName = sub(".*Code\\:[0-9]+\\s+(.*)\\*\\*\\sORIG.*", "\\1", line);
  theDataFrame$"Error Name" =  errorName
  dataDump = sub(".*DUMP\\:\\s+(.*)\\s\\*\\*\\sEND.*", "\\1", line);
  theDataFrame$"Data Dump" =  dataDump
}




links = getLinks(result);unique(links)



getLinks <- function(result){
  loc = (grepl("http", result)); loc
  wow = result[loc]
  
  first = gsub(".*http", "", wow); first
  second = gsub("].*", "", first);second
  new = paste0("http", second); new
  return(new)
}

























