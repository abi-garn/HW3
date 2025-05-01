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
file1 = listOfFiles[1]; file1
file2 = listOfFiles[2]; file2

data = data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA); 
names(data) = c("Title Of Event", "Classification", "Priority", "SnortID", "Date-Time", "SourceIP", "SourceIP : Port", "DestinationIP", "DestinationIP : Port", "Protocol", "TTL", "TOS", "ID", "IpLen", "DgmLen", "Extra-AfterDgmLen", "Combo", "Additional Lines", "Error Type & Code", "Error Name", "Data Dump")


forRegularLines <- function(line){
  anyExtraInfoAfterDgm = grepl("DgmLen\\:[0-9]+(\\s[A-Za-z]+\\s)\\*\\*\\*.*", line)
  extraInfoAfterDgm = NA
  if (anyExtraInfoAfterDgm){
    extraInfoAfterDgm = sub("DgmLen\\:[0-9]+(\\s[A-Za-z]+\\s)\\*\\*\\*.*", "\\1", line)
    extraInfoAfterDgm = trimws(extraInfoAfterDgm)
  }
  info = attachCombinedData(line)
  extraLines = sub(".*TcpLen\\:\\s[0-9]+\\s(.*)", "\\1", line)
  return(c(extraInfoAfterDgm, info, extraLines))
}


attachCombinedData <- function(line){
  tcpflag = sub(".*\\*\\*\\*(.*)\\*\\*\\*.*", "\\1", line)
  seq = sub(".*Seq\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  ack = sub(".*Ack\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  window = sub(".*Win\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  tcpLen = sub(".*TcpLen\\:\\s([0-9]+)\\s+.*", "\\1", line)
  info = paste(tcpflag, seq, ack, window, tcpLen)
  return(info)
}



forErrors <- function(line){
  errorType = sub(".*Type\\:([0-9]+).*", "\\1", line); 
  errorCode = sub(".*Code\\:([0-9]+).*", "\\1", line);
  errorTypeAndCode = paste0(errorType, ",", errorCode)
  errorName = sub(".*Code\\:[0-9]+\\s+(.*)\\s+\\*\\*\\s+ORIG.*", "\\1", line);
  dataDump = sub(".*DUMP\\:\\s+(.*)\\s+\\*\\*\\sEND.*", "\\1", line);
  return(c(errorTypeAndCode, errorName, dataDump))
}


returnExtraInfo <- function(line){
  condition = grepl("^Type", line)
  if (!condition){
    list = forRegularLines(line)
    incrementDF = data.frame(list[1], list[2], list[3], NA, NA, NA)
  } else {
    list = forErrors(line)
    incrementDF = data.frame(NA, NA, NA, list[1], list[2], list[3])
  }
  names(incrementDF) = c("Extra-AfterDgmLen", "Combo", "Additional Lines", "Error Type & Code", "Error Name", "Data Dump")
  return(incrementDF)
}



extractEverything <- function(file){
  allLines = readFile(file)
  theLines.grouped = readFile.2(allLines);
  result = unlist(theLines.grouped);
  headers = sub("(\\[*\\*].*?\\[*\\*]).*", "\\1", result); headers[1]
  
  snortIDs = sub(".*(\\[[0-9\\:]+\\]).*", "\\1", headers); snortIDs[100]
  titlesOfEvents = sub(".*\\]\\s(.*)\\s\\[.*", "\\1", headers); titlesOfEvents[1]
  #Second Line extractions
  secondLines = sub(".*(\\[Classification:.*Priority:[^]]*]).*", "\\1", result);secondLines[1]
  classifications = sub("\\[Classification:\\s([^]]*).*", "\\1", secondLines); classifications[1]
  priorities = sub(".*\\[Priority:\\s([^]]*).*", "\\1", secondLines); priorities[1]
  
  #Final extractions
  #Third Line extractions
  remainingLines = sub(".*Priority\\: [0-9]\\]\\s+([0-9]+\\/.*).*", "\\1", result);remainingLines[1]
  thirdLine = sub("([^a-zA-Z]*).*", "\\1", remainingLines);thirdLine[1]
  thirdLine = trimws(thirdLine)
  print(thirdLine)
  
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
  
  #Last Line extractions
  #I need to get all info after dgmlength
  lastLines = sub(".*DgmLen\\:[0-9]+\\s(Type.*)", "\\1", remainingLines);
  lastLines.replacement = sub(".*(DgmLen\\:[0-9]+\\s.*)", "\\1", remainingLines);
  
  
  
  newDataFrame = data.frame(snortIDs, titlesOfEvents, classifications, priorities, dateTimes, sourceIP, sourceIP.port, destinationIP, destinationIP.port, protocol, ttl, tos, id, iplength, dgmlength, stringsAsFactors=FALSE)
  condition = grepl("^Type", lastLines)
  lastLines = ifelse(condition, lastLines, lastLines.replacement)
  
  
  extraInfo = lapply(lastLines,returnExtraInfo)
  allExtraInfo = do.call(rbind, extraInfo)
  
  newDataFrame2= cbind(newDataFrame, allExtraInfo)
  
  return(newDataFrame2)
  
}
  

newDataframe = extractEverything(file2)
View(newDataframe)



View(returnExtraInfo("DgmLen:91 DF ***AP*** Seq: 0x1989FB74  Ack: 0x6D4515E5  Win: 0xFB  TcpLen: 32 TCP Options (3) => NOP NOP TS: 999976349 8824698"))









links = getLinks(file1);unique(links)

getLinks <- function(file){
  allLines = readFile(file)
  theLines.grouped = readFile.2(allLines);
  result = unlist(theLines.grouped);
  loc = (grepl("http", result)); loc
  wow = result[loc]
  
  first = gsub(".*http", "", wow); first
  second = gsub("].*", "", first);second
  new = paste0("http", second); new
  return(new)
}
