library(stringr)

#In order to turn the .clm file within each zip file into a data frame, I knew 
#that I would need to first deal with the zipped files
setwd("/Users/abigarn/Desktop/DavisWork/Yr1Q3/STA141B/HMW/Assignment3")

#We utilize our functions below

#Preparing the data
#We identify the zipped file that we will be working with
zippedFiles = ("./alert.zip"); 



groupLines <- function(allLines){
  #Keep track of where lines are blank
  emptyLines.loc = cumsum(allLines == "");
  #Split up lines into groups using blank lines
  lines = by(allLines, emptyLines.loc, paste, collapse=" ");
  return(trimws(lines))
}
listOfFiles = unzip(zippedFiles); 
file1 = listOfFiles[1]; file1
file2 = listOfFiles[2]; file2



forRegularLines <- function(line){
  anyExtraInfoAfterDgm = grepl("DgmLen\\:[0-9]+(\\s[A-Za-z]+\\s)\\*\\*\\*.*", line)
  extraInfoAfterDgm = ""
  if (anyExtraInfoAfterDgm){
    extraInfoAfterDgm = sub("DgmLen\\:[0-9]+(\\s[A-Za-z]+\\s)\\*\\*\\*.*", "\\1", line)
    extraInfoAfterDgm = trimws(extraInfoAfterDgm)
  }
  info = attachCombinedData(line)
  extraLines = sub(".*TcpLen\\:\\s[0-9]+(.*)", "\\1", line)
  return(c(extraInfoAfterDgm, info, extraLines))
}


attachCombinedData <- function(line){
  tcpflag = sub(".*\\*\\*\\*([^*]+)\\*\\*\\*.*", "\\1", line)
  seq = sub(".*Seq\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  ack = sub(".*Ack\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  window = sub(".*Win\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  tcpLen = sub(".*TcpLen\\:\\s+([0-9]+).*", "\\1", line)
  info = paste(tcpflag, seq, ack, window, tcpLen)
  return(info)
}



forErrorLines <- function(line){
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
    incrementDF = data.frame(list[1], list[2], list[3], "", "", "")
  } else {
    list = forErrorLines(line)
    incrementDF = data.frame("", "", "", list[1], list[2], list[3])
  }
  names(incrementDF) = c("Extra-AfterDgmLen", "Combo", "Additional Lines", "Error Type & Code", "Error Name", "Data Dump")
  return(incrementDF)
}




returnDF <- function(file){
  allLines = readLines(file)
  theLines.grouped = groupLines(allLines);
  result = unlist(theLines.grouped);
  headers = sub("(\\[*\\*].*?\\[*\\*]).*", "\\1", result); 
  snortIDs = sub(".*(\\[[0-9\\:]+\\]).*", "\\1", headers); 
  snortIDs = as.character(snortIDs)
  titlesOfEvents = sub(".*\\]\\s(.*)\\s\\[.*", "\\1", headers); 
  titlesOfEvents = as.character(titlesOfEvents)
  #Second Line extractions
  secondLines = sub(".*(\\[Classification:.*Priority:[^]]*]).*", "\\1", result);
  classifications = sub("\\[Classification:\\s([^]]*).*", "\\1", secondLines);
  classifications = as.character(classifications)
  priorities = sub(".*\\[Priority:\\s([^]]*).*", "\\1", secondLines);
  priorities = as.numeric(priorities)
  
  #Final extractions
  #Third Line extractions
  remainingLines = sub(".*Priority\\: [0-9]\\]\\s+([0-9]+\\/.*).*", "\\1", result);
  thirdLine = sub("([^a-zA-Z]*).*", "\\1", remainingLines);
  thirdLine = trimws(thirdLine)

  dateTimes = sub(".*([0-9]+\\/[0-9]+\\-[0-9]+\\:[0-9]+\\:[0-9]+\\.[0-9]+).*", "\\1", thirdLine); 
  dateTimes = as.character(dateTimes)
  sourceIP = sub(".*(\\s[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", thirdLine);
  sourceIP = as.character(sourceIP)
  sourceIP = trimws(sourceIP); 
  sourceIP.port = sub(".*\\.[0-9]+\\s[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\:([0-9]+).*", "\\1", thirdLine); 
  sourceIP.port = as.numeric(sourceIP.port);
  sourceIP.port = ifelse(is.na(sourceIP.port), "", sourceIP.port)
  

  destinationIP = sub(".*\\>\\s+([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", thirdLine);
  destinationIP = as.character(destinationIP)
  destinationIP.port = sub(".*\\:([0-9]+).*", "\\1", thirdLine); 
  destinationIP.port = as.numeric(destinationIP.port);
  #Fourth Line extractions
  fourthLine = sub(".*\\s[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\:[0-9]+(.*DgmLen\\:[0-9]+).*", "\\1", remainingLines);
  
  protocol = sub("([A-Za-z]+).*", "\\1", fourthLine);

  protocol = trimws(protocol)
  protocol = as.character(protocol)

  ttl = sub(".*TTL\\:([0-9]+).*", "\\1", fourthLine);
  ttl = as.numeric(ttl)
  tos = sub(".*TOS\\:([0-9]+x[0-9]+).*", "\\1", fourthLine);
  tos = as.character(tos)
  id = sub(".*ID\\:([0-9]+).*", "\\1", fourthLine);
  id = as.numeric(id)
  iplength = sub(".*IpLen\\:([0-9]+).*", "\\1", fourthLine);
  iplength = as.numeric(iplength)
  dgmlength = sub(".*DgmLen\\:([0-9]+).*", "\\1", fourthLine);
  dgmlength = as.numeric(dgmlength)
  #Last Line extractions
  #I need to get all info after dgmlength
  lastLines = sub(".*DgmLen\\:[0-9]+\\s(Type.*)", "\\1", remainingLines);
  lastLines.replacement = sub(".*(DgmLen\\:[0-9]+\\s.*)", "\\1", remainingLines);
  
 
  
  newDataFrame = data.frame(snortIDs, titlesOfEvents, classifications, priorities, dateTimes, sourceIP, sourceIP.port, destinationIP, destinationIP.port, protocol, ttl, tos, id, iplength, dgmlength, stringsAsFactors=FALSE)
  names(newDataFrame) = c("Title Of Event", "Classification", "Priority", "SnortID", "Date-Time", "SourceIP", "SourceIP : Port", "DestinationIP", "DestinationIP : Port", "Protocol", "TTL", "TOS", "ID", "IpLen", "DgmLen")
  condition = grepl("^Type", lastLines)

  lastLines = ifelse(condition, lastLines, lastLines.replacement)
  
  
  extraInfo = lapply(lastLines,returnExtraInfo)
  allExtraInfo = do.call(rbind, extraInfo)
  newDataFrame2= cbind(newDataFrame, allExtraInfo)
  newDataFrame2.length = nrow(newDataFrame2)
  return(newDataFrame2[-(newDataFrame2.length),])
  
}



newDataframe = returnDF(file1)

View(newDataframe)


getLinks <- function(file){
  allLines = readLines(file)
  theLines.grouped = groupLines(allLines);
  result = unlist(theLines.grouped);
  loc = (grepl("http", result)); 
  relevantLineGroups = result[loc]
  
  
  includingLinkContent = gsub(".*http", "", relevantLineGroups );
  linkBody = gsub("].*", "", includingLinkContent);
  links = paste0("http", linkBody); 
  return(links)
}

links = getLinks(file1);unique(links)








summary(newDataframe)
names(newDataframe)
dim(newDataframe)
sapply(newDataframe, class)
any(is.na(newDataframe))



#Verifying if each Title Of Event is in the correct format
table(grepl("\\[[0-9]\\:[0-9]+\\:[0-9]+\\]", newDataframe$`Title Of Event`))
#Verifying if each date-time is in the correct format
table(grepl("([0-9]+\\/[0-9]+\\-[0-9]+\\:[0-9]+\\:[0-9]+\\.[0-9]+)", newDataframe$`Date-Time`))
#Verifying if each SourceIP and DestinationIP is in the correct format
table(grepl("[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+", newDataframe$SourceIP))
table(grepl("[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+", newDataframe$DestinationIP))


