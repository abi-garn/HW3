

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
  anyExtraInfoAfterDgm = grepl("DgmLen\\:[0-9]+(\\s[A-Za-z]+\\s)\\*\\*\\*", line)
  extraInfoAfterDgm = ""
  if (anyExtraInfoAfterDgm){
    extraInfoAfterDgm = trimws(sub("DgmLen\\:[0-9]+(\\s[A-Za-z]+\\s)\\*\\*\\*.*", "\\1", line))
  }
  info = attachCombinedData(line)
  extraLines = sub(".*?TcpLen:\\s[0-9]+\\s+", "\\1", line)
  return(c(extraInfoAfterDgm, info, extraLines))
}


attachCombinedData <- function(line){
  tcpflag = sub(".*?\\*\\*\\*([^*]+)\\*\\*(\\*)?.*", "\\1", line)
  seq = sub(".*?Seq\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  ack = sub(".*?Ack\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  window = sub(".*?Win\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
  tcpLen = sub(".*?TcpLen\\:\\s+([0-9]+).*", "\\1", line)
  info = paste(tcpflag, seq, ack, window, tcpLen)
  return(info)
}



forErrorLines <- function(line){
  errorType = sub(".*?Type\\:([0-9]+).*", "\\1", line); 
  errorCode = sub(".*?Code\\:([0-9]+).*", "\\1", line);
  errorTypeAndCode = paste0(errorType, ",", errorCode)
  errorName = sub(".*?Code\\:[0-9]+\\s+(.*)\\s+\\*\\*\\s+ORIG", "\\1", line);
  dataDump = sub(".*?DUMP\\:\\s+(.*)\\s+\\*\\*\\sEND", "\\1", line);
  return(c(errorTypeAndCode, errorName, dataDump))
}


returnExtraInfo <- function(line){
  condition = grepl("^Type", line)
  if (!condition){
    list = forRegularLines(line)
    incrementDF = data.frame(list[1], list[2], list[3], "", "", "")
  } else if (grepl(".*?UDP.*", line)){
    incrementDF = data.frame("", "", "", "", "", "")
  } else {
    list = forErrorLines(line)
    incrementDF = data.frame("", "", "", list[1], list[2], list[3])
  }
  names(incrementDF) = c("Extra-AfterDgmLen", "Combo", "Additional Lines", "Error Type & Code", "Error Name", "Data Dump")
  return(incrementDF)
}





returnDF <- function(file){
  cat("\nProcessing Lines...\n------")
  allLines = readLines(file)
  theLines.grouped = groupLines(allLines);
  cat("\nFinding header lines...\n------")
  result = unlist(theLines.grouped);
  headers = sub(".*?(\\[*\\*].*?\\[*\\*]).*", "\\1", result); 
  
  cat("\nProcessing info...\n------")
  snortIDs = as.character(sub(".*\\[([0-9]+\\:[0-9]+\\:[0-9]+).*", "\\1", headers)); 

  titlesOfEvents = as.character(sub(".*\\]\\s+([^\\[]+).*", "\\1", headers)); 
  
  #Second Line extractions
  cat("\nFinding second lines...\n------")
  secondLines = sub(".*?(\\[Classification:.*?Priority:[^]]*\\]).*", "\\1", result);
  cat("\nProcessing info...\n------")
  classifications = as.character(sub(".*?Classification\\:\\s+([^]]+)\\].*", "\\1", secondLines));
  priorities = as.numeric(sub(".*Priority:\\s([0-9]+).*", "\\1", secondLines));
  
  #Final extractions
  #Third Line extractions
  cat("\nFinding third lines...\n------")
  remainingLines = sub(".*?Priority\\: [0-9]\\]\\s+([0-9]+\\/.*)", "\\1", result);
  thirdLine = trimws(sub("([^a-zA-Z]*)", "\\1", remainingLines));
  
  cat("\nProcessing info...\n------")
  dateTimes = as.character(sub(".*?([0-9]+\\/[0-9]+\\-[0-9]+\\:[0-9]+\\:[0-9]+\\.[0-9]+).*", "\\1", thirdLine)); 
  sourceIP = trimws(as.character(sub(".*?\\s+([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", thirdLine)));
  sourceIP = ifelse(grepl("^[0-9]+\\/", sourceIP), "", sourceIP)
  sourceIP.port = as.numeric(sub(".*?\\.[0-9]+\\s[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\:([0-9]+).*", "\\1", thirdLine)); 
  sourceIP.port = ifelse(is.na(sourceIP.port), "", sourceIP.port)
  sourceIP.port = ifelse(grepl("^[0-9]+\\/", sourceIP.port), "", sourceIP.port)
  
  destinationIP = as.character(sub(".*\\s+->\\s+([^ ]+)\\:.*", "\\1", thirdLine));
  destinationIP.port = sub(".*?->.*?\\:([0-9]+).*", "\\1", thirdLine); 
  destinationIP.port = as.numeric(destinationIP.port);
  #Fourth Line extractions
  cat("\nFinding fourth lines...\n------")
  fourthLine = sub(".*?\\s[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\:[0-9]+(.*DgmLen\\:[0-9]+)", "\\1", remainingLines);
  cat("\nProcessing info...\n------")
  
  protocol = as.character(trimws(sub(".*?\\:[0-9a-z]+\\s+([A-Z0-9]+(\\-[A-Z0-9]+)?).*", "\\1", remainingLines)));

  ttl = as.numeric(sub(".*?TTL\\:([0-9]+).*", "\\1", fourthLine));
  tos = as.character(sub(".*?TOS\\:([0-9]+x[0-9A-Z]+).*", "\\1", fourthLine));
  id = as.numeric(sub(".*?ID\\:([0-9]+).*", "\\1", fourthLine));
  iplength = as.numeric(sub(".*?IpLen\\:([0-9]+).*", "\\1", fourthLine));
  dgmlength = as.numeric(sub(".*?DgmLen\\:([0-9]+).*", "\\1", fourthLine));
  #Last Line extractions
  #Involves getting all info after dgmlength
  cat("\nFinding final lines...\n------")
  lastLines = sub(".*DgmLen\\:[0-9]+\\s(Type.*)", "\\1", remainingLines);
  lastLines.replacement = sub(".*?(DgmLen\\:[0-9]+\\s.*)", "\\1", remainingLines);
  print(lastLines[9701])
 
  cat("\nProcessing info...\n------")
  newDataFrame = data.frame(titlesOfEvents, classifications, priorities, snortIDs,  dateTimes, sourceIP, sourceIP.port, destinationIP, destinationIP.port, protocol, ttl, tos, id, iplength, dgmlength, stringsAsFactors=FALSE)
  names(newDataFrame) = c("Title Of Event", "Classification", "Priority", "SnortID", "Date-Time", "SourceIP", "SourceIP : Port", "DestinationIP", "DestinationIP : Port", "Protocol", "TTL", "TOS", "ID", "IpLen", "DgmLen")
  condition = grepl("^Type", lastLines)

  lastLines = ifelse(condition, lastLines, lastLines.replacement)
  
  cat("\nIntegrating extra info...\n------")
  
  extraInfo = lapply(lastLines,returnExtraInfo)
  allExtraInfo = do.call(rbind, extraInfo)
  newDataFrame2= cbind(newDataFrame, allExtraInfo)
  newDataFrame2.length = nrow(newDataFrame2)
  cat("\nReturning...\n------")
  return(newDataFrame2[-(newDataFrame2.length),])
  
}


#Creating the data frame for our first file
newDataframe1 = returnDF(file1)
#Creating the data frame for our second file
newDataframe2 = returnDF(file2)
#Combining the data frames
combinedDataframe = rbind(newDataframe1, newDataframe2)
View(newDataframe1)
View(combinedDataframe)


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
line = "03/16-07:31:55.960000 192.168.202.89:58332 -> 10.20.250.49:161 UDP TTL:128 TOS:0x0 ID:3284 IpLen:20 DgmLen:105 Len: 77 [Xref => http://cve.mitre.org/cgi-bin/cvename.cgi?name=2002-0013][Xref => http://cve.mitre.org/cgi-bin/cvename.cgi?name=2002-0012][Xref => http://cve.mitre.org/cgi-bin/cvename.cgi?name=1999-0517][Xref => http://www.securityfocus.com/bid/4089][Xref => http://www.securityfocus.com/bid/4088][Xref => http://www.securityfocus.com/bid/2112]" 
sub(".*?\\:[0-9a-z]+\\s+([A-Z0-9]+(\\-[A-Z0-9]+)?).*", "\\1", line)

sub(".*?\\*\\*\\*([^*]+)\\*\\*(\\*)?.*", "\\1", line)
sub(".*?Seq\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
sub(".*?Ack\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
window = sub(".*?Win\\:\\s([0-9]+x[0-9A-Za-z]+).*", "\\1", line)
tcpLen = sub(".*?TcpLen\\:\\s+([0-9]+).*", "\\1", line)

sub(".*?\\:[0-9a-z]+\\s+([A-Z0-9]+\\-[A-Z0-9]+).*", "\\1", line)



# nrow(newDataframe1)
# nrow(newDataframe2)



summary(combinedDataframe)

names(combinedDataframe)
dim(combinedDataframe)

sapply(combinedDataframe, class)
any(is.na(combinedDataframe))


table(grepl("[0-9]+\\:[0-9]+\\:[0-9]+", combinedDataframe$`SnortID`))
table(grepl("[0-9]\\/[0-9]+\\-[0-9]+\\:([0-9]+)?", combinedDataframe$`Date-Time`))


#Verifying if each SourceIP and DestinationIP is in the correct format
table(grepl("[0-9]+\\.[0-9]+\\.[0-9]+\\.([0-9]+)", combinedDataframe$`SourceIP`))
table(grepl("[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+|[0-9a-z]+::[0-9a-z]+:[0-9a-z]+|[0-9a-z]+:", combinedDataframe$`DestinationIP`))
table(grepl("[A-Z]+", combinedDataframe$`Protocol`))
which(!grepl("[A-Z]+", combinedDataframe$`Protocol`), arr.ind=TRUE)
table(grepl("[0-9]+x[0-9A-Z]+", combinedDataframe$`TOS`))



