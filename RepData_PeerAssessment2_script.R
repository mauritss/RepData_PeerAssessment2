# Set work directory
setwd("C:/Users/Maurits/test-repo/RepData_PeerAssessment2")

# Load packages needed for analysis
library(dplyr); library(lubridate); library(data.table)

# Create data folder
if (!file.exists("data")) {
    dir.create("data")
}

# Downloading Raw Data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              "./data/rawData.bz2")

# Date of downloaded data:
dataDownloaded <- date()
print(dataDownloaded)

# Reading the file
rawData = read.csv("./data/rawData.bz2", sep = ",", na.strings = NA)

# General tidying of data (variable names, data type)
colnames(rawData) = tolower(colnames(rawData))
colnames(rawData) = colnames(rawData) %>% gsub("_", "", .)
rawData$bgndate = as.Date(rawData$bgndate, format = "%m/%d/%Y")
rawData$countyname = as.character(rawData$countyname)
rawData$bgnlocati = as.character(rawData$bgnlocati)
rawData$enddate = as.Date(rawData$enddate, format = "%m/%d/%Y")
rawData$endlocati = as.character(rawData$endlocati)
rawData$zonenames = as.character(rawData$zonenames)
rawData$remarks = as.character(rawData$remarks)
names(rawData)[7] = "state2" ## rename double var name
names(rawData)[35] = "longitude2"## rename double var name

# Subsetting data and change to data table (for more analysis speed) 
## 1. Since our objective is comparing the effects of different weather events.
## Before Jan 1996 only Tornado type was recorded. Hence I have decided to 
## drop all records before this time.
sub1Data = rawData %>%
    filter(bgndate >= ("1996-01-01"))
rm(rawData)
## 2. I have decided to not take several variables (time, states) in to account 
## based on the questions asked. We only need to provide general information 
## about the US as a whole and different weather events.
sub1Data = sub1Data %>%
    select(state, bgndate, county, state2, evtype, bgnrange, bgnazi, enddate,
           endrange, endazi, length, width, f, mag, fatalities, injuries,
           propdmg, propdmgexp, cropdmg, cropdmgexp, wfo, stateoffic, refnum)
## 3. For more analysis speed change data frame to data table.
sub1Data = data.table(sub1Data)


