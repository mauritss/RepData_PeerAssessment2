# Set work directory
setwd("C:/Users/Maurits/test-repo/RepData_PeerAssessment2")

# Load packages needed for analysis
library(dplyr); library(lubridate); library(data.table); library(stringdist)

# Create data folder
if (!file.exists("data")) {
    dir.create("data")
}

# Downloading Raw Data (cache)
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              "./data/rawData.bz2")

# Date of downloaded data:
dataDownloaded <- date()
print(dataDownloaded)

# Reading the file (cache)
rawData = read.csv("./data/rawData.bz2", sep = ",", na.strings = NA)


# General tidying of data (variable names, data type)
colnames(rawData) = tolower(colnames(rawData))
colnames(rawData) = colnames(rawData) %>% gsub("_", "", .)
names(rawData)[7] = "state2" ## rename double var name
names(rawData)[35] = "longitude2"## rename double var name
rawData$bgndate = as.Date(rawData$bgndate, format = "%m/%d/%Y")
#rawData$enddate = as.Date(rawData$enddate, format = "%m/%d/%Y")

# Subsetting data and change to data table (for more analysis speed) 
## 1. Since our objective is comparing the effects of different weather events.
## Before Jan 1996 only Tornado type was recorded. Hence I have decided to 
## drop all records before this time.
sub1Data = rawData %>%
    filter(bgndate >= ("1996-01-01"))
#rm(rawData)

## 2. I have decided to not take several variables (time, states) in to account 
## based on the questions asked. We only need to provide general information 
## about the US as a whole and different weather events.
sub1Data = sub1Data %>%
    select(evtype, f, mag, fatalities, injuries, propdmg, propdmgexp, cropdmg,
           cropdmgexp, wfo, stateoffic, refnum)

## 3. For more analysis speed change data frame to data table.
sub1Data = data.table(sub1Data)

# Cleaning all variables left in de subsetted data set
sub1Data$evtype = tolower(sub1Data$evtype)


# EVTYPE
## Created a list of the event types based on the documentation files
evtype_cat = read.table("./data/evtype_categories.txt", sep = ",", header = T)
## Match the evtype character string based on osa method with max distance 4.
## Checked the results with different methods and distance to get optimum 
## result. Because of time chose to drop (subset) all NA's.
matchlist = amatch(sub1Data$evtype, evtype_cat$name, nomatch = NA, 
                   method = "osa", maxDist = 4)
sub1Data$evtypecat = matchlist
sub2Data = sub1Data[matchlist > 0]
## merge sub2Data with evtype_cat to create a 48 event variable
sub2Data = merge(sub2Data, evtype_cat, by.x = "evtypecat", by.y = "nr")
rm(sub1Data, matchlist, evtype_cat)

# PROPDMG(EXP) & CROPDMG(EXP)
sub2Data$propdmgexp = tolower(sub2Data$propdmgexp)
sub2Data$cropdmgexp = tolower(sub2Data$cropdmgexp)
zero = c("-","?","+","0", "")
## PROPDMG
sub2Data$propdmgexp2 <- ifelse(sub2Data$propdmgexp %in% zero, NA, sub2Data$propdmgexp)
sub2Data$propdmgexp2 <- ifelse(sub2Data$propdmgexp == "b", 1000000000, sub2Data$propdmgexp2)
sub2Data$propdmgexp2 <- ifelse(sub2Data$propdmgexp == "m", 1000000, sub2Data$propdmgexp2)
sub2Data$propdmgexp2 <- ifelse(sub2Data$propdmgexp == "k", 1000, sub2Data$propdmgexp2)
sub2Data$propdmgexp2 <- as.numeric(sub2Data$propdmgexp2)
sub2Data$totpropdmg <- sub2Data$propdmg * sub2Data$propdmgexp2
## CROPDMG
sub2Data$cropdmgexp2 <- ifelse(sub2Data$cropdmgexp %in% zero, NA, sub2Data$cropdmgexp)
sub2Data$cropdmgexp2 <- ifelse(sub2Data$cropdmgexp == "b", 1000000000, sub2Data$cropdmgexp2)
sub2Data$cropdmgexp2 <- ifelse(sub2Data$cropdmgexp == "m", 1000000, sub2Data$cropdmgexp2)
sub2Data$cropdmgexp2 <- ifelse(sub2Data$cropdmgexp == "k", 1000, sub2Data$cropdmgexp2)
sub2Data$cropdmgexp2 <- as.numeric(sub2Data$cropdmgexp2)
sub2Data$totcropdmg <- sub2Data$cropdmg * sub2Data$cropdmgexp2
## Dropping variables we don't need anymore (prop, crop & ev)
sub2Data <- subset(sub2Data, select = c(-propdmg, -propdmgexp, -propdmgexp2,
                                        -cropdmg, -cropdmgexp, -cropdmgexp2,
                                        -evtypecat, -evtype))

