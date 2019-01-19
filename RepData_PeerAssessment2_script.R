# Set work directory
setwd("C:/Users/Maurits/test-repo/RepData_PeerAssessment2")

# Load packages needed for analysis
library(dplyr); library(lubridate)

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

# Tidying data (variable names, data type)
colnames(rawData) = tolower(colnames(rawData))
colnames(rawData) = colnames(rawData) %>% gsub("_", "", .)
rawData$bgndate = as.Date(rawData$bgndate, format = "%m/%d/%Y")
rawData$countyname = as.character(rawData$countyname)
rawData$bgnlocati = as.character(rawData$bgnlocati)
rawData$enddate = as.Date(rawData$enddate, format = "%m/%d/%Y")
rawData$endlocati = as.character(rawData$endlocati)
rawData$zonenames = as.character(rawData$zonenames)
rawData$remarks = as.character(rawData$remarks)


