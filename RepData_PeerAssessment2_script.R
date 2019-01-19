# Set work directory
setwd("C:/Users/Maurits/test-repo/RepData_PeerAssessment2")

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
str(rawData)

