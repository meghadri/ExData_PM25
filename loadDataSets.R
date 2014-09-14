




remoteFile <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'

source('fetchRemoteFile.R')
##dataFiles <- fetchRemoteFile(remoteFile, 'data/', unzip=TRUE)

NEI <- readRDS('data/summarySCC_PM25.rds')
SCC <- readRDS('data/Source_Classification_Code.rds')

yrsInDataSet <- as.numeric(levels(as.factor(NEI$year)))

yrsOfInterest <- c(1999, 2002, 2005, 2008)