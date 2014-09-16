## Helper function to download remote file to the specified destDir, unzip (if specified) and uses localFName if present, else derives the local name from the
##   name of the downloaded file (uses the last segment in the URL path).
fetchRemoteFile <- function(locRemote, dirDest, unzip = FALSE, localFName = NULL) {
    require(tools)
    
    if(!file.exists(dirDest))    dir.create(dirDest)
    
    if(is.null(localFName) || is.na(localFName) || nchar(localFName) == 0 ) {
        fileExt <- file_ext(basename(URLdecode(locRemote)))
        destFile <- file_path_sans_ext(basename(URLdecode(locRemote)))
        destFile <- paste(dirDest, .Platform$file.sep, destFile, "_", as.Date(Sys.time()), ".", fileExt, sep="")
    } else {
        destFile = localFName
    }
    
    codeSucc <- download.file(locRemote, destFile, method = "curl")
    if(codeSucc != 0)	stop(paste("Unable to download file from :", locRemote))
    
    retVal = destFile
    if(isTRUE(unzip))	retVal <- unzip(destFile, exdir=dirDest)
    
    retVal
    
}

remoteFile <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
source('fetchRemoteFile.R')
## Download the data file zip and unzip to a subfolder 'data' of the working directory
dataFiles <- fetchRemoteFile(remoteFile, 'data/', unzip=TRUE)

## Read the data
NEI <- readRDS('data/summarySCC_PM25.rds')
SCC <- readRDS('data/Source_Classification_Code.rds')
yrsInDataSet <- as.numeric(levels(as.factor(NEI$year)))
yrsOfInterest <- c(1999, 2002, 2005, 2008)

## Plot for 2nd Question
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
##  Use the base plotting system to make a plot answering this question.

## Get the total annual emissions for the years of interest for Baltimore

vecYears <- numeric(length(yrsInDataSet))
vecSumEmissionsBaltimore <- numeric(length(yrsInDataSet))
idx = 1
for(y in yrsInDataSet) {
    if(y %in% yrsOfInterest) {
        emissions <- subset(NEI, year == y & fips == '24510', select = c('Emissions'))
        ##v <-  c(as.character(y), sum(emissions))
        vecYears[idx] <- y
        vecSumEmissionsBaltimore[idx] <- sum(emissions)
        idx <- idx + 1
    }
}

dfSumBaltimoreEmissionsByYr <- data.frame(vecYears, vecSumEmissionsBaltimore) 

png("plot2.png", width=640, height=480)

plot(dfSumBaltimoreEmissionsByYr$vecSumEmissions ~dfSumBaltimoreEmissionsByYr$vecYears, type='p', pch=5, xlab='Year', ylab=expression(PM[2.5] * ' Emissions'), xaxt='n', axes=F, col.lab = 'forestgreen')
abline(lm(dfSumBaltimoreEmissionsByYr$vecSumEmissions~dfSumBaltimoreEmissionsByYr$vecYears), col='red', lwd=2)
box()
axis(side=1, at=dfSumBaltimoreEmissionsByYr$vecYears, labels=dfSumBaltimoreEmissionsByYr$vecYears)

xticksAt <- seq(from=min(dfSumBaltimoreEmissionsByYr$vecSumEmissions), to=max(dfSumBaltimoreEmissionsByYr$vecSumEmissions), by = (max(dfSumBaltimoreEmissionsByYr$vecSumEmissions) - min(dfSumBaltimoreEmissionsByYr$vecSumEmissions)) / 3)
axis(side=2, at=xticksAt)
title(main=expression('Annual Cumulative ' * PM[2.5] * ' Emissions in Baltimore (1999-2008)'), col.main='midnightblue')

dev.off()

##
## Conclusion - Total emission has reduced, but there was an increase from 2002 to 2005.
##