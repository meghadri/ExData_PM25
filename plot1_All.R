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
##dataFiles <- fetchRemoteFile(remoteFile, 'data/', unzip=TRUE)

## Read the data
##NEI <- readRDS('data/summarySCC_PM25.rds')
##SCC <- readRDS('data/Source_Classification_Code.rds')
##yrsInDataSet <- as.numeric(levels(as.factor(NEI$year)))
##yrsOfInterest <- c(1999, 2002, 2005, 2008)


## Plot for 1st Question
## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##  Using the base plotting system, make a plot showing the total PM2.5 emission from 
##  all sources for each of the years 1999, 2002, 2005, and 2008.


## Get the total annual emissions for the years of interest

vecYears <- numeric(length(yrsInDataSet))
vecSumEmissions <- numeric(length(yrsInDataSet))
idx = 1
for(y in yrsInDataSet) {
    if(y %in% yrsOfInterest) {
        emissions <- subset(NEI, year == y, select = c('Emissions'))
        vecYears[idx] <- y
        vecSumEmissions[idx] <- sum(emissions)
        idx <- idx + 1
    }
}

dfSumEmissionsByYr <- data.frame(vecYears, vecSumEmissions) 

png("plot1.png", width=640, height=480)
plot(dfSumEmissionsByYr$vecSumEmissions ~dfSumEmissionsByYr$vecYears, type='p', pch=5, xlab='Year', ylab=expression(PM[2.5] * ' Emissions'), xaxt='n', axes=F, , col.lab = 'forestgreen')
abline(lm(dfSumEmissionsByYr$vecSumEmissions~dfSumEmissionsByYr$vecYears), col='red', lwd=2)
box()
axis(side=1, at=dfSumEmissionsByYr$vecYears, labels=dfSumEmissionsByYr$vecYears)

xticksAt <- seq(from=min(dfSumEmissionsByYr$vecSumEmissions), to=max(dfSumEmissionsByYr$vecSumEmissions), by = (max(dfSumEmissionsByYr$vecSumEmissions) - min(dfSumEmissionsByYr$vecSumEmissions)) / 3)
axis(side=2, at=xticksAt)

title(main=expression('Annual Cumulative ' * PM[2.5] * ' Emissions in the United States'), col.main='midnightblue')

dev.off()

##
## Conclusion - Total emission has reduced between 1999 and 2008. There is a precipitous drop between 1999 and 2002, and again between 2005 and 2008
##

