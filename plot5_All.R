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

## Plot for 5th Question
## How have emissions from motor vehicle sources changed from 1999???2008 in Baltimore City? 

##sccVehicles <- SCC[grep('vehicle', SCC$SCC.Level.Two, ignore.case=TRUE),]
##sccVehicles <- sccVehicles[sccVehicles$Data.Category == 'Onroad', ]
sccVehicles <- SCC[SCC$Data.Category == 'Onroad', ]

idx <- 1
vecYears <- numeric(length(yrsInDataSet))
vecSumEmissions <- numeric(length(yrsInDataSet))
for(y in yrsInDataSet) {
    if(y %in% yrsOfInterest) {
        emissions <- subset(NEI, year == y & fips == '24510' & SCC %in% sccVehicles$SCC, select = c('Emissions'))
        vecYears[idx] <- y
        vecSumEmissions[idx] <- sum(emissions)
        idx <- idx + 1
    }
} 


dfBaltimoreVehiclesByYr <-  data.frame(vecYears, vecSumEmissions)

png("plot5.png", width=640, height=480)

plot(dfBaltimoreVehiclesByYr$vecSumEmissions ~dfBaltimoreVehiclesByYr$vecYears, type='p', pch=5, xlab='Year', ylab=expression(PM[2.5] * ' Emissions'), xaxt='n', axes=F, col.lab = 'forestgreen')
abline(lm(dfBaltimoreVehiclesByYr$vecSumEmissions~dfBaltimoreVehiclesByYr$vecYears), col='red', lwd=2)
box()
axis(side=1, at=dfBaltimoreVehiclesByYr$vecYears, labels=dfBaltimoreVehiclesByYr$vecYears)

xticksAt <- seq(from=min(dfBaltimoreVehiclesByYr$vecSumEmissions), to=max(dfBaltimoreVehiclesByYr$vecSumEmissions), by = (max(dfBaltimoreVehiclesByYr$vecSumEmissions) - min(dfBaltimoreVehiclesByYr$vecSumEmissions)) / 3)
axis(side=2, at=xticksAt)

title(main=expression('Annual Cumulative ' * PM[2.5] * ' Emissions from Vehicles in Baltimore'), col.main='midnightblue')

dev.off()

###
### Conclusion - Emissions from motor vehicle sources have decreased in Baltimore City by over 74% while they have increased in Los Angeles County by about 4%.
###
