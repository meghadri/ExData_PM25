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



## Plot for 4th Question
## Across the United States, how have emissions from coal combustion-related sources changed from 1999???2008?


## Subset on 'coal' in the EI.Sector and 'combustion' in the Level.One fields
sccCoalCombust <- SCC[grep('coal', SCC$EI.Sector, ignore.case=T),]
sccCoalCombust <- sccCoalCombust[grep('combustion', sccCoalCombust$SCC.Level.One, ignore.case=T),]

idx <- 1
vecYears <- numeric(length(yrsInDataSet))
vecSumEmissions <- numeric(length(yrsInDataSet))
for(y in yrsInDataSet) {
    if(y %in% yrsOfInterest) {
        emissions <- subset(NEI, year == y & SCC %in% sccCoalCombust$SCC, select = c('Emissions'))
        vecYears[idx] <- y
        vecSumEmissions[idx] <- sum(emissions)
        idx <- idx + 1
    }
} 

dfCoalCombustByYr <-  data.frame(vecYears, vecSumEmissions)

png("plot4.png", width=640, height=480)

plot(dfCoalCombustByYr$vecSumEmissions ~dfCoalCombustByYr$vecYears, type='p', pch=5, xlab='Year', ylab=expression(PM[2.5] * ' Emissions'), xaxt='n', axes=F, col.lab = 'forestgreen')
abline(lm(dfCoalCombustByYr$vecSumEmissions~dfCoalCombustByYr$vecYears), col='red', lwd=2)
box()
axis(side=1, at=dfCoalCombustByYr$vecYears, labels=dfCoalCombustByYr$vecYears)

xticksAt <- seq(from=min(dfCoalCombustByYr$vecSumEmissions), to=max(dfCoalCombustByYr$vecSumEmissions), by = (max(dfCoalCombustByYr$vecSumEmissions) - min(dfCoalCombustByYr$vecSumEmissions)) / 3)
axis(side=2, at=xticksAt)

title(main=expression('Annual Cumulative ' * PM[2.5] * ' Emissions from Coal Combustion in the United States'), col.main='midnightblue')

dev.off()

###
### Conclusion - Total emissions from coal combustion sources, across the United States, have reduced overall between 1999-2009. There was a slight increase between 2002 and 2005.
###
