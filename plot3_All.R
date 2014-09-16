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


## Plot for 3rd Question
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
##  which of these four sources have seen decreases in emissions from 1999???2008 for Baltimore City? 
##  Which have seen increases in emissions from 1999???2008? 
##  Use the ggplot2 plotting system to make a plot answer this question.

vecYears <- numeric(length(yrsInDataSet))
vecType <- numeric(length(yrsInDataSet))
vecSumForType <-numeric(length(yrsInDataSet))
idx = 1
for(y in yrsInDataSet) {
    if(y %in% yrsOfInterest) {
        emissionsAndType <- subset(NEI, year == y & fips == '24510', select = c('Emissions', 'type'))
        types <- as.character(levels(as.factor(emissionsAndType$type)))
        
        for(t in types) {
            emissions <- subset(emissionsAndType, type == t, select = c('Emissions'))
            
            vecYears[idx] <- y
            vecType[idx] <- t
            vecSumForType[idx] <- sum(emissions)
            idx <- idx + 1
        }
    }    
}

dfSumBaltimoreEmissionsByTypeForYr <- data.frame(vecYears, vecType, vecSumForType) 


library(ggplot2)

png("plot3.png", width=640, height=480)

plot <- qplot(dfSumBaltimoreEmissionsByTypeForYr$vecYears, 
              dfSumBaltimoreEmissionsByTypeForYr$vecSumForType, 
              data = dfSumBaltimoreEmissionsByTypeForYr, 
              color = dfSumBaltimoreEmissionsByTypeForYr$vecType, 
              geom = c("point", "smooth")) +
    geom_smooth() + theme_bw() +
    geom_point(size=6, alpha = I(.5)) +
    labs(x = 'Year') +
    labs(y = expression(PM[2.5] * ' Emissions by type')) +
    labs(title=expression('Cumulative PM'[2.5] * ' Emissions in Baltimore by Type (1999-2008)')) +
    theme(axis.title=element_text(face='italic', size='14', color='forestgreen'), 
          legend.position='top', 
          plot.title=element_text(face='bold', size='20', color='midnightblue'), 
          legend.title=element_blank())

print(plot)

dev.off()

###
### Conclusion - Road, nonroad, nonpoint types have decreased over the period 1999 through 2008, while PM2.5 emissions of type point have increased in the same period for Baltimore City.
###
