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


## Plot for 6th Question
## Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle 
##  sources in Los Angeles County, California (fips == "06037"). 
##  Which city has seen greater changes over time in motor vehicle emissions?

##sccVehicles <- SCC[grep('vehicle', SCC$SCC.Level.Two, ignore.case=TRUE),]
##sccVehicles <- sccVehicles[sccVehicles$Data.Category == 'Onroad', ]
sccVehicles <- SCC[SCC$Data.Category == 'Onroad', ]

idx <- 1
vecYears <- numeric(length(yrsInDataSet) * 2)
vecSumEmissions <- numeric(length(yrsInDataSet) * 2)
vecCity <- numeric(length(yrsInDataSet) * 2)

for(y in yrsInDataSet) {
    if(y %in% yrsOfInterest) {
        emissions <- subset(NEI, year == y & fips == '24510' & SCC %in% sccVehicles$SCC, select = c('Emissions'))
        vecYears[idx] <- y
        vecSumEmissions[idx] <- sum(emissions)
        vecCity[idx] <- 'Baltimore, MD'
        idx <- idx + 1
        
        emissions <- subset(NEI, year == y & fips == '06037' & SCC %in% sccVehicles$SCC, select = c('Emissions'))
        vecYears[idx] <- y
        vecSumEmissions[idx] <- sum(emissions)
        vecCity[idx] <- 'Los Angeles County, CA'
        idx <- idx + 1
    }
} 

dfVehiclesByYr <-  data.frame(vecYears, vecSumEmissions, vecCity)
library(ggplot2)


emBalt1999 <- dfVehiclesByYr[dfVehiclesByYr$vecYears == 1999 & dfVehiclesByYr$vecCity == 'Baltimore, MD', ]$vecSumEmissions
emBalt2008 <- dfVehiclesByYr[dfVehiclesByYr$vecYears == 2008 & dfVehiclesByYr$vecCity == 'Baltimore, MD', ]$vecSumEmissions
emLA1999 <- dfVehiclesByYr[dfVehiclesByYr$vecYears == 1999 & dfVehiclesByYr$vecCity == 'Los Angeles County, CA', ]$vecSumEmissions
emLA2008 <- dfVehiclesByYr[dfVehiclesByYr$vecYears == 2008 & dfVehiclesByYr$vecCity == 'Los Angeles County, CA', ]$vecSumEmissions

emChngBalt <- round(((emBalt2008 - emBalt1999)/emBalt1999) * 100, digits=2)
emChngLA <- round(((emLA2008 - emLA1999)/emLA1999) * 100, digits=2)


lblLegendBaltimore <- paste("Baltimore, MD (Change: ", emChngBalt, "%)")
lblLegendLA <- paste("Los Angeles County, CA (Change: ", emChngLA, "%)")

png("plot6.png", width=640, height=480)

plot <- qplot(dfVehiclesByYr$vecYears, 
              dfVehiclesByYr$vecSumEmissions, 
              data = dfVehiclesByYr, 
              ##              color = dfVehiclesByYr$vecCity, 
              color = factor(dfVehiclesByYr$vecCity, labels = c(lblLegendBaltimore, lblLegendLA)),
              geom = c("point", "smooth")) +
    geom_smooth() + theme_bw() +
    geom_point(size=6, alpha = I(.5)) +
    labs(x = 'Year') +
    labs(y = expression(PM[2.5] * ' Emissions')) +
    labs(title=expression('Cumulative PM'[2.5] * ' Vehicular Emissions in Baltimore and Los Angeles County')) +
    theme(axis.title=element_text(face='italic', size='14', color='forestgreen'), 
          legend.position='top', 
          plot.title=element_text(face='bold', size='20', color='midnightblue'), 
          legend.title=element_blank())


print(plot)

dev.off()


###
### Los Angeles County has seen an increate in cumulative PM2.5 vehicular emissions over the time period (about 4%) while Baltimore City has seen a decrease of just over 74%
###
