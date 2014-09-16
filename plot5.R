
## Conditional sourcing of the loading of the data sets.
##  Assuming that these two files are loaded into NEI and SCC
if(!isTRUE(exists("NEI")) | !isTRUE(exists("SCC")) | !isTRUE(exists("yrsInDataSet")))
    source("loadDataSets.R")

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

