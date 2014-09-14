

## Conditional sourcing of the loading of the data sets.
##  Assuming that these two files are loaded into NEI and SCC
if(!isTRUE(exists("NEI")) | !isTRUE(exists("SCC")) | !isTRUE(exists("yrsInDataSet")))
    source("loadDataSets.R")

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


plot(dfSumEmissionsByYr$vecSumEmissions ~dfSumEmissionsByYr$vecYears, type='p', pch=5, xlab='Year', ylab=expression(PM[2.5] * ' Emissions'), xaxt='n', axes=F, , col.lab = 'forestgreen')
abline(lm(dfSumEmissionsByYr$vecSumEmissions~dfSumEmissionsByYr$vecYears), col='red', lwd=2)
box()
axis(side=1, at=dfSumEmissionsByYr$vecYears, labels=dfSumEmissionsByYr$vecYears)

xticksAt <- seq(from=min(dfSumEmissionsByYr$vecSumEmissions), to=max(dfSumEmissionsByYr$vecSumEmissions), by = (max(dfSumEmissionsByYr$vecSumEmissions) - min(dfSumEmissionsByYr$vecSumEmissions)) / 3)
axis(side=2, at=xticksAt)

##
## Conclusion - Total emission has reduced between 1999 and 2008. There is a precipitous drop between 1999 and 2002, and again between 2005 and 2008
##
title(main=expression('Annual Cumulative ' * PM[2.5] * ' Emissions in the United States'), col.main='midnightblue')
