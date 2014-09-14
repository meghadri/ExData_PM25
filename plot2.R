

## Conditional sourcing of the loading of the data sets.
##  Assuming that these two files are loaded into NEI and SCC
if(!isTRUE(exists("NEI")) | !isTRUE(exists("SCC")) | !isTRUE(exists("yrsInDataSet")))
    source("loadDataSets.R")

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


plot(dfSumBaltimoreEmissionsByYr$vecSumEmissions ~dfSumBaltimoreEmissionsByYr$vecYears, type='p', pch=5, xlab='Year', ylab=expression(PM[2.5] * ' Emissions'), xaxt='n', axes=F, col.lab = 'forestgreen')
abline(lm(dfSumBaltimoreEmissionsByYr$vecSumEmissions~dfSumBaltimoreEmissionsByYr$vecYears), col='red', lwd=2)
box()
axis(side=1, at=dfSumBaltimoreEmissionsByYr$vecYears, labels=dfSumBaltimoreEmissionsByYr$vecYears)

xticksAt <- seq(from=min(dfSumBaltimoreEmissionsByYr$vecSumEmissions), to=max(dfSumBaltimoreEmissionsByYr$vecSumEmissions), by = (max(dfSumBaltimoreEmissionsByYr$vecSumEmissions) - min(dfSumBaltimoreEmissionsByYr$vecSumEmissions)) / 3)
axis(side=2, at=xticksAt)
title(main=expression('Annual Cumulative ' * PM[2.5] * ' Emissions in Baltimore (1999-2008)'), col.main='midnightblue')

##
## Conclusion - Total emission has reduced, but there was an increase from 2002 to 2005.
##