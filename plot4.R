
## Conditional sourcing of the loading of the data sets.
##  Assuming that these two files are loaded into NEI and SCC
if(!isTRUE(exists("NEI")) | !isTRUE(exists("SCC")) | !isTRUE(exists("yrsInDataSet")))
    source("loadDataSets.R")


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

### Conclusion - Total emissions from coal combustion sources, across the United States, have reduced overall between 1999-2009. There was a slight increase between 2002 and 2005.
