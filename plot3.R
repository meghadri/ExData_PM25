
## Conditional sourcing of the loading of the data sets.
##  Assuming that these two files are loaded into NEI and SCC
if(!isTRUE(exists("NEI")) | !isTRUE(exists("SCC")) | !isTRUE(exists("yrsInDataSet")))
    source("loadDataSets.R")

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

dev.off();