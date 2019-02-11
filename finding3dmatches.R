# This is the script that finds two indicators with matching reporting frequences
# Which allows them to be compared over time in 3d 

library(readr)
library(dplyr)
library(magrittr)
library(reshape2)

# This is how to get the indicators which have matching dates

currentData <- read_rds("./Scripts/Data/ASCIndicatorsData.rds") # # created in getFingertipsSocialCareData.R

# Get a data frame just of dates
currentData %<>% 
        select(IndicatorID, IndicatorName, AreaCode, AreaName, TimePeriodAsDate, Value) %>% 
        group_by(IndicatorID, IndicatorName, AreaCode, AreaName, TimePeriodAsDate) %>% 
        summarise(TotalValueX = sum(Value, na.rm = TRUE)) %>% # basically just get one value per local authority per reporting period
        filter(AreaCode != "E06000053" & AreaCode != "E09000001") %>% # Take out Sicilly and City of London because they squash everything
        filter(TotalValueX!=0) # If total value is 0 this is because they haven't submitted data

currentData %>% dcast(TimePeriodAsDate ~ IndicatorID, value.var = "IndicatorID", fun.aggregate = length) -> frameOfDates

# Create an empty data frame to test whether rows have matching dates
outputFrame <- data.frame(matrix(ncol = 98, nrow = 0))

# Test whether each indicator has another indicator which has exactly matching
# reporting dates - this will produce a symmetrical matrix
for(i in 2:98){
        
        testIdentical <- sapply(frameOfDates, identical, frameOfDates[[i]])
        outputFrame <- rbind(outputFrame, testIdentical)
        
}

# Give it proper column names and row names
names(outputFrame) <- names(frameOfDates)
names(outputFrame)[1] <- "IndicatorID"
outputFrame$IndicatorID <- names(frameOfDates)[-1]

# Then basically we want to filter it to those indicators which have more than one match
# Because they all match themselves
outputFrame$Sum <- sapply(outputFrame[-1], sum)

# Then filter it to where Sum is greater than 1,  delete the sum column, 
# melt into a new data frame where we will easily be able to write logical tests
# for which indicators match using dplyr::filter
outputFrame %<>% filter(Sum>1) %>% 
        select(-Sum) %>% 
        melt(id="IndicatorID", variable.name = "Match")

# Get a data frame of unique indicators so we can bring the indicator name back in
uniqueIndicators <- read_rds("./Scripts/Data/uniqueIndicators.rds")

# Now we want to get the data frame of indicators that you can look at in 3d as
# a vector they can be listed in the shiny app on startup as 3d x axis
outputFrame %>% select(IndicatorID) %>% distinct() %>% 
        mutate(IndicatorID=as.integer(IndicatorID)) %>% 
        left_join(uniqueIndicators, by = "IndicatorID") -> unique3dIndicators

# Get a list for the first values of y axis - they will be dynamically updated
# by the server after the first time they are loaded
outputFrame %>% filter(IndicatorID==1112 & value==TRUE) %>% 
        select(Match) %>% transmute(IndicatorID=as.integer(as.character(Match))) %>% 
        left_join(uniqueIndicators, by = "IndicatorID") -> first3dYAxisValues



write_rds(outputFrame, "./Scripts/Data/meltedDateMatch.rds")
write_rds(unique3dIndicators, "unique3dIndicators.rds")
write_rds(first3dYAxisValues, "first3dYAxisValues.rds")





# Now we will easily be able to query matches in the format
# outputFrame %>% filter(IndicatorID==1112 & value==TRUE) %>% 
 #       select(Match)

