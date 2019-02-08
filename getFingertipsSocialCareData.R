## This is the code to get the Fingertips indicators and metadata into a form
## that can be easily imported by the Shiny App - it is already in a nice format
## so the only real task is getting the date into date form

library(readr)
library(dplyr)
library(magrittr)
library(lubridate)


socialCareData <- fingertips_data(ProfileID = 8)


# let's take out everything which isn't a local authority - 117k
socialCareData %<>% filter(AreaType == "County & UA")  

# Let#s make the the time period field into an actual Date class with the date
# being the first day of the time period. We need to split it into three types
# of timeperiod - year, quarter and month.

# Let's treat it as numeric for a minute
socialCareData$TimeperiodSortable %<>% as.integer()

# Years first - that's easy, if it has four 0s it is just a year 
justYear <- filter(socialCareData, TimeperiodSortable/1e4 == round(TimeperiodSortable/1e4)) 

        justYear$TimeperiodSortable %>% as.character() %>% substr(1,4) %>% paste0("01/01/", .) %>% dmy() -> justYear$TimePeriodAsDate

# Quarters next - if it has two zeroes then it's a quarter
yearQuarters <- filter(socialCareData, TimeperiodSortable/1e4 != 
                                round(TimeperiodSortable/1e4) & 
                                TimeperiodSortable/1e2 == round(TimeperiodSortable/1e2))

# Everything under this indent is sorting the quarter number as a date 
        # Find the quarter number
        yearQuarters$QuarterNumber <- yearQuarters$TimeperiodSortable %>% as.character() %>% substr(5,6) 
        
        # Change to a string
        yearQuarters$QuarterString <- NA
        yearQuarters$QuarterString[yearQuarters$QuarterNumber=="01"] <- "01/04"
        yearQuarters$QuarterString[yearQuarters$QuarterNumber=="02"] <- "01/07"
        yearQuarters$QuarterString[yearQuarters$QuarterNumber=="03"] <- "01/10"
        yearQuarters$QuarterString[yearQuarters$QuarterNumber=="04"] <- "01/01"
        
        # We need to increment the year if it is Q4
        yearQuarters$QuarterIncrement <- 0
        yearQuarters$QuarterIncrement[yearQuarters$QuarterNumber=="04"] <- 1
        
        # Then extract the year and make it numeric for a moment so we can increment the year for Q4 dates
        yearQuarters$TimeperiodSortable %>% as.character() %>% substr(1,4) %>% as.numeric() -> yearQuarters$TimePeriodAsDate
        
        # Increment year then change it back to a string, append
        yearQuarters$TimePeriodAsDate <- yearQuarters$TimePeriodAsDate + yearQuarters$QuarterIncrement 
        
        yearQuarters$TimePeriodAsDate %<>% as.character() %>% paste0(yearQuarters$QuarterString, .) %>% dmy()
        
        # Delete the quarter calculation columns
        yearQuarters %<>% select(-c(QuarterNumber, QuarterString, QuarterIncrement))

# Now let's deal with the ones which are years and months:
yearMonths <- filter(socialCareData, TimeperiodSortable/1e4 != 
                                             round(TimeperiodSortable/1e4) & 
                                             TimeperiodSortable/1e2 != round(TimeperiodSortable/1e2))
# Let's take out the year
yearMonths$TimeperiodSortable %>% as.character() %>% substr(1,4) -> yearMonths$TimePeriodAsDate

# Let's prepend the month
yearMonths$TimeperiodSortable %>% as.character() %>% substr(7,8) %>% 
                 paste0("01/", ., yearMonths$TimePeriodAsDate) %>% dmy ->yearMonths$TimePeriodAsDate

# ok so let's put it all back together
socialCareData <- rbind(justYear, yearQuarters, yearMonths)

# clear up workspace a bit
rm(justYear, yearQuarters, yearMonths)

# Get metadata
metaData <- indicator_metadata(ProfileID = 8)

# Get unique indicators
uniqueIndicators <- indicators_unique(ProfileID = 8)

# let's save this somewhere
write_rds(x = socialCareData, path = "./Scripts/Data/ASCIndicatorsData.rds")
write_rds(x = metaData, path = "./Scripts/Data/MetadataASC.rds")
write_rds(x = uniqueIndicators, path = "./Scripts/Data/uniqueIndicators.rds")


