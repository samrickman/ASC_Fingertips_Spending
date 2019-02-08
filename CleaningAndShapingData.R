## Code to Adult Social Care spending per local authority, per primary support group

library(readr)
library(reshape2)
library(fingertipsR)
library(dplyr) # Note if running in an existing session makes sure to unload package plotly or change function calls from "filter" and "select" to "dplyr::filter" and "dplyr::select"
library(magrittr)

# import full data - 500k rows, 47mb of RAM, may take 10 seconds or so
financeData <- read.csv("./Scripts/Data/ASCFR Data with descriptions.csv")

# Change ITEMVALUE from a FACTOR to NUMERIC

financeData$ITEMVALUE %<>% as.character %>% parse_number(na = ":")

grossCurrentExpenditureAllPSRs <- filter(financeData, DATA_LEVEL == "Gross Current Expenditure" & 
                                                GEOGRAPHY_LEVEL == "Local Authority" &
                                                CareType_Key != 99 & 
                                                PrimarySupportReason_Key !=99 &
                                                SupportSetting_Key != 99 &
                                                Purpose_Key != 99 &
                                                CarerSupportType_Key !=99
)

# Take out the 173 (out of 15k) negative values which must be errors as we are looking at 
# gross current expenditure
grossCurrentExpenditureAllPSRs  %<>%  filter(ITEMVALUE >= 0) 

# Make the value pounds rather than thousands
grossCurrentExpenditureAllPSRs$ITEMVALUE <- grossCurrentExpenditureAllPSRs$ITEMVALUE * 1000

# NHS Digital have explained that the SupportSetting_Key field only applies
# to long term care, and is blank if the care type is "short term care" or other.
# So essentially I want to create a new field where either it contains SupportSetting_Key
# or if that is blank, it contains CareType_Key
grossCurrentExpenditureAllPSRs$supportOrCareType <- ifelse(grossCurrentExpenditureAllPSRs$SupportSetting_Key != "", 
                                                           as.character(grossCurrentExpenditureAllPSRs$SupportSetting_Key),
                                                           as.character(grossCurrentExpenditureAllPSRs$CareType_Key)
)


# Now I want to create a data frame where every local authority has a total
# spending per primary support group which can then be divided by the number of
# people in those groups in each local authority

# I am only interestd in doing this for the first four main PSR types (Mental 
# health, physical support, support with memory and cognition, learning
# disability support) which are a) broken down by support setting and b) you can
# get estimates for number of people in each group

relevantPSRs <- unique(grossCurrentExpenditureAllPSRs$PrimarySupportReason_Key)[1:5][-4]

grossCurrentExpenditureAllPSRs %>% filter(PrimarySupportReason_Key %in% relevantPSRs) %>% 
        group_by(DH_GEOGRAPHY_NAME, GEOGRAPHY_CODE, PrimarySupportReason_Key) %>% 
        summarise(totalItemValue = sum(ITEMVALUE)) %>% 
        mutate(PrimarySupportReason_Key = as.character(PrimarySupportReason_Key)) %>% #remove factor variables   
        rename("AreaCode" = GEOGRAPHY_CODE) -> # change column name for later joining 
        spendingByPrimarySupportReason

# Now I want to get the table of the relevant indicators using the FingertipsR package functions

# Three are straightforward:

# 1164	Adults with physical disabilities supported throughout the year per 100,000
# 1165	Adults with learning disabilities supported throughout the year per 100,000
# 1166	Adults with mental health problems supported throughout the year per 100,000

# Memory and cognition will use: 

# 247	Dementia: QOF prevalence (all ages)

relevantIndicatorsVector <- c(247, 1164, 1165, 1166)

groupPopulationData <- fingertips_data(IndicatorID = relevantIndicatorsVector)

# Find most recent year for each indicator
mostRecent <- tapply(groupPopulationData$TimeperiodSortable, groupPopulationData$IndicatorID, max)

# Filter the data so we only have most recent year for each indicator
groupPopMostRecent <- NULL
for(i in 1:length(mostRecent)) {
        groupPopMostRecent <- rbind(groupPopMostRecent, 
                                    filter(groupPopulationData, IndicatorID == rownames(mostRecent)[i] & TimeperiodSortable == mostRecent[i])
                                    )
}

# Let's just take the columns we want and make it simpler
groupPopMostRecent %<>% select(IndicatorID, IndicatorName, AreaCode, AreaName, Value) 

# More wrangling - let's rename the indicators in the main table so they can be easily referred to by name as well as number later on

groupPopMostRecent$IndicatorName[groupPopMostRecent$IndicatorID==247] <- "Support with Memory and Cognition"
groupPopMostRecent$IndicatorName[groupPopMostRecent$IndicatorID==1164] <- "Physical Support"
groupPopMostRecent$IndicatorName[groupPopMostRecent$IndicatorID==1165] <- "Learning Disability Support"
groupPopMostRecent$IndicatorName[groupPopMostRecent$IndicatorID==1166] <- "Mental Health Support"

# We should rename IndicatorName now because it's not correct anymore 
names(groupPopMostRecent)[2] <- "PrimarySupportReason_Key"


# then we need population estimates in the spendPerCouncil data

# Get population estimates


fileUrl <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/2011censuspopulationestimatesbyfiveyearagebandsandhouseholdestimatesforlocalauthoritiesintheunitedkingdom/r12ukrttablep04ukv2_tcm77-304141.xls"
destfile = "./Scripts/Data/ONSPopulationData.xls"

if(!file.exists(destfile)) download.file(fileUrl, destfile = destfile)

# ONS data unfortunately in XLS format - so need to read it in with xlsx package
library(xlsx)
UKpopulation <- read.xlsx(destfile, sheetIndex = 2)

# this data is in a horrible format and will need to be wrangled quite a bit

# first name the columns
names(UKpopulation) <- c("AreaCode", "ParentName", "AreaNameWithHalfData", "AnotherNameColumn", "EmptyColumn", "UsualResidents", "Hectares", "Density", "EmptyColumn2", "EmptyColumn3", "EmptyColumn4")

# then do the rest of the wrangling:
UKpopulation %<>% filter(between(row_number(), 16,n())) %>% # delete comment rows
                                select(1,3,4,6) %>% # keep only area code, area name and usual residents
                                mutate(AreaName = ifelse(is.na(AreaNameWithHalfData), # move area names into one column
                                                 as.character(AnotherNameColumn), as.character(AreaNameWithHalfData))) %>%  
                                mutate(AreaName = as.character(AreaName), AreaCode = as.character(AreaCode), 
                                       UsualResidents = parse_number(as.character(UsualResidents))) %>% # get rid of factor variables
                                select(-(2:3)) %>% # delete old name columns
                                filter(!is.na(AreaName)) #-> # anything left with NA is a comment row                         
                                                  

# Manually change the Gateshead and
# Northumberland codes because they do not match across the data
UKpopulation$AreaCode[UKpopulation$AreaName=="Gateshead"] <- "E08000037"
UKpopulation$AreaCode[UKpopulation$AreaName=="Northumberland UA 5"] <- "E06000057"

# Now let's join them

supportGroupsData <- left_join(groupPopMostRecent, UKpopulation, by = "AreaCode")

# Any that did not join are not local authorities (i.e. they area parent areas,
# East of England etc) and can be deleted
supportGroupsData %<>% filter(!is.na(UsualResidents)) 

# Clear up workspace a little
rm(groupPopulationData, UKpopulation, destfile, fileUrl, i, mostRecent, relevantIndicatorsVector)

# Now we want to join this data with the ASC data according to two factors - AreaCode and PrimarySupportReason

fullData <- left_join(spendingByPrimarySupportReason, supportGroupsData, by = c("PrimarySupportReason_Key", "AreaCode"))

# There is one NA - the missing Buckinghamshire Dementia QOF figure - let's just delete it for now
# and remove the duplicated area name columns
fullData %<>% filter(!is.na(UsualResidents)) %>% select(-AreaName.x, -AreaName.y)

# Now let's work out the actual population of people in each group - dementia is
# a percent and the rest are per 100,000 so let's make dementia per 100k

fullData %<>% filter() %>% 
        mutate(Value = ifelse(PrimarySupportReason_Key == "Support with Memory and Cognition", Value*1e3, Value) )

# Then we can multiply them by population
fullData %<>% mutate(ActualGroupPopulation = Value *UsualResidents/1e5) 

# Let's work out spendingPer100k (raw, unweighted) and spending per person per group
fullData %<>% mutate(SpendingPer100k = totalItemValue/UsualResidents*1e5, SpendingPerPerson = totalItemValue/ActualGroupPopulation)

# And let's also take an average across groups - weighted and unweighted
fullData %<>% group_by(DH_GEOGRAPHY_NAME, AreaCode) %>% 
        summarise(
                        SpendingPer100k = mean(SpendingPer100k), # raw spending per 100k pop
                        SpendingPerPerson = sum(SpendingPerPerson * ActualGroupPopulation)/ sum(ActualGroupPopulation), #weighted spending per person with condition
                        Value = mean(Value),
                        UsualResidents = mean(UsualResidents),
                        totalItemValue = sum(totalItemValue),
                        ActualGroupPopulation = sum(ActualGroupPopulation)
                  ) %>% mutate(PrimarySupportReason_Key = "Average") %>% 
                        rename("GroupPopulationPer100kResidents" = Value) %>% 
                bind_rows(fullData)
        

write_rds(x = fullData, path = "./Scripts/Data/mergedDataPerGroup.rds")
rm(list=ls())

