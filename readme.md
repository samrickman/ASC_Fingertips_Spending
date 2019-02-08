## What is this repo?

This is the repo for the [Adult Social Care Spending and Fingertips Outcomes Explorer](https://samrickman.shinyapps.io/asc_fingertips_spending/), which is my first attempt to bring together two data sources:

1. [Adult Social Care Activity and Finance Report]("https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-activity-and-finance-report/2017-18").
2. [Public Health England Fingertips Data]("https://fingertips.phe.org.uk/")

I previously created an [R Shiny Dashboard](https://samrickman.shinyapps.io/ascfr_dashboard/) look at the Adult Social Care Finance report alone which led me to three main goals:

1. Create a weighted spending per person figure. The ASCFR spending data contains how much is spent per support group, per local authority. However, as there are, for example, far more people with dementia than with learning disabilities, even if you divide the raw figure by the local authority population you cannot compare between groups.

2. Create interative plots, ideally in 3d. The [Dashboard](https://samrickman.shinyapps.io/ascfr_dashboard/) used R package ggplot2, which is great for publication plots but the plots were not interactive. Ggplot2 plots can be made interactive but with large amounts of data they are unacceptably slow. This is my first attempt using R's plotly package. 

3. I wanted to use the PHE FingertipsR package, which I have just discovered.

## How to use this

The first thing I would recommend is to play around with the drop-down menus and see what changes!

I find the 3d plots easier to understand if they are viewed in 2d first. I was interested in creating them but I am not sure how much they actually add.

The data sets for each plot can be downloaded using the download button on each respective page. All data in the entire dataset is in the [Github repo](), including scripts for how the data was generated from raw data.

## Are there caveats?

Yes! Lots. **I hacked this together in a few evenings to try out a couple of new packages and data sources. It is interesting but it is not a serious piece of research.** 

In particular there are a number of areas for future improvement:

1. In order to calculate spending per person, I had to:
    -   Look up population - I used ONS 2011 census data. There are more recent estimates.
    -   Calculate spending - I only used one Fingertips indicator for each group as a proxy. There are a number of indicators relating to each support group - with a bit more time I would try to combine several indicators into a per-estimate group.
2. Primary Support Reason is recorded by local authorities and has drawbacks. Notably:
    -   It is not necessarily always recorded accurately - need arising from physical support appear to be over-represented, probably caused by older people initially requiring physical support when contacting a local authority, and this not being updated on local authority systems as people acquire more needs.
    -    Primary support reason may not be the most useful construct. Many people are in more than one category and it is difficult to say which need is "primary".
3. I put in a linear regression line with one coefficient to give a flavour of relationship between two variables. Clearly any proper analysis would want to look at multiple coefficients and interactions, and there will be indicators where other regression models (e.g. Poisson) are more appropriate.
4. A correlation between two variables is only an indicator that they are related to each other or a third variable, or both. It is quite possible that as a third variable is introduced, the degree or direction of the relationship will change.
5. The 3d plot only works right now two indicators have the exact same reporting dates. This could be updated - it would require essentially collapsing dates close to one another into one row. I am not sure whether it is worth doing this - the 3d time series plot is not as visually helpful as I hoped.

## What are the files in the github repo?

They are:

1. App.R. The application.
2. getFingertipsSocialCareData.R - this is the script that downloads the Fingertips Adult Social Care data indicators and formats the Time Period as a Date.
3. Finding3dmatches.R - this is the script to find indicators with matching reporting dates to be made into a 3d time series plot.
4. CleaningAndShapingData.R - this is the code to get the Adult Social Care Finance Report Data into reasonable shape. This includes downloading the ONS data and the Fingertips support group indicators, which are used to get a per person spending.
5. Include.md - the about page that is imported into the About tab. 
6. Readme.md - the readme file for the repo.
6. Data directory.

## Note on reproducibility 

The code in the github repository works on the date as downloaded in the week commencing 4th February 2019. 

All data is downloaded in the code either with download.file() or the FingertipsR functions. Source files have been known to change format at times. If you plan to run the code, I would strongly recommend using the data files in the repository, rather than downloading them again from source.

The one exception is the “ASCFR Data with descriptions.csv”, which (following some difficulty importing a .xlsb file into R on Linux)  was created by opening the “ASCFR Data File (with descriptions).xlsb file” in LibreOffice5.1 and saving as .csv with UTF-8 encoding, comma as field delimiter and double quotes as text delimiter. 

## About

This dashboard was created by Sam Rickman using R version 3.5.2 (2018-12-20) on an x86, 64 bit Linux-GNU PC. 

All code is open source and can be viewed in the [Shiny Care Github Repository](https://github.com/samrickman/)

