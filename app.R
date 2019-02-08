library(shiny)
library(readr)
library(dplyr)
library(magrittr)
library(plotly) 
library(ggplot2)
library(scales)
library(lubridate)
library(rmarkdown)


# Load the spending data
spendingData <- read_rds("./Data/mergedDataPerGroup.rds")

# Load the indicators for the spending data to be read against
indicatorsSocialCare <- read_rds("./Data/ASCIndicatorsData.rds")

# Load the metadata
metaData <- read_rds("./Data/MetadataASC.rds")

# Load the list of unique ASC indicators
uniqueIndicators <- read_rds("./Data/uniqueIndicators.rds")

# Load the files for the 3d indicators
indicators3dData <- read_rds("./Data/meltedDateMatch.rds")
xAxis3dFrame <- read_rds("./Data/unique3dIndicators.rds")
yAxis3dFrame <- read_rds("./Data/first3dYAxisValues.rds")


# Create the drop-down menu choices


# Spending data
primarySupportReasons <- c("Support with Memory and Cognition", "Learning Disability Support", "Mental Health Support", "Physical Support", "Weighted average" = "Average")

# Indicators 2d data 
uniqueInds <- setNames(uniqueIndicators$IndicatorID, uniqueIndicators$IndicatorName)

# Indicators 3d data
xAxis3dChoices <- setNames(xAxis3dFrame$IndicatorID, xAxis3dFrame$IndicatorName)
yAxis3dChoices <- setNames(yAxis3dFrame$IndicatorID, yAxis3dFrame$IndicatorName)

# Define objects in global environment which will take the data if user wishes to download as csv
csv3dOutput <- data.frame()
csvSpendingVsOutcomes <- data.frame()

ui <- fluidPage(
        
        # Application title
        title = "Adult Social Care Spending Data Fingertips Comparison",
        
        titlePanel("Adult Social Care Spending and Outcomes"),
        

        
        # Main panel for displaying outputs ----
        mainPanel(
                # Output: Tabset w/ plot, summary, and table ----
                tabsetPanel(type = "tabs",
                            
                                                        
                            
                            # 2d plot
                            tabPanel("Plot spending vs outcomes",
                                     # Sidebar panel for inputs ----
                                     sidebarPanel(
                                             # Input: Select the y axis
                                             selectInput(inputId = "yAxisChoice2d", label = "Choose y-axis", choices = uniqueInds, selected = 1139),
                                             # Filter by primary support reason
                                             radioButtons(inputId = "PrimarySupportReason", label = "Choose selected group", choices = primarySupportReasons, selected = "Average"),
                                             downloadButton(outputId = "download2dData", label = "Download csv"),
                                             htmlOutput(outputId = "disclaimerText2d"),
                                             width = 4
                                     ),
                                     mainPanel(
                                        htmlOutput(outputId = "explanatoryText2d"),
                                     plotlyOutput(outputId = "plotlyPlot"),
                                     br(),
                                     htmlOutput(outputId = "noteText"),
                                     tableOutput(outputId = "coefTable"), width = 8
                                     )
                                     ),
                
                            # 3d plot
                            tabPanel("3d Plot (time series)", 
                                     textOutput(outputId = "explanatoryText3d"),
                                     fluidRow(
                                             # Input: Select the x axis
                                             column(4,
                                                    selectInput(inputId = "xAxisChoice3d", label = "Choose x-axis", choices = xAxis3dChoices, selected = 1112)
                                             ),
                                             # Input: Select the y axis
                                             column(4,
                                                    selectInput(inputId = "yAxisChoice3d", label = "Choose y-axis", choices = yAxis3dChoices, selected = 1113)
                                             ),
                                             column(4,
                                                    radioButtons(inputId = "plotDimensions", label = "Select plot dimensions (2d then 3d recommended)", choices = c("2d", "3d"), selected = "2d")
                                             )                                     
                                             
                                     ),
                                     plotlyOutput(outputId = "plotlyPlot3d"),
                                     uiOutput(outputId = "3dnoteText"),
                                     downloadButton(outputId = "download3dData", label = "Download data for selected indicators (csv)") 
                                     
                                     
                                     
                            ), 
                            
                            # About
                            tabPanel("About", uiOutput(outputId = "outputUI"))
                            
                            
                ),
                width = 9
        )
        
        
        
        
)

server <- function(input, output, session) {

        # Update 3d plot y axis list when x axis changes and set plots back to 2d
        observeEvent(input$xAxisChoice3d,
                     {
                             indicators3dData %>% filter(IndicatorID==input$xAxisChoice3d & value==TRUE) %>% 
                                     select(Match) %>% transmute(IndicatorID=as.integer(as.character(Match))) %>% 
                                     left_join(uniqueIndicators, by = "IndicatorID") -> yAxis3dNewFrame 
                                   
                                     yAxis3dNewValues <- setNames(yAxis3dNewFrame$IndicatorID, yAxis3dNewFrame$IndicatorName)
                                     
                                     updateSelectInput(session = session, inputId = "yAxisChoice3d", label = "Choose y-axis", choices = yAxis3dNewValues, selected = yAxis3dNewValues[2])
                                     
                                     # Set plot back to 2d
                                     updateRadioButtons(session = session, inputId = "plotDimensions", label = "Select plot dimensions (2d then 3d recommended)", choices = c("2d", "3d"), selected = "2d")
                                     
                                     
                     })
        
        # Also want to set plot back to 2d when y Axis is changed
        observeEvent(input$xAxisChoice3d,
                     {       
                             # Set plot back to 2d
                             updateRadioButtons(session = session, inputId = "plotDimensions", label = "Select plot dimensions (2d then 3d recommended)", choices = c("2d", "3d"), selected = "2d")
                             
                             
                     })
        
        
        
        # This is the reactive statement that contains the functions to build the 2d plot
        plotToRender <- reactive({        
                
                # Select an indicator
                indicatorsSocialCare %>% filter(IndicatorID == input$yAxisChoice2d) ->
                currentData
                
                # Wrangle the data
                currentData %<>% 
                        select(IndicatorID, IndicatorName, AreaCode, AreaName, TimePeriodAsDate, Value) %>% 
                        filter(TimePeriodAsDate == max(TimePeriodAsDate)) %>%  # could change this to a shiny variable rather than most recent automatically
                        group_by(AreaCode) %>% 
                        summarise(TotalValue = mean(Value, na.rm = TRUE), IndicatorID = mean(IndicatorID), IndicatorName = IndicatorName[1]) %>% # basically just get one value per local authority if there are duplicates for same date
                        mutate(PrimarySupportReason_Key = input$PrimarySupportReason) %>% 
                        left_join(spendingData, by = c("PrimarySupportReason_Key", "AreaCode")) %>% 
                        filter(!is.na(SpendingPerPerson)) %>% # Take out any NAs - which should only be Buckinghamshire in Dementia
                        filter(AreaCode != "E06000053" & AreaCode != "E09000001") %>% # Take out Sicilly and City of London because they squash everything
                        filter(TotalValue!=0) # If total value is 0 this is because they haven't submitted data
                
                
                
                # Get units
                metaData %>% filter(IndicatorID == input$yAxisChoice2d) %>% 
                        select(28) %>% as.character -> yAxisUnits
                
                # Assign data frame to global environment so user can download
                csvSpendingVsOutcomes <<- currentData
                
                scatterPlot <- plot_ly(data = currentData, x = ~SpendingPerPerson, y = ~TotalValue, type = "scatter", mode = "markers", text = ~DH_GEOGRAPHY_NAME)
                
                # Get linear regression line
                fit <- lm(data = currentData, formula = TotalValue ~ I(SpendingPerPerson / 1000))
                
                coefOut <- summary(fit)$coef %>% as_tibble(rownames = "Coefficients")
                coefOut[2,1] <- "Change per £1000 per person/year"
                
                # Define axes a bit
                f <- list(
                        size = 18
                )
                fx <- list(
                        size = 14
                )
                y <- list(
                        title = yAxisUnits,
                        titlefont = f
                )
                x <- list(
                        title = "Average Spending Per Person receiving services per year (£)",
                        titlefont = fx
                )
                
                scatterPlot  %<>% add_lines(x = ~SpendingPerPerson, y = fitted(fit)) %>% # add fit line
                   layout(yaxis = y, xaxis = x) %>% layout(showlegend = FALSE)
                
                
                
                # clearly it would make sense to deal with the y axis percent label in the data rather than in the rendering
                list(plot = scatterPlot, table = coefOut)
                
        }
        )
        # Metadata lookup function
        metaDataToWrite <- reactive({
                metaData %>% filter(IndicatorID == input$yAxisChoice2d) %>% 
                        select(3, 28) -> metaOutput
                metaCharacter <- paste("<b>", "Indicator Definition:", "</b>", as.character(metaOutput[1]), "<b>", "Units:", "</b>", as.character(metaOutput[2]))
                metaCharacter
        })

        
        build3dPlot <- reactive({
                # First choice of variable
                indicatorsSocialCare %>% filter(IndicatorID == input$xAxisChoice3d) ->
                        firstVariable
                
                # Filter first choice of variable
                firstVariable %<>% 
                        select(IndicatorID, IndicatorName, AreaCode, AreaName, TimePeriodAsDate, Value) %>% 
                        group_by(AreaCode, AreaName, TimePeriodAsDate) %>% 
                        summarise(TotalValueX = sum(Value, na.rm = TRUE), IndicatorID = IndicatorID, IndicatorName = IndicatorName) %>% # basically just get one value per local authority per reporting period
                        filter(AreaCode != "E06000053" & AreaCode != "E09000001") %>% # Take out Sicilly and City of London because they squash everything
                        filter(TotalValueX!=0) # If total value is 0 this is because they haven't submitted data
                
                # Filter second choice of variable
                indicatorsSocialCare %>% filter(IndicatorID == input$yAxisChoice3d) ->
                        secondVariable
                
                secondVariable %<>% 
                        select(IndicatorID, IndicatorName, AreaCode, AreaName, TimePeriodAsDate, Value) %>% 
                        group_by(AreaCode, AreaName, TimePeriodAsDate) %>% 
                        summarise(TotalValueY = sum(Value, na.rm = TRUE), IndicatorID = IndicatorID, IndicatorName = IndicatorName) %>% # basically just get one value per local authority per reporting period
                        filter(AreaCode != "E06000053" & AreaCode != "E09000001") %>% # Take out Sicilly and City of London because they squash everything
                        filter(TotalValueY!=0)  # If total value is 0 this is because they haven't submitted data
                
                bothVariables <- left_join(firstVariable, secondVariable, by = c("AreaCode", "AreaName", "TimePeriodAsDate"))
                
                # Assign this data frame to global environment so user can click download button at any time
                csv3dOutput <<- bothVariables                
                
                # Get y Axis Units
                yAxisUnits <- metaData %>% filter(IndicatorID == input$yAxisChoice3d) %>% 
                        select(28) %>% as.character 
                
                # Get y Axis label
                yAxisLabel <- metaData %>% filter(IndicatorID == input$yAxisChoice3d) %>% 
                        select(3) %>% as.character 
                
                # Get x Axis Units
                xAxisUnits <- metaData %>% filter(IndicatorID == input$xAxisChoice3d) %>% 
                        select(28) %>% as.character 
                
                # Get x Axis label
                xAxisLabel <- metaData %>% filter(IndicatorID == input$xAxisChoice3d) %>% 
                        select(3) %>% as.character 
                
                
                # Define axes a bit
                f <- list(
                        size = 10
                )
                x <- list(
                        title = paste0(xAxisLabel, " (", xAxisUnits, ")"),
                        titlefont = f
                )
                y <- list(
                        title = paste0(yAxisLabel, " (", yAxisUnits, ")"),
                        titlefont = f
                )
                
                                
                if(input$plotDimensions=="2d"){
                # so a 2d scatter plot would be:
                scatterPlot2d <- plot_ly(data = bothVariables, x = ~TotalValueX, y = ~TotalValueY, type = "scatter", mode = "markers", text = ~AreaName)
                
                
                scatterPlot2d %>%
                        layout(yaxis = y, xaxis =x, title = "PHE Fingertips indicators: 2D comparison")                        
                }
                else{
                # so a 3d scatter plot would be:
                scatterPlot3d <- plot_ly(data = bothVariables, x = ~TotalValueX, y = ~TotalValueY, z = ~TimePeriodAsDate, type = "scatter3d", mode = "markers", text = ~AreaName, marker = list(size = 2))
                        
                z <- list(
                        title = "Time"
                )
                scatterPlot3d %>%
                        layout(
                                title = "PHE Fingertips indicators: Time-series comparison",
                                scene = list(
                                        xaxis = x,
                                        yaxis = y,
                                        zaxis = z
                                )
                                )
                }
                
                
        })

        # Call the reactive function to render 2d plot
        output$plotlyPlot <- renderPlotly(
                plotToRender()$plot
                                          )
        
        
        # Call the table output of the 2d plot
        output$coefTable <- renderTable(plotToRender()$table)
        
        # Print the metadata info of the 2d plot
        output$noteText <- renderText(metaDataToWrite())
        
        # Call the reactive function to render the 3d plot
        output$plotlyPlot3d <- renderPlotly(
                build3dPlot()
        )
        
        

        
        # Download full data -3d
        output$download3dData <- downloadHandler(
                
                filename = "3dIndicators.csv",
                content = function(file) {
                        write.csv(csv3dOutput, file, row.names = FALSE)
                }
        )
        
        # Download full data - 2d (spending vs outcomes)
        output$download2dData <- downloadHandler(
                
                filename = "spendingvsoutcome.csv",
                content = function(file) {
                        write.csv(csvSpendingVsOutcomes, file, row.names = FALSE)
                }
        )
        
        # Everything below this point is static text
        output$explanatoryText3d <- renderText(
                "This is where you can explore the interaction between indicators over time. The drop down menu will dynamically update, as indicators can only be compared against others with the same reporting dates. Once desired indicators are selected, choose 3d to see the indicators split over time."
        )

        output$explanatoryText2d <- renderText(
                "This section allows you to plot PHE Fingertips Outcomes against Adult Social Care Spending, either by support group or averaged across support groups."
        )
        output$disclaimerText2d <- renderText(
                "<b>Note:</b> Spending is calculated as a yearly mean per person receiving services, based on dividing Adult Social Care Finance Report spending by PHE Fingertips prevalence data. Please see About section for more info and caveats about interpretation."
        )
        

        # About section - just displays the markdown file
        output$outputUI <- renderUI({ 
                
                
                includeMarkdown("include.md")
                
                
        })                        
        
}

shinyApp(ui = ui, server = server)