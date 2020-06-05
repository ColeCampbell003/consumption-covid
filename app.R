# =============================================================================
# title: Toy R Shiny App
# date: 05/21/2020
# comments: this script (1) runs analysis for an R Shiny App using Earnhest
# consumption data, and (2) creates the barebones app for the project.
# =============================================================================

# libraries
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(ggthemes)
library(shiny)
library(shinythemes)

# preferences
# dir <- "/Users/colecampbell/Box Sync/Inequality-COVID/Shiny App"
# datadir <- file.path(dir, "Data")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# clean data
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# import data
# data <- read_excel(file.path(datadir, "earnest05_21_2020.xlsx"))
data <- read_excel("Data/earnest05_21_2020.xlsx")
data_st <- read_excel("Data/earnest05_25_2020_state.xlsx")
data_subcat <- read_excel("Data/earnest05-28-2020-subcategory.xlsx")


## clean category data ##
# rename variables
data <- data %>% 
    select(-c(`Week Ending Label`, `Weeks Trailing Value Label`)) %>% 
    rename(category = Category,
           day = `Day of Earnest Week End Date`,
           earn_week = `Earnest Week`,
           year = `Earnest Year`,
           month = `Month of Earnest Week End Date`,
           lag_spend = `Trailing Weeks Value`,
           lag_growth = `Trailing Weeks YoY Growth`,
    ) %>% 
    # take logs of spending
    mutate(log_spend = log(lag_spend)) %>% 
    mutate(date = paste(month, day, sep = " ") %>% 
               paste0(",") %>% 
               paste(year, sep = " ") %>% 
               as.Date(format = '%B %d, %Y')) %>% 
    # make category a factor
    mutate(category = as.factor(category)) %>% 
    # make percent change from january (2020)
    group_by(category) %>%
    mutate(pct_change = 
               ((lag_spend - lag_spend[date==as.Date("2020-01-08")]
               )/lag_spend[date==as.Date("2020-01-08")])*100)

# make consistent date variable -> i think there's something fucked up with 
# their dates
# data <- data %>% 
#   mutate(year = ifelse(earn_week=="Week 52" & year==2019, 2020, 2019))

# make column of total sum by date
data_total <- data %>% 
    group_by(date) %>%
    mutate(total_spend = sum(lag_spend, na.rm = TRUE)) %>% 
    select(date, total_spend) %>% 
    distinct() %>% 
    ungroup()

data_total <- data_total %>%
    mutate(log_spend = log(total_spend)) %>% 
    mutate(pct_change = 
               ((total_spend - total_spend[date==as.Date("2020-01-08")]
                 )/total_spend[date==as.Date("2020-01-08")])*100) %>% 
    mutate(yoy_growth = (total_spend -  lag(total_spend, 52))/
               lag(total_spend,52)*100)

## clean state data ##
# select variables
data_st <- data_st %>% 
    rename(state = State,
           yoy_growth = yoyGrowth) %>% 
    select(c(state, date, consumption, yoy_growth)) %>% 
    group_by(state) %>% 
    mutate(date = as.Date(date)) %>% 
    mutate(log_spend = log(consumption)) %>% 
    mutate(pct_change = ((consumption - consumption[date==as.Date("2020-01-01")]
                          )/consumption[date==as.Date("2020-01-01")])*100)
 
## clean subcategory data ##
data_subcat <- data_subcat %>% 
    select(-c(`Week Ending Label`, `Weeks Trailing Value Label`)) %>% 
    # rename variables
    rename(category = Category, 
           day = `Day of Earnest Week End Date`,
           earn_week = `Earnest Week`,
           year = `Earnest Year`,
           month = `Month of Earnest Week End Date`,
           subcategory = Subcategory,
           lag_spend = `Trailing Weeks Value`,
           lag_growth = `Trailing Weeks YoY Growth`
           ) %>% 
    # make consistent date variable
    mutate(date = paste(month, day, sep = " ") %>% 
               paste0(",") %>% 
               paste(year, sep = " ") %>% 
               as.Date(format = '%B %d, %Y')) %>% 
    # take logs of spending
    mutate(log_spend = log(lag_spend)) %>% 
    group_by(subcategory) %>% 
    mutate(pct_change = 
               ((lag_spend - lag_spend[date==as.Date("2020-01-08")]
               )/lag_spend[date==as.Date("2020-01-08")])*100)
    



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# app interface
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# define ui for app
ui <- navbarPage(
    title = "Tracking Consumer Spending During COVID-19",
    fluid = TRUE,
    theme = shinytheme("superhero"),
    tabPanel("National Trends",
             sidebarLayout(
                 sidebarPanel(
                     # input: spending categories checkbox
                     checkboxGroupInput(inputId = "category",
                                        label = "Select Spending Category",
                                        choices = data$category %>% 
                                            unique %>% 
                                            sort,
                                        selected = data$category[1]
                     ),
                     
                     # input: select log or percent change graph
                     radioButtons(inputId = "graph_choice",
                                  label = "Select Graph Type",
                                  choices = list("Log Scale" = "log_type",
                                                 "Percent Change" = "pct_type",
                                                 "YoY Growth" = "yoy_type"),
                                  selected = "log_type"),
                     
                     # input: dates slider
                     sliderInput(inputId = "date_range",
                                 label = "Date Range",
                                 min = min(data$date),
                                 max = max(data$date),
                                 value = c(as.Date("January 1, 2020", format = '%B %d, %Y'),
                                           max(data$date))),
                     width = 3
                 ),
                 
                 # main panel for displaying outputs
                 mainPanel(
                     plotOutput(outputId = "totalPlot"),
                     plotOutput(outputId = "tsPlot")
                 )
             )
    ),
    tabPanel("State Trends",
             sidebarLayout(
                 sidebarPanel(
                     
                     # input: dates slider
                     sliderInput(inputId = "date_range_st",
                                 label = "Date Range",
                                 min = min(data_st$date),
                                 max = max(data_st$date),
                                 value = c(as.Date("January 1, 2020", format = '%B %d, %Y'),
                                           max(data_st$date))),
                     
                     # input: select log or percent change graph
                     radioButtons(inputId = "graph_choice_st",
                                  label = "Select Graph Type",
                                  choices = list("Log Scale" = "log_type",
                                                 "Percent Change" = "pct_type",
                                                 "YoY Growth" = "yoy_type"),
                                  selected = "log_type"),
                     
                     # input: state selections
                     selectInput(inputId = "states",
                                 label = "Select States",
                                 choices = data_st$state %>% 
                                     unique %>% 
                                     sort,
                                 multiple = TRUE)
                 ),
                 
                 # main panel for displaying outputs
                 mainPanel(
                     plotOutput(outputId = "statePlot")
                 )
             )
    )
    # ,
    # tabPanel("Spending Subcategories",
    #          sidebarLayout(
    #            sidebarPanel(
    #              # input: dates slider
    #              sliderInput(inputId = "date_range_st",
    #                          label = "Date Range",
    #                          min = min(data_st$date),
    #                          max = max(data_st$date),
    #                          value = c(as.Date("January 1, 2020", format = '%B %d, %Y'),
    #                                    max(data_st$date))),
    #              
    #              # input: select log or percent change graph
    #              radioButtons(inputId = "graph_choice_st",
    #                           label = "Select Graph Type",
    #                           choices = list("Log Scale" = "log_type",
    #                                          "Percent Change" = "pct_type",
    #                                          "YoY Growth" = "yoy_type"),
    #                           selected = "log_type"),
    #              
    #              # input: state selections
    #              conditionalPanel(condition = )
    #            )
    #          )
    # )
)

# define server logic
server <- function(input, output) {

    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    # national trends
    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    # output: data
    plotData <- reactive({
        data %>% 
            filter(category %in% input$category) %>% 
            filter(date>=input$date_range[1] & date<=input$date_range[2])
    })
    
    plotDataTotal <- reactive({
        data_total %>%
            filter(date>=input$date_range[1] & date<=input$date_range[2])
    })
    
    # output: graph type selection
    plotType <- reactive({
        input$graph_choice
    })
    
    # output: total plot
    output$totalPlot <- renderPlot({
        if (plotType()=="log_type") {
            plotDataTotal() %>% 
                # plot
                ggplot(aes(x = date, y = log_spend)) +
                geom_line() +
                geom_vline(xintercept = as.Date("March 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                geom_vline(xintercept = as.Date("April 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                xlab("Date") +
                ylab("Log Spending") +
                labs(title = "Aggregate Consumption Changes", 
                     caption = "Source: Earnest Research") +
                theme_economist_white(gray_bg = FALSE)
        } else if (plotType()=="pct_type") {
            plotDataTotal() %>% 
                # plot
                ggplot(aes(x = date, y = pct_change)) +
                geom_line() +
                geom_vline(xintercept = as.Date("March 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                geom_vline(xintercept = as.Date("April 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                xlab("Date") +
                ylab("% Change") +
                labs(title = "Aggregate Consumption Changes", 
                     caption = "Source: Earnest Research") +
                theme_economist_white(gray_bg = FALSE)
        } else {
            plotDataTotal() %>% 
                # plot
                ggplot(aes(x = date, y = yoy_growth)) +
                geom_line() +
                geom_vline(xintercept = as.Date("March 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                geom_vline(xintercept = as.Date("April 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                xlab("Date") +
                ylab("YoY Growth") +
                labs(title = "Aggregate Consumption Changes", 
                     caption = "Source: Earnest Research") +
                theme_economist_white(gray_bg = FALSE)
        }
    })
    
    # output: time series plot
    output$tsPlot <- renderPlot({
        # output: percent change plot
        if (plotType()=="log_type") {
            plotData() %>%
                group_by(category) %>% 
                ggplot(aes(x = date, y = log_spend, color = category)) +
                geom_line() +
                geom_vline(xintercept = as.Date("March 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                geom_vline(xintercept = as.Date("April 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                xlab("Date") +
                ylab("Log Spending") +
                labs(title = "Consumption Changes by Category", 
                     color = "Category",
                     caption = "Source: Earnest Research") +
                guides(col = guide_legend("Category")) +
                theme_economist_white(gray_bg = FALSE) +
                scale_colour_economist() +
                scale_linetype_identity()
        } 
        else if (plotType()=="pct_type") {
            # output: percent change plot
            plotData() %>% 
                group_by(category) %>% 
                ggplot(aes(x = date, y = pct_change, color = category)) +
                geom_line() +
                geom_vline(xintercept = as.Date("March 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                geom_vline(xintercept = as.Date("April 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                xlab("Date") +
                ylab("% Change") +
                labs(title = "Consumption Changes by Category", 
                     color = "Category",
                     caption = "Source: Earnest Research") +
                theme_economist_white(gray_bg = FALSE) +
                scale_colour_economist() +
                scale_linetype_identity()
        }
        else {
            # output: year to year growth plot
            plotData() %>% 
                group_by(category) %>% 
                ggplot(aes(x = date, y = lag_growth, color = category)) +
                geom_line() +
                geom_vline(xintercept = as.Date("March 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                geom_vline(xintercept = as.Date("April 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                xlab("Date") +
                ylab("YoY Growth") +
                labs(title = "Consumption Changes by Category", 
                     color = "Category",
                     caption = "Source: Earnest Research") +
                theme_economist_white(gray_bg = FALSE) +
                scale_colour_economist() +
                scale_linetype_identity()
        }
    }) 
    
    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    # state trends
    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    # output: data
    stateData <- reactive({
        data_st %>% 
            filter(state %in% input$states) %>% 
            filter(date>=input$date_range_st[1] & date<=input$date_range_st[2])
    })
    
    # output: graph type selection
    plotTypeState <- reactive({
        input$graph_choice_st
    })
    
    # # output: plot
    # output$statePlot <- renderPlot({
    #     # output: log plot
    #     if (plotType=="log_type")
    #     stateData() %>% 
    #         group_by(state) %>% 
    #         ggplot(aes(x = date, y = pct_change, color = state)) +
    #         geom_line() +
    #         geom_vline(xintercept = as.Date("March 13, 2020", format = '%B %d, %Y'),
    #                    linetype = 4) +
    #         geom_vline(xintercept = as.Date("April 13, 2020", format = '%B %d, %Y'),
    #                    linetype = 4) +
    #         xlab("Date") +
    #         ylab("% Change") +
    #         labs(title = "Consumption Changes by State", 
    #              color = "State",
    #              caption = "Source: Earnest Research") +
    #         theme_economist_white(gray_bg = FALSE) +
    #         scale_colour_colorblind() +
    #         scale_linetype_identity()
    #     
    # })
    
    
    # output: time series plot
    output$statePlot <- renderPlot({
        # output: log plot
        if (plotTypeState()=="log_type") {
            stateData() %>% 
                group_by(state) %>% 
                ggplot(aes(x = date, y = log_spend, color = state)) +
                geom_line() +
                geom_vline(xintercept = as.Date("March 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                geom_vline(xintercept = as.Date("April 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                xlab("Date") +
                ylab("Log Spending") +
                labs(title = "Consumption Changes by Category", 
                     color = "Category",
                     caption = "Source: Earnest Research") +
                guides(col = guide_legend("Category")) +
                theme_economist_white(gray_bg = FALSE) +
                scale_colour_economist() +
                scale_linetype_identity()
        } 
        else if (plotTypeState()=="pct_type") {
            # output: percent change plot
            stateData() %>% 
                group_by(state) %>% 
                ggplot(aes(x = date, y = pct_change, color = state)) +
                geom_line() +
                geom_vline(xintercept = as.Date("March 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                geom_vline(xintercept = as.Date("April 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                xlab("Date") +
                ylab("% Change") +
                labs(title = "Consumption Changes by Category", 
                     color = "Category",
                     caption = "Source: Earnest Research") +
                theme_economist_white(gray_bg = FALSE) +
                scale_colour_economist() +
                scale_linetype_identity()
        }
        else {
            # output: year to year growth plot
            stateData() %>% 
                group_by(state) %>% 
                ggplot(aes(x = date, y = yoy_growth, color = state)) +
                geom_line() +
                geom_vline(xintercept = as.Date("March 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                geom_vline(xintercept = as.Date("April 13, 2020", format = '%B %d, %Y'),
                           linetype = 4) +
                xlab("Date") +
                ylab("YoY Growth") +
                labs(title = "Consumption Changes by Category", 
                     color = "Category",
                     caption = "Source: Earnest Research") +
                theme_economist_white(gray_bg = FALSE) +
                scale_colour_economist() +
                scale_linetype_identity()
        }
    }) 
}

# run app
shinyApp(ui = ui, server = server)
