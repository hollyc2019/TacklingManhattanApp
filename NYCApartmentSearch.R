library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggmap)
library(lubridate)
library(tidyverse)
library(readxl)
library(stargazer)
library(rsconnect)
library(tidyverse)
library(shinythemes)
library(plotly)
library(readr)
library(shinyWidgets)


# reading in my datasets
# dataset 1
#
ZmedianAskingRent <- read_csv("/Users/hollychristensen/Desktop/finalproject/Gov1005FinalProject/medianAskingRent_All.csv")
manhattanmedianaskingrent <- ZmedianAskingRent %>% 
  filter(Borough == "Manhattan") %>% 
  gather(key = "year_month", value = "asking_price", -c(areaName, areaType, Borough))
# dataset 2
#
ZrentalInventory_All <- read_csv("/Users/hollychristensen/Desktop/finalproject/Gov1005FinalProject/rentalInventory_All.csv")
manhattaninventory <- ZrentalInventory_All %>% 
  filter(Borough == "Manhattan", areaType == "submarket") %>% 
  gather(key = "year_month", value = "units", -c(areaName, areaType, Borough)) %>% 
  group_by(areaName)
# dataset 3
#
Zrollingsales_manhattan <- read_excel("/Users/hollychristensen/Desktop/finalproject/Gov1005FinalProject/rollingsales_manhattan.xls", skip = 4) 
manhattan <- Zrollingsales_manhattan %>% 
  filter(`BUILDING CLASS CATEGORY` == c("01 ONE FAMILY DWELLINGS", "02 TWO FAMILY DWELLINGS", "03 THREE FAMILY DWELLINGS", "07 RENTALS - WALKUP APARTMENTS", "08 RENTALS - ELEVATOR APARTMENTS", "09 COOPS - WALKUP APARTMENTS", "10 COOPS - ELEVATOR APARTMENTS"), `SALE PRICE` <= 10000000) %>% 
  group_by(`BUILDING CLASS CATEGORY`, `NEIGHBORHOOD`)
# dataset 4
#
ZOpen_HPD_Violations <- read_csv("/Users/hollychristensen/Desktop/finalproject/Gov1005FinalProject/Open_HPD_Violations.csv")
manhattanHPD <- ZOpen_HPD_Violations %>% 
  filter(Borough == 'MANHATTAN')

# Navigation Bar Design
# Playing with various color choices
nav_bar_html = '.navbar { background-color: #00BFFF }
                .navbar-default
.navbar-brand{color: #F7FE2E;}

.navbar-nav li a:hover, .navbar-nav > .active > a {
color: #00BFFF !important;
background-color: #F7FE2E !important;
}
.nav-tabs li a:hover, .nav-tabs > .active > a {
background-color: #FA58F4 !important;
}'

year_choices <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

# Define shiny UI 
ui <- 
# This is the title of my app with proper formatting 
  navbarPage("Tackling Manhattan",
             position="fixed-top",
             collapsible=TRUE,
# The first tab on my app is Availability            
             tabPanel(
               "Availability",
               tags$style(type = 'text/css', 
                          'body {padding-top: 70px;}',
                          HTML(nav_bar_html)
               ),
# The title for the Availability tab             
               titlePanel("Rental Availability by Area"),
# Subtitle describing what I'm analyzing
               p("Analysis of the number of rentable units in Manhattan, New York City from January 2010 until October 2018."),
# What does my analysis show
               helpText("In this tab we have provided the ability for experimentation on the data by anchoring on a specific area within Manhattan. The value here is to be able to compare unit availability/area popularity over the last 8 years to understand similarities and differences between the different areas"),
# Step by step instructions for the user 
               HTML(paste('<b style="font-size:22px">Instructions:</b>', 
                          '1. Step uno', 
                          '2. Step dos',
                          '3. Step tres',
                          '4. Step quatro',
                          sep="<br/>")),
               
               hr(),
               
               # Sidebar layout with input and output definitions
      sidebarLayout(
        # Sidebar panel for interactive input
        sidebarPanel(
          # Describing how the data is already broken up by area within Manhattan for user simplicity
          tags$b("The data is for the borough of Manhattan and is filtered by the top 5 submarkets within it."),
          hr(),
          # directions for input
          h2("Choose years to display"),
          # second input: drop-down menu to display selections for year
          pickerInput("year", "Years to show:", choices = c("2010" = "2010",
                                                            "2011" = "2011",
                                                            "2012" = "2012",
                                                            "2013" = "2013",
                                                            "2014" = "2014",
                                                            "2015" = "2015",
                                                            "2016" = "2016",
                                                            "2017" = "2017",
                                                            "2018" = "2018"),
                      selected = c("2010" = "2010",
                                   "2011" = "2011",
                                   "2012" = "2012",
                                   "2013" = "2013",
                                   "2014" = "2014",
                                   "2015" = "2015",
                                   "2016" = "2016",
                                   "2017" = "2017",
                                   "2018" = "2018"), 
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE),
          # option for user to explore code further
          # create checkbox for linear model
          checkboxInput("line", label = "Add linear model")
          
        ),
          mainPanel(
            # Output: Tabset w/ 5 plots for each submarket within Manhattan
            tabsetPanel(type = "tabs",
                        tabPanel("Downtown", plotOutput("plot_boroughs_Downtown")),
                        tabPanel("Midtown", plotOutput("plot_boroughs_Midtown")),
                        tabPanel("Upper East Side", plotOutput("plot_boroughs_Upper_East")), 
                        tabPanel("Upper Manhattan", plotOutput("plot_boroughs_Upper_Manhattan")), 
                        tabPanel("Upper West Side", plotOutput("plot_boroughs_Upper_West"))
                     )
                   )
                 )
               )
             )
                   
              

# Defining server logic 
server <- function(input, output) {
  
  my_theme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (25)), 
                    plot.subtitle=element_text(family = "Helvetica", face = "bold", size = (25)),
                    axis.text.x = element_text(angle = 0, hjust = .5),
                    legend.title = element_text(face = "bold.italic", family = "Helvetica", size=20), 
                    legend.text = element_text(face = "italic", family = "Helvetica", size=13), 
                    axis.title = element_text(family = "Helvetica", size = (20)),
                    axis.text = element_text(family = "Courier", size = (13)))
  
  default_plot <- ggplot(data.frame()) +
    geom_point() + 
    xlim(2010, 2018) + 
    ylim(0, 10000) +
    my_theme
  
  output$plot_boroughs_Downtown <- renderPlot({
    # filter data for selected years
    plot_boroughs_Downtown <- reactive({
      plot_boroughs_Downtown <- downtownMI[downtownMI$year %in% input$year, ]
  })
  
  # use if statement to create different plots for with and without linear model selection
  if(input$line == TRUE) {
    ggplot(data = plot_boroughs_Downtown(), aes_string(x = "month", y = "units", color = "year")) +
      geom_point() +
      geom_smooth(method=loess) +
      labs(x = "Month",
           y = "Units",
           title = "Availability of Rental Units Over the Last 8 Years",
           subtitle = "Helpful analysis for deciding when to buy")
  }
  else{
    ggplot(data = plot_boroughs_Downtown(), aes_string(x = "month", y = "units", color = "year")) +
      geom_point() +
      labs(x = "Month",
           y = "Units",
           title = "Availability of Rental Units Over the Last 8 Years",
           subtitle = "Helpful analysis for deciding when to buy")
  }
})
  
  output$plot_boroughs_Midtown <- renderPlot({
    # filter data for selected years
    plot_boroughs_Midtown <- reactive({
      plot_boroughs_Midtown <- midtownMI[midtownMI$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line == TRUE) {
      ggplot(data = plot_boroughs_Midtown(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
    else{
      ggplot(data = plot_boroughs_Midtown(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
  })
  
  output$plot_boroughs_Upper_East <- renderPlot({
    # filter data for selected years
    plot_boroughs_Upper_East <- reactive({
      plot_boroughs_Upper_East <- uppereastMI[uppereastMI$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line == TRUE) {
      ggplot(data = plot_boroughs_Upper_East(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
    else{
      ggplot(data = plot_boroughs_Upper_East(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
  })
  
  output$plot_boroughs_Upper_Manhattan <- renderPlot({
    # filter data for selected years
    plot_boroughs_Upper_Manhattan <- reactive({
      plot_boroughs_Upper_Manhattan <- uppermanhattanMI[uppermanhattanMI$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line == TRUE) {
      ggplot(data = plot_boroughs_Upper_Manhattan(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
    else{
      ggplot(data = plot_boroughs_Upper_Manhattan(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
  })
  
  output$plot_boroughs_Upper_West <- renderPlot({
    # filter data for selected years
    plot_boroughs_Upper_West <- reactive({
      plot_boroughs_Upper_West <- upperwestMI[upperwestMI$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line == TRUE) {
      ggplot(data = plot_boroughs_Upper_West(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
    else{
      ggplot(data = plot_boroughs_Upper_West(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
  })
}

    
shinyApp(ui = ui, server = server)

