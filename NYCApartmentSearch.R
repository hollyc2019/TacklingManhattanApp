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
# Dataset 1
#
ZmedianAskingRent <- read_csv("medianAskingRent_All.csv")
manhattanmedianaskingrent <- ZmedianAskingRent %>% 
  filter(Borough == "Manhattan") %>% 
  gather(key = "year_month", value = "asking_price", -c(areaName, areaType, Borough))
# subsetting data for submarket 1 - Lower East Side Manhattan
lower_eastP <- manhattanmedianaskingrent %>% 
  select(areaName, year_month, asking_price) %>% 
  filter(areaName == "Lower East Side") %>% 
  separate(year_month, c("year", "month")) 
lower_eastP$asking_price <- as.numeric(lower_eastP$asking_price)
lower_eastP$month <- as.numeric(lower_eastP$month)
write.csv(lower_eastP, "lower_eastP.csv")
# repeating above but for the remaining submarkets
# Submartket 2 - Chelsea Manhattan
chelseaP <- manhattanmedianaskingrent %>% 
  select(areaName, year_month, asking_price) %>% 
  filter(areaName == "Chelsea") %>% 
  separate(year_month, c("year", "month"))
chelseaP$asking_price <- as.numeric(chelseaP$asking_price)
chelseaP$month <- as.numeric(chelseaP$month)
write.csv(chelseaP, "chelseaP.csv")
# Submartket 3 - Soho Manhattan
sohoP <- manhattanmedianaskingrent %>% 
  select(areaName, year_month, asking_price) %>% 
  filter(areaName == "Soho") %>% 
  separate(year_month, c("year", "month")) 
sohoP$asking_price <- as.numeric(sohoP$asking_price)
sohoP$month <- as.numeric(sohoP$month)
write.csv(sohoP, "sohoP.csv")
# Submartket 4 - Upper Manhattan
FiDiP <- manhattanmedianaskingrent %>% 
  select(areaName, year_month, asking_price) %>% 
  filter(areaName == "Financial District") %>% 
  separate(year_month, c("year", "month")) 
FiDiP$asking_price <- as.numeric(FiDiP$asking_price)
FiDiP$month <- as.numeric(FiDiP$month)
write.csv(FiDiP, "FiDiP.csv")
# Submartket 5 - East Village Manhattan
east_villageP <- manhattanmedianaskingrent %>% 
  select(areaName, year_month, asking_price) %>% 
  filter(areaName == "East Village") %>% 
  separate(year_month, c("year", "month")) 
east_villageP$asking_price <- as.numeric(east_villageP$asking_price)
east_villageP$month <- as.numeric(east_villageP$month)
write.csv(east_villageP, "east_villageP.csv")
# Submartket 6 - West Village Manhattan
west_villageP <- manhattanmedianaskingrent %>% 
  select(areaName, year_month, asking_price) %>% 
  filter(areaName == "West Village") %>% 
  separate(year_month, c("year", "month")) 
west_villageP$asking_price <- as.numeric(west_villageP$asking_price)
west_villageP$month <- as.numeric(west_villageP$month)
write.csv(west_villageP, "west_villageP.csv")
# Submarket 7 - Gramercy Manhattan
gramercyP <- manhattanmedianaskingrent %>% 
  select(areaName, year_month, asking_price) %>% 
  filter(areaName == "Gramercy Park") %>% 
  separate(year_month, c("year", "month")) 
gramercyP$asking_price <- as.numeric(gramercyP$asking_price)
gramercyP$month <- as.numeric(gramercyP$month)
write.csv(gramercyP, "gramercyP.csv")
# Also Battery Park City, Central Harlem, Central Park South, Chelsea, Chinatown, Civic Center, East Harlem, Financial District, Flatiron, Gramercy Park, Greenwich Village, Hamilton Heights, Inwood, Little Italy, Lower East Side, Manhattan, Manhattanville, Marble Hill, Midtown East, Midtown South, Midtown West, Morningside Heights, Nolita, Roosevelt Island, Soho, Tribeca, West Harlem, West Village
# Dataset 2
#
ZrentalInventory_All <- read_csv("rentalInventory_All.csv")
manhattaninventory <- ZrentalInventory_All %>% 
  filter(Borough == "Manhattan", areaType == "submarket") %>% 
  gather(key = "year_month", value = "units", -c(areaName, areaType, Borough))
# subsetting data for submarket 1 - Downtown Manhattan
downtownMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "All Downtown") %>% 
  separate(year_month, c("year", "month")) 
downtownMI$units <- as.numeric(downtownMI$units)
downtownMI$month <- as.numeric(downtownMI$month)
write.csv(downtownMI, "downtownMI.csv")
# repeating above but for the remaining 4 submarkets
# Midtown Manhattan
midtownMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "All Midtown") %>% 
  separate(year_month, c("year", "month"))
midtownMI$units <- as.numeric(midtownMI$units)
midtownMI$month <- as.numeric(midtownMI$month)
write.csv(midtownMI, "midtownMI.csv")
# Upper East Side Manhattan
uppereastMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "All Upper East Side") %>% 
  separate(year_month, c("year", "month"))
uppereastMI$units <- as.numeric(uppereastMI$units)
uppereastMI$month <- as.numeric(uppereastMI$month)
write.csv(uppereastMI, "uppereastMI.csv")
# Upper Manhattan
uppermanhattanMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "All Upper Manhattan") %>% 
  separate(year_month, c("year", "month"))
uppermanhattanMI$units <- as.numeric(uppermanhattanMI$units)
uppermanhattanMI$month <- as.numeric(uppermanhattanMI$month)
write.csv(uppermanhattanMI, "uppermanhattanMI.csv")
# Upper West Side Manhattan
upperwestMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "All Upper West Side") %>% 
  separate(year_month, c("year", "month"))
upperwestMI$units <- as.numeric(upperwestMI$units)
upperwestMI$month <- as.numeric(upperwestMI$month)
write.csv(upperwestMI, "upperwestMI.csv")

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
              p("In this tab we have provided the ability for experimentation on the data by anchoring on a specific area within Manhattan. The value here is to be able to compare unit availability/area popularity over the last 8 years to understand similarities and differences between the different areas."),
# Step by step instructions for the user 
              HTML(paste('<b style="font-size:22px">Instructions:</b>', 
                         '1. Using the tabs (highlighted in pink), select a submarket that you would like to take a closer look at.', 
                         '2. Using the drop-down menu, select which year(s) you would like to examine. All years have automatically been selected for you.',
                         '3. If you would like, check the box "Add a linear model" to be able to see the trends more clearly.',
                         '4. Be sure to toggle between tabs to compare and contrast the different submarkets of Manhattan.',
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
          checkboxInput("line", label = "Add linear model"),
          hr(),
          h4('See Code:'),
          HTML(paste('<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/downtownMI.csv" target="_blank">Downtown</a>', 
                     '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/midtownMI.csv" target="_blank">Midtown</a>', 
                     '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/uppereastMI.csv" target="_blank">Upper East Side</a>', 
                     '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/uppermanhattanMI.csv" target="_blank">Upper Manhattan</a>',
                     '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/upperwestMI.csv" target="_blank">Upper West Side</a>',
                     sep="<br/>"))
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
    xlim(0, 12) + 
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

