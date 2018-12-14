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
  filter(Borough == "Manhattan", areaType == "neighborhood") %>% 
  gather(key = "year_month", value = "units", -c(areaName, areaType, Borough))
# subsetting data for submarket 1 - Lower East Side Manhattan
lower_eastMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "Lower East Side") %>% 
  separate(year_month, c("year", "month")) 
lower_eastMI$units <- as.numeric(lower_eastMI$units)
lower_eastMI$month <- as.numeric(lower_eastMI$month)
write.csv(lower_eastMI, "lower_eastMI.csv")
# repeating above but for the remaining 4 submarkets
# Chelsea Manhattan
chelseaMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "Chelsea") %>% 
  separate(year_month, c("year", "month"))
chelseaMI$units <- as.numeric(chelseaMI$units)
chelseaMI$month <- as.numeric(chelseaMI$month)
write.csv(chelseaMI, "chelseaMI.csv")
# Soho Manhattan
sohoMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "Soho") %>% 
  separate(year_month, c("year", "month"))
sohoMI$units <- as.numeric(sohoMI$units)
sohoMI$month <- as.numeric(sohoMI$month)
write.csv(sohoMI, "sohoMI.csv")
# Financial District Manhattan
fidiMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "Financial District") %>% 
  separate(year_month, c("year", "month"))
fidiMI$units <- as.numeric(fidiMI$units)
fidiMI$month <- as.numeric(fidiMI$month)
write.csv(fidiMI, "fidiMI.csv")
# East Village Manhattan
eastvillageMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "East Village") %>% 
  separate(year_month, c("year", "month"))
eastvillageMI$units <- as.numeric(eastvillageMI$units)
eastvillageMI$month <- as.numeric(eastvillageMI$month)
write.csv(eastvillageMI, "eastvillageMI.csv")
# West Village Manhattan
westvillageMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "West Village") %>% 
  separate(year_month, c("year", "month"))
westvillageMI$units <- as.numeric(westvillageMI$units)
westvillageMI$month <- as.numeric(westvillageMI$month)
write.csv(westvillageMI, "westvillageMI.csv")
# Gramercy Manhattan
gramercyMI <-manhattaninventory %>% 
  select(areaName, year_month, units) %>% 
  filter(areaName == "Gramercy Park") %>% 
  separate(year_month, c("year", "month"))
gramercyMI$units <- as.numeric(gramercyMI$units)
gramercyMI$month <- as.numeric(gramercyMI$month)
write.csv(gramercyMI, "gramercyMI.csv")

# Navigation Bar Design
# Playing with various color choices
nav_bar_html = '.navbar { background-color: #B40431 }
                .navbar-default
.navbar-brand{color: #FFFFFF;}

.navbar-nav li a:hover, .navbar-nav > .active > a {
color: #FFFFFF !important;
background-color: #298A08 !important;
}
.nav-tabs li a:hover, .nav-tabs > .active > a {
color: #FFFFFF !important;
background-color: #01A9DB !important;
}'


# Define shiny UI 
ui <- 
# This is the title of my app with proper formatting 
  navbarPage("Tackling Manhattan",
             position="fixed-top",
             collapsible=TRUE,
# This is the first tab on my app, an about page
             tabPanel("About",
                      fluidPage(
                        fluidRow(
                          h3("Hello"),
                          p("Glad you could make it. What you have before you is a fully functional final project for Gov1005: Data. As you could probably tell from the title, 'Tackling Manhattan', this project focuses on Manhattan, the most densely populated of New York City's five boroughs. As of October 2018 I found out that I'll be working in Manhattan and decided what better topic to focus my final project on then the place I'll be calling home just a few months following graduation. Often described as the cultural, financial, media, and entertainment capital of the world, the Big Apple can be overwhelming to just about anyone. So when it comes to looking for my first apartment, I want to make the most informed decision as I can."),
                          br(),
                          h3("Data Focus"), 
                          p(" The purpose of this project is to explore gather I gathered from NYC Open Data on www.nyc.gov and streeteasy.com . I use focus on two datasets: Manhattan Rental Inventory and Manhattan Median Asking Price, both of which date back from January 2010 until October 2018."),
                          br(),
                          h3("What does this app let you do?"),
                          p("This app lets you explore the varying availability and price within a handful of neighborhoods within Manhattan, NYC. I decided to focus on Manhattan at large because that is where my office is located. While I know that I want to focus my efforts on Manhattan, what lies within its border is fair game. I have little idea of what neighborhoods within Manhattan are recommended for graduates like me and I'm excited to begin the process. Join me and be sure to record any and all feedback in the Takeaways tab! Thank you!")
                        )
                      )),
# The second tab on my app is Availability            
             tabPanel(
               "Availability",
               tags$style(type = 'text/css', 
                          'body {padding-top: 70px;}',
                          HTML(nav_bar_html)
               ),
# The title for the Availability tab             
              titlePanel("Rental Availability by Area"),
              br(),
# Subtitle describing what I'm analyzing
              p("Analysis of the number of rentable units in Manhattan, New York City from January 2010 until October 2018."),
# What does my analysis show
              br(),
              p("In this tab we have provided the ability for experimentation on the data by anchoring on a specific area within Manhattan. The value here is to be able to compare unit availability/area popularity over the last 8 years to understand similarities and differences between the different areas."),
# Step by step instructions for the user 
              br(),
              HTML(paste('<b style="font-size:22px">Instructions:</b>', 
                         '1. Using the tabs (highlighted in snowflake blue), select a neighborhood that you would like to take a closer look at.', 
                         '2. Using the drop-down menu, select which year(s) you would like to examine. All years have automatically been selected for you.',
                         '3. If you would like, check the box "Add a linear model" to be able to see the trends more clearly.',
                         '4. Be sure to toggle between tabs to compare and contrast the different neighborhoods of Manhattan.',
                         sep="<br/>")),
               
               hr(),
               
               # Sidebar layout with input and output definitions
      sidebarLayout(
        # Sidebar panel for interactive input
        sidebarPanel(
          # Describing how the data is already broken up by area within Manhattan for user simplicity
          tags$b("The data is for the borough of Manhattan and is filtered by the top neighborhoods within it. If you'd like to explore the data further, see the links below."),
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
# letting the user explore datasets more closely with clickable links to my github
          h4('See Code:'),
          HTML(paste('<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/lower_eastMI.csv" target="_blank">Lower East Side</a>', 
                     '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/chelseaMI.csv" target="_blank">Chelsea</a>', 
                     '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/sohoMI.csv" target="_blank">Soho</a>', 
                     '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/fidiMI.csv" target="_blank">Financial District</a>',
                     '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/eastvillageMI.csv" target="_blank">East Village</a>',
                     '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/westvillageMI.csv" target="_blank">West Village</a>',
                     '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/gramercyMI.csv" target="_blank">Gramercy Park</a>',
                     sep="<br/>"))
          ),
        
          mainPanel(
# Output: Tabset w/ plots for the popular neighborhoods within Manhattan, as recommended by my brother, a East Village native for 2 years
            tabsetPanel(type = "tabs",
                        tabPanel("Lower East Side", plotOutput("plot_lowereastMI")),
                        tabPanel("Chelsea", plotOutput("plot_chelseaMI")),
                        tabPanel("Soho", plotOutput("plot_sohoMI")), 
                        tabPanel("Financial District", plotOutput("plot_fidiMI")), 
                        tabPanel("East Village", plotOutput("plot_eastvillageMI")),
                        tabPanel("West Village", plotOutput("plot_westvillageMI")),
                        tabPanel("Gramercy Park", plotOutput("plot_gramercyMI"))
                        
                        
                     )
                   )
                 )
               ),
# The third tab on my app is called Price  
# Layout is very similar to the Availability tab
              tabPanel(
                "Price",
                tags$style(type = 'text/css', 
                           'body {padding-top: 70px;}',
                           HTML(nav_bar_html)), 
                titlePanel("The Price is Right...or Is It?"), 
                br(),
                p("Analysis of median asking rent price for in Manhattan, New York City from January 2010 until October 2018."), 
                br(),
                p("In this tab we have provided the ability for experimentation on the data by anchoring on a specific neighborhood within Manhattan. The value here is to be able to get a better idea for what an apartment goes for between a selection of neighborhoods in Manhattan and to be able to compare/contract their price trends over the last 8 years."),
                br(),
                HTML(paste('<b style="font-size:22px">Instructions:</b>', 
                           '1. Using the tabs (highlighted in snowflake blue), select a neighborhood that you would like to take a closer look at.', 
                           '2. Using the drop-down menu, select which year(s) you would like to examine. All years have automatically been selected for you.',
                           '3. If you would like, check the box "Add a linear model" to be able to see the trends more clearly.',
                           '4. Be sure to toggle between tabs to compare and contrast the different neighborhoods of Manhattan.',
                           sep="<br/>")),
                hr(),
                
                # Sidebar layout with input and output definitions
                    sidebarLayout(
                  # Sidebar panel for interactive input
                  sidebarPanel(
                    # Describing how the data is already broken up by area within Manhattan for user simplicity
                    tags$b("The data is for the borough of Manhattan and is filtered by the top neighborhoods within it. If you'd like to explore the data further, see the links below."),
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
                    checkboxInput("line2", label = "Add linear model"),
                    hr(),
                    h4('See Code:'),
                    HTML(paste('<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/lower_eastP.csv" target="_blank">Lower East Side</a>', 
                               '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/chelseaP.csv" target="_blank">Chelsea</a>', 
                               '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/sohoP.csv" target="_blank">Soho</a>', 
                               '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/fidiP.csv" target="_blank">Financial District</a>',
                               '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/east_villageP.csv" target="_blank">East Village</a>',
                               '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/west_villageP.csv" target="_blank">West Village</a>',
                               '<a href="https://github.com/hollyc2019/TacklingManhattanApp/blob/master/gramercyP.csv" target="_blank">Gramercy Park</a>',
                               sep="<br/>"))
                  ),
                  
                  mainPanel(
                    # Output: Tabset w/ plots for the popular neighborhoods within Manhattan
                    tabsetPanel(type = "tabs",
                                tabPanel("Lower East Side", plotOutput("plot_lowereastP")),
                                tabPanel("Chelsea", plotOutput("plot_chelseaP")),
                                tabPanel("Soho", plotOutput("plot_sohoP")), 
                                tabPanel("Financial District", plotOutput("plot_fidiP")), 
                                tabPanel("East Village", plotOutput("plot_eastvillageP")),
                                tabPanel("West Village", plotOutput("plot_westvillageP")),
                                tabPanel("Gramercy Park", plotOutput("plot_gramercyP")))))),
tabPanel("Takeaways",
         fluidPage(
           fluidRow(
             h3("We did it!"),
             br(),
             p("Thank you for joining me on this journey to find an apartment, or at least the start of it!"),
             br(),
             h3("What have I learned?"),
             p("Through making this app I have really come to appreciate the accessibility that Shiny offers 
               and how easy it can be to create a presentable website once you know the basics. Completing 
               this project was not an easy task by any means and it involved countless hours of trial and 
               error, but I'm proud of the way it came out and I hope to expand on it later on/create new 
               interactive apps in the future. On of the biggest struggles I had with this project was the
               data itself. NYC is a big city with a HUGE amount of public data up for grabs. I had to narrow 
               my focus and not take on too much so that my project could be succinct. At first I downloaded 
               5 datasets, but then narrowed it to the two in order to focus on the critical aspects of 
               Manhattan supply and demand: availability and price. Given the number of neighborhoods I wanted 
               to explore, focusing on these two datasets was crucial to me completing a well-designed project
               with helpful insights. Another challenge I faced was attempted to recreate zillow.com with a map 
               of NYC and a pin of each and every available apartment within a 5 mile radius of my office. 
               This was a lofty goal for a project and I soon realized that after facing barriers with Google 
               API and a messy dataset that I had to forgo my interactive map idea. Afterall, Zillow has a 
               whole team of developers working on their website at once, as a new coder I was no match. But, 
               I'm very happy with what I was able to create in the end and I think it is a great first-look 
               into my apartment search. One of the insights a user could deduce from my analysis is in the availability tab.
                I realized that over the years, the amount of rentable units typically surge in the summer months. When availability is high, that helps
               drive down price because the supply has increased. This is great news for me because after I 
               graduate in May I'll be looking to sign a lease shortly thereafter."), 
             br(),
             h3("What next?"),
             br(),
             p("I would love to hear your thoughts!"),
             HTML(paste('<a href="https://goo.gl/forms/nmdvgowMeiQUk8C73" target="_blank">Evaluation</a>', sep="<br/>"))
           )
         ))
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
  
  output$plot_lowereastMI <- renderPlot({
    # filter data for selected years
    plot_lowereastMI <- reactive({
      plot_lowereastMI <- lower_eastMI[lower_eastMI$year %in% input$year, ]
  })
  
  # use if statement to create different plots for with and without linear model selection
  if(input$line == TRUE) {
    ggplot(data = plot_lowereastMI(), aes_string(x = "month", y = "units", color = "year")) +
      geom_point() +
      geom_smooth(method=loess) +
      labs(x = "Month",
           y = "Units",
           title = "Availability of Rental Units Over the Last 8 Years",
           subtitle = "Helpful analysis for deciding when to buy")
  }
  else{
    ggplot(data = plot_lowereastMI(), aes_string(x = "month", y = "units", color = "year")) +
      geom_point() +
      labs(x = "Month",
           y = "Units",
           title = "Availability of Rental Units Over the Last 8 Years",
           subtitle = "Helpful analysis for deciding when to buy")
  }
})
  
  output$plot_chelseaMI <- renderPlot({
    # filter data for selected years
    plot_chelseaMI <- reactive({
      plot_chelseaMI <- chelseaMI[chelseaMI$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line == TRUE) {
      ggplot(data = plot_chelseaMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
    else{
      ggplot(data = plot_chelseaMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
  })
  
  output$plot_sohoMI <- renderPlot({
    # filter data for selected years
    plot_sohoMI <- reactive({
      plot_sohoMI <- sohoMI[sohoMI$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line == TRUE) {
      ggplot(data = plot_sohoMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
    else{
      ggplot(data = plot_sohoMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
  })
  
  output$plot_fidiMI <- renderPlot({
    # filter data for selected years
    plot_fidiMI <- reactive({
      plot_fidiMI <- fidiMI[fidiMI$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line == TRUE) {
      ggplot(data = plot_fidiMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
    else{
      ggplot(data = plot_fidiMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
  })
  
  output$plot_eastvillageMI <- renderPlot({
    # filter data for selected years
    plot_eastvillageMI <- reactive({
      plot_eastvillageMI <- eastvillageMI[eastvillageMI$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line == TRUE) {
      ggplot(data = plot_eastvillageMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
    else{
      ggplot(data = plot_eastvillageMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
  })
  
  output$plot_westvillageMI <- renderPlot({
    # filter data for selected years
    plot_westvillageMI <- reactive({
      plot_westvillageMI <- westvillageMI[westvillageMI$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line == TRUE) {
      ggplot(data = plot_westvillageMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
    else{
      ggplot(data = plot_westvillageMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
  })
  
  output$plot_gramercyMI <- renderPlot({
    # filter data for selected years
    plot_gramercyMI <- reactive({
      plot_gramercyMI <- gramercyMI[gramercyMI$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line == TRUE) {
      ggplot(data = plot_gramercyMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
    else{
      ggplot(data = plot_gramercyMI(), aes_string(x = "month", y = "units", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Units",
             title = "Availability of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for deciding when to buy")
    }
  })
  
# Repeating the same process but for the price output plots
  output$plot_lowereastP <- renderPlot({
    # filter data for selected years
    plot_lowereastP <- reactive({
      plot_lowereastP <- lower_eastP[lower_eastP$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line2 == TRUE) {
      ggplot(data = plot_lowereastP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
    else{
      ggplot(data = plot_lowereastP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
  })
  
  output$plot_chelseaP <- renderPlot({
    # filter data for selected years
    plot_chelseaP <- reactive({
      plot_chelseaP <- chelseaP[chelseaP$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line2 == TRUE) {
      ggplot(data = plot_chelseaP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
    else{
      ggplot(data = plot_chelseaP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
  })
  
  output$plot_sohoP <- renderPlot({
    # filter data for selected years
    plot_sohoP <- reactive({
      plot_sohoP <- sohoP[sohoP$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line2 == TRUE) {
      ggplot(data = plot_sohoP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
    else{
      ggplot(data = plot_sohoP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
  })
  
  
  output$plot_fidiP <- renderPlot({
    # filter data for selected years
    plot_fidiP <- reactive({
      plot_fidiP <- FiDiP[FiDiP$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line2 == TRUE) {
      ggplot(data = plot_fidiP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
    else{
      ggplot(data = plot_fidiP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
  })
  
  output$plot_eastvillageP <- renderPlot({
    # filter data for selected years
    plot_eastvillageP <- reactive({
      plot_eastvillageP <- east_villageP[east_villageP$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line2 == TRUE) {
      ggplot(data = plot_eastvillageP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
    else{
      ggplot(data = plot_eastvillageP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
  })
  
  output$plot_westvillageP <- renderPlot({
    # filter data for selected years
    plot_westvillageP <- reactive({
      plot_westvillageP <- west_villageP[west_villageP$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line2 == TRUE) {
      ggplot(data = plot_westvillageP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
    else{
      ggplot(data = plot_westvillageP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
  })
  
  output$plot_gramercyP <- renderPlot({
    # filter data for selected years
    plot_gramercyP <- reactive({
      plot_gramercyP <- gramercyP[gramercyP$year %in% input$year, ]
    })
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line2 == TRUE) {
      ggplot(data = plot_gramercyP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        geom_smooth(method=loess) +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
    else{
      ggplot(data = plot_gramercyP(), aes_string(x = "month", y = "asking_price", color = "year")) +
        geom_point() +
        labs(x = "Month",
             y = "Price",
             title = "Median Asking Price of Rental Units Over the Last 8 Years",
             subtitle = "Helpful analysis for seeing what neighborhood fits your budget")
    }
  })
}

    
shinyApp(ui = ui, server = server)

