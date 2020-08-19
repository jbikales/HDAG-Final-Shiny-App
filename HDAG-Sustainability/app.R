library(shiny)
library(tidyverse)
library(readr)
library(plotly)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("yeti"), "HDAG Sustainability Thinktank",
        tabPanel("Mobility vs AQI",
                 sidebarLayout(
                     sidebarPanel(
                         h4("Interactive Map"),
                         p("Toggle your desired county"),
                         selectInput(inputId = "county",
                                     label = "County",
                                     choices = list("Denver County, CO" = 1, 
                                                    "New York County, NY" = 2, 
                                                    "Hennepin County, MN" = 3, 
                                                    "Hawaii County, HI" = 4, 
                                                    "Suffolk County, MA" = 5,
                                                    "San Juan County, NM" = 6,
                                                    "San Joaquin County, CA" = 7), 
                                                    selected = 1)),
                     
                     mainPanel(
                       h3(textOutput("map_title")),
                       plotlyOutput("graphs"),
                       h6("Sources: ", a(href = 'https://www.google.com/covid19/mobility/', 'Google Mobility Data', .noWS = "outside"), " and ",
                          a(href = 'https://www.epa.gov/outdoor-air-quality-data/air-quality-index-daily-values-report', 
                            'Air Quality Index Daily Values Report', .noWS = "outside")),
                       p("The mobility data ", a(href = 'https://www.google.com/covid19/mobility/', 'from Google', .noWS = "outside"), 
                         " compares the amount of time people are spending in the workplace compared to a median value for that day of 
                           week from the 5‑week baseline period Jan 3 – Feb 6, 2020. The AQI data compares the AQI (based on the highest level
                           polluntant that day) on each day to a baseline of the median AQI in that area for the same period (Feb 16-July 31) 
                           in 2019.")
    )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$graphs <- renderPlotly({
        
       denver <- read_rds("clean-data/denver.rds")
       
       new_york <- read_rds("clean-data/new_york.rds")
       
       hennepin <- read_rds("clean-data/hennepin.rds")
       
      hawaii <- read_rds("clean-data/hawaii.rds")
       
      suffolk <- read_rds("clean-data/suffolk.rds")
       
     san_juan <- read_rds("clean-data/san_juan.rds")
      
    san_joaquin <- read_rds("clean-data/san_joaquin.rds")
        
        combined_data <- switch(input$county,
                             '1' = denver,
                             '2' = new_york,
                             '3' = hennepin,
                             '4' = hawaii,
                             '5' = suffolk,
                             '6' = san_juan,
                             '7' = san_joaquin)  

        plot <- ggplot(data = combined_data, 
                       aes(x = id, y = workplaces_percent_change_from_baseline,
                       text = paste("County:", sub_region_2, "<br>",
                                     "Day from Feb. 16:", id, "<br>",
                                     "% Change Mobility:", workplaces_percent_change_from_baseline, "<br>",
                                    "% Change AQI:", value))) +
            geom_point(color = "#00BFC4") +
            geom_point(aes(x = id, y = value), color = "#F8766D") +
            ylab("Percent Change from Baseline") +
            xlab("Day (from Feb 16, 2020)") 
            
        ggplotly(plot, tooltip = c("text"))
        
    })
    
    output$map_title <- renderText ({
      paste(case_when(input$county == '1' ~ "Denver County, CO",
                      input$county == '2' ~ "New York County, NY",
                      input$county == '3' ~ "Hennepin County, MN",
                      input$county == '4' ~ "Hawaii County, HI",
                      input$county == '5' ~ "Suffolk County, MA",
                      input$county == '6' ~ "San Juan County, NM",
                      input$county == '7' ~ "San Joaquin County, CA"),
            "Mobility vs AQI")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
