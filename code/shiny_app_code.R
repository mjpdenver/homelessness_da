# Load required libraries
library(shiny)
library(leaflet)
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)


places <- read_csv("places.csv", show_col_types = FALSE)

# Define the UI
ui <- navbarPage("Denver Traffic Accidents",
                 
                 # Tab 1: Text Content
                 tabPanel("Intro",
                          fluidPage(
                              h1("Denver Traffic Accidents"),
                              h2("by Matthew Pocernich"),
                              h2("Feb 12, 2025"),
                              h2("Data Source"),
                              tags$a(
                                  href = "https://opendata-geospatialdenver.hub.arcgis.com/?mode=yoxjflyvfgfj&tag=traffic-accidents", 
                                  "Denver Open Data", 
                                  target = "_blank"  # Opens the link in a new tab
                              ),
                              p("Source: Denver Open Data
                                ")
                          )
                 ),
                 
                 # Tab 2: Map with filtering controls
                 tabPanel("Map",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      h3("Filter Markers"),
                                      # Add a checkbox to bypass filtering and show all records
                                      checkboxGroupInput("filterCategories", "Select Categories:",
                                                         choices = unique(places$category),
                                                         selected = unique(places$category)),
                                      dateRangeInput("dateRange", "Select Date Range:",
                                                     start = min(places$date),
                                                     end = max(places$date),
                                                     min = min(places$date),
                                                     max = max(places$date))
                                      #dateInput("displayDate", "Select Date:",
                                      #          value = min(places$date),
                                      #          min = min(places$date),
                                      #          max = max(places$date))
                                  ),
                                  mainPanel(
                                      leafletOutput("map", height = 500)
                                  )
                              )
                          )
                 ),
                 
                 # Tab 3: Time Series Plot
                 tabPanel("Time Series",
                          fluidPage(
                              selectInput("aggPeriod", "Aggregation Period", 
                                          choices = c("Week", "Month"), 
                                          selected = "Week"),
                              plotOutput("timeSeriesPlot", height = 500)
                          )
                 ),
                 # Tab 1: Text Content
                 tabPanel("To-Do List",
                          fluidPage(
                              h1(""),
                              h2("by Matthew Pocernich"),
                              h2("My Bulleted List"),
                              tags$ul(
                                  tags$li("First bullet point"),
                                  tags$li("Second bullet point"),
                                  tags$li("Third bullet point")
                              ),
                              h2("Feb 12, 2025"),
                              p("Source: Denver Open Data
                                ")
                          )
                 )
)

# Define the server logic
server <- function(input, output, session) {
    
    # Create a reactive expression that filters the places data based on user input.
    # If "Display All Records" is checked, bypass the filters.
    filteredPlaces <- reactive({
        #if (isTRUE(input$displayAll)) {
        #    return(places)
        #}
        
        data <- places
        # Filter by category if any are selected
        if (!is.null(input$filterCategories) && length(input$filterCategories) > 0) {
            data <- data[data$category %in% input$filterCategories, ]
        }
        # Filter by the selected single date
        if (!is.null(input$dateRange)) {
            data <- data[data$date >= input$dateRange[1] & data$date <= input$dateRange[2], ]
        }
        if (nrow(data) > 50) {
            data <- head(data, 50)
        }
        data
    })
    
    # Render the base leaflet map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addMarkers(data = filteredPlaces(),  # or yourData if you prefer
                       lng = ~lng, 
                       lat = ~lat, 
                       popup = ~paste("<strong>", name, "</strong><br>",
                                      "Category:", category, "<br>",
                                      "Date:", date)) %>%
            setView(lng = -105.0, lat = 39.75, zoom = 12)
    })
    
    # Update the map markers whenever the filtered data changes
    observe({
        leafletProxy("map", data = filteredPlaces()) %>%
            clearMarkers() %>%
            addMarkers(lng = ~lng, lat = ~lat, 
                       popup = ~paste("<strong>", name, "</strong><br>",
                                      "Category:", category, "<br>",
                                      "Date:", date))
        
    })
    

    # Render the time series plot aggregated by week and color coded by category
    output$timeSeriesPlot <- renderPlot({
        aggUnit <- if (input$aggPeriod == "Week") "week" else "month"
        print(aggUnit)
        # Group the data by week (floor_date rounds down to the start of the week) and category
        df_summary <- places %>%
            group_by(period = floor_date(date, unit = aggUnit), category) %>%
            summarize(count = n(), .groups = "drop")
        print("ASDF")
        print(head(df_summary))
        # Create the time series plot with ggplot2
        ggplot(df_summary, aes(x = period, y = count, color = category)) +
            geom_line(size = 1) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
            labs(title = "Weekly Time Series Plot by Category",
                 x = "Week",
                 y = "Count") +
            theme_minimal() +
            theme(legend.title = element_blank())
    })
}

# Run the application
shinyApp(ui, server)
