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
                              h3("by Matthew Pocernich"),
                              h3("Feb 12, 2025"),
                              p("Sample Dashboard with Visualizations"),
                              tags$img(src = "i70-crash.jpeg", width = "500px"),
                              p("  "),
                              tags$a(
                              href = "https://opendata-geospatialdenver.hub.arcgis.com/?mode=yoxjflyvfgfj&tag=traffic-accidents", 
                                  "Source:Denver Open Data", 
                                  target = "_blank"  # Opens the link in a new tab
                              )
                          )
                 ),
                 
                 # Tab 2: Map with filtering controls
                 tabPanel("Location of reported incidents",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      #h3("Filter Markers"),
                                      # Add a checkbox to bypass filtering and show all records
                                      checkboxGroupInput("filterCategories", "Select Categories:",
                                                         choices = unique(places$category),
                                                         selected = unique(places$category)),
                                      dateRangeInput("dateRange", "Select Date Range:",
                                                     start = as.Date("2024-01-01"),
                                                     end = as.Date("2024-04-01"),
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
                              # New input for selecting categories
                              checkboxGroupInput("tsCategories", "Select Categories:",
                                                 choices = unique(places$category),
                                                 selected = unique(places$category)),
                              selectInput("aggPeriod", "Aggregation Period", 
                                          choices = c("Week", "Month"), 
                                          selected = "Week"),
                              plotOutput("timeSeriesPlot", height = 500)
                          )
                 ),
                 # Tab 1: Text Content
                 # Add this new tabPanel to your navbarPage, for example after the "Time Series" tab
                 tabPanel("Summary",
                          fluidPage(
                              checkboxGroupInput("summaryCategories", "Select Categories:",
                                                 choices = unique(places$category),
                                                 selected = unique(places$category)),
                              selectInput("aggType", "Summarize Counts By:",
                                          choices = c("Day of Week", "Month of Year"),
                                          selected = "Day of Week"),
                              plotOutput("summaryPlot", height = 500)
                          )
                 ),
                 tabPanel("To-Do List",
                          fluidPage(
                              h2("To Do List "),
                              p("With such a rich dataset, many features may be added and tasks remain to be completed."),
                              tags$ul(
                                  tags$li("Learn more about the data source.  Between 2018 and 2019 there is a significant drop in reported accidents.  Determine if this is caused by a change in the collection process."),
                                  tags$li("Integrate with API to automatically update data on a daily cadence."),
                                  tags$li("Provide summary statistics of crashes by vehicle type"),
                                  tags$li("Integrate external datasource such as weather and vehicle type registrations in the metro area."),
                                  tags$li("Add heatmap to accident locations.  There are too many points to represent clearly. "),
                                  tags$li("Remove Incomplete Time Periods in timeseries"),
                                  tags$li("Create Ratio Plot - Months"),
                                  tags$li("Add baseline values to figures to help user identify what is unusual.")
                              ),
                              
                              
                          )
                 )
                 
                 
)

# Define the server logic
server <- function(input, output, session) {
    
    # Create a reactive expression that filters the places data based on user input.
    # If "Display All Records" is checked, bypass the filters.
    # Reactive expression for the full dataset (for the heatmap)
    filteredPlacesAll <- reactive({
        data <- places
        if (!is.null(input$filterCategories) && length(input$filterCategories) > 0) {
            data <- data[data$category %in% input$filterCategories, ]
        }
        if (!is.null(input$dateRange)) {
            data <- data[data$date >= input$dateRange[1] & data$date <= input$dateRange[2], ]
        }
        data
    })
    
    # Reactive expression for a limited set of data (for markers)
    filteredPlacesLimited <- reactive({
        data <- filteredPlacesAll()
        if(nrow(data) > 50) {
            data <- head(data, 50)  # or use sample_n(data, 50) for a random selection
        }
        data
    })
    
    # Render the base leaflet map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            # Add the heatmap layer using the full filtered data
            addHeatmap(
                data = filteredPlacesAll(),
                lng = ~lng,
                lat = ~lat,
                radius = 15,   # Adjust as needed
                blur = 20,     # Adjust as needed
                max = 0.05     # Adjust as needed
            ) %>%
            # Add a markers layer using only the limited dataset
            addMarkers(
                data = filteredPlacesLimited(),
                lng = ~lng, 
                lat = ~lat, 
                popup = ~paste("<strong>", name, "</strong><br>",
                               "Category:", category, "<br>",
                               "Date:", date)
            ) %>%
            setView(lng = -105.0, lat = 39.75, zoom = 12.5)
    })    
    observe({
        leafletProxy("map", data = filteredPlacesLimited()) %>%
            clearMarkers() %>%
            addMarkers(
                lng = ~lng,
                lat = ~lat,
                popup = ~paste("<strong>", name, "</strong><br>",
                               "Category:", category, "<br>",
                               "Date:", date)
            )
    })
    

    # Render the time series plot aggregated by week and color coded by category
    output$timeSeriesPlot <- renderPlot({
        aggUnit <- if (input$aggPeriod == "Week") "week" else "month"
        
        df_summary <- places %>%
            # Filter by the selected categories for the time series plot
            filter(category %in% input$tsCategories) %>%
            group_by(period = floor_date(date, unit = aggUnit), category) %>%
            summarize(count = n(), .groups = "drop")
        
        ggplot(df_summary, aes(x = period, y = count, color = category)) +
            geom_line(size = 1) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
            labs(title = "Time Series Plot by Category",
                 x = "Period",
                 y = "Count") +
            theme_minimal() +
            theme(legend.title = element_blank())
    })
    output$summaryPlot <- renderPlot({
        # Filter places by selected categories from the summary tab
        filteredData <- places %>% 
            filter(category %in% input$summaryCategories)
        
        if (input$aggType == "Day of Week") {
            df_summary <- filteredData %>%
                mutate(day = wday(date, label = TRUE, week_start = 1)) %>% 
                group_by(day, category) %>%
                summarize(count = n(), .groups = "drop")
            
            ggplot(df_summary, aes(x = day, y = count, fill = category)) +
                geom_bar(stat = "identity", position = "dodge") +
                labs(title = "Accidents by Day of Week and Category", 
                     x = "Day of Week", y = "Count") +
                theme_minimal()
        } else {
            df_summary <- filteredData %>%
                mutate(month = month(date, label = TRUE)) %>% 
                group_by(month, category) %>%
                summarize(count = n(), .groups = "drop")
            
            ggplot(df_summary, aes(x = month, y = count, fill = category)) +
                geom_bar(stat = "identity", position = "dodge") +
                labs(title = "Accidents by Month of Year and Category", 
                     x = "Month", y = "Count") +
                theme_minimal()
        }
    })
    
    
    
    
}

# Run the application
shinyApp(ui, server)
