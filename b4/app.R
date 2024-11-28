# Load packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(DT)
library(ggplot2)
library(shinyWidgets) # Make my widgets beautiful!
library(lubridate) # Date format
library(paletteer) # For beautiful colors!


# Load dataset
us_accident_data <- read.csv("us_accident.csv")
# Only select 30,000 rows
us_accident_data <- sample_n(us_accident_data,30000,replace = FALSE)

# Preprocessing
us_accident_data <- us_accident_data %>%
  mutate(
    # Convert to date format
    Start_Time = as.POSIXct(Start_Time, format = "%Y-%m-%d %H:%M:%S"),
    Year = year(Start_Time),
    Severity = as.factor(Severity),
    # Get DC for the map and table
    State_Name = ifelse(State == "DC", "District of Columbia",
                        state.name[match(State, state.abb)]),
    Latitude = as.numeric(Start_Lat),
    Longitude = as.numeric(Start_Lng)
  )

# Get all state names and remove AK and HI
states_df <- data.frame(
  # Add DC
  State = c(state.abb, "DC"),
  State_Name = c(state.name, "District of Columbia")
) %>%
  filter(!(State %in% c("AK", "HI")))

# Get weather types
weather_conditions <- unique(us_accident_data$Weather_Condition)

# UI
ui <- dashboardPage(
  skin = 'green',
  dashboardHeader(title = "USA Accidents"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About Dataset", tabName = "info", icon = icon("circle-info")),
      menuItem("How to Use the App", tabName = "usage", icon = icon("circle-user")),
      menuItem("Accidents Map", tabName = "state", icon = icon("globe")),
      menuItem("Potential Factors Analysis", tabName = "factors", icon = icon("magnifying-glass")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("thumbtack"))
    )
  ),
  
  dashboardBody(
    chooseSliderSkin("Flat", color = "green"),
    # Change head font using CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "title.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "tab.css")
    ),
    tabItems(
      # Intro to dataset
      tabItem(tabName = "info",
              fluidRow(
                column(width = 12,
                       h2("Dataset Information"),
                       includeHTML("www/dataintro.Rhtml")
                )
              )
      ),
      
      # How to use it
      tabItem(tabName = "usage",
              fluidRow(
                column(width = 12,
                       h2("App Instruction"),
                       includeHTML("www/instruction.Rhtml")
                )
              )
      ),
      
      # Accidents Map
      tabItem(tabName = "state",
              fluidRow(
                # Left part
                column(width = 6,
                       box(
                         title = "Accident Data Table By States",
                         status = "success",
                         width = 12,
                         textOutput("result_count"),
                         DT::dataTableOutput("accident_table")
                       ),
                       tags$div(
                         style = "margin-top: 10px;",
                         img(src = "car_crash.png", height = "225px", width = "100%")
                       )
                ),
                column(width = 6,
                       # By severity
                       selectInput("severity_part", "Select Severity of Accident:", 
                                   choices = c("All", "1", "2", "3", "4"), 
                                   selected = "All"),
                       # By year
                       sliderInput("year_part", "Select Year Range:",
                                   # chooseSliderSkin("Modern"),
                                   min = min(us_accident_data$Year, na.rm = TRUE),
                                   max = max(us_accident_data$Year, na.rm = TRUE),
                                   value = c(min(us_accident_data$Year, na.rm = TRUE), max(us_accident_data$Year, na.rm = TRUE)),
                                   sep = ""),
                       # By state
                       selectInput("state_part", "Select State:",
                                   choices = c("All" = "All", states_df$State),
                                   selected = "All"),
                       # By day or night
                       selectInput("day_night_part", "Day or Night:",
                                   choices = c("All", "Day", "Night"),
                                   selected = "All"),
                       # Download
                       downloadButton("download_part", "Download as CSV"),
                       # Map
                       leafletOutput("map_part", height = "400px")
                )
              )
      ),
      
      # Potential Factors Analysis
      tabItem(tabName = "factors",
              fluidRow(
                box(
                  title = "Potential Factors",
                  status = "success",
                  width = 12,
                  # Three child tabs
                  tabBox(
                    title = "Factors",
                    id = "factors_tabbox",
                    height = "800px",
                    width = 12,
                    # Weather
                    tabPanel("Weather",
                             h3("Weather Factors"),
                             # Filters
                             fluidRow(
                               box(
                                 status = "success",
                                 width = 12,
                                 fluidRow(
                                   # By year
                                   column(4,
                                          sliderInput("weather_year_range", "Select Year Range:",
                                                      min = min(us_accident_data$Year, na.rm = TRUE),
                                                      max = max(us_accident_data$Year, na.rm = TRUE),
                                                      value = c(min(us_accident_data$Year, na.rm = TRUE), max(us_accident_data$Year, na.rm = TRUE)),
                                                      sep = "")
                                   ),
                                   # By state
                                   column(4,
                                          selectInput("weather_state", "Select State:",
                                                      choices = c("All" = "All", states_df$State),
                                                      selected = "All")
                                   ),
                                   # By day or night
                                   column(4,
                                          selectInput("weather_day_night", "Day or Night:",
                                                      choices = c("All", "Day", "Night"),
                                                      selected = "All")
                                   )
                                 )
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Bar Plot",
                                 status = "success",
                                 width = 12,
                                 plotOutput("weather_plot_filtered")
                               )
                             ),
                    ),
                    
                    # Environment
                    tabPanel("Environment",
                             fluidRow(
                               box(
                                 status = "success",
                                 width = 12,
                                 fluidRow(
                                   # By year
                                   column(3,
                                          sliderInput("f_env_year_range", "Select Year Range:",
                                                      min = min(us_accident_data$Year, na.rm = TRUE),
                                                      max = max(us_accident_data$Year, na.rm = TRUE),
                                                      value = c(min(us_accident_data$Year, na.rm = TRUE), max(us_accident_data$Year, na.rm = TRUE)),
                                                      sep = "")
                                   ),
                                   # By state
                                   column(3,
                                          selectInput("f_env_state", "Select State:",
                                                      choices = c("All" = "All", states_df$State),
                                                      selected = "All")
                                   ),
                                   # By day or night
                                   column(3,
                                          selectInput("f_env_day_night", "Day or Night:",
                                                      choices = c("All", "Day", "Night"),
                                                      selected = "All")
                                   ),
                                   # By weather
                                   column(3,
                                          selectInput("f_env_weather", "Select Weather Condition:",
                                                      choices = c("All", weather_conditions),
                                                      selected = "All")
                                   )
                                 )
                               )
                             ),
                             h3("Environment Factors"),
                             # Plot part
                             fluidRow(
                               box(
                                 status = "success",
                                 width = 12,
                                 fluidRow(
                                   column(4,
                                          selectInput("selected_variable", "Select Variable to Plot:",
                                                      choices = c("Temperature (F)" = "Temperature.F.",
                                                                  "Humidity" = "Humidity...",
                                                                  "Pressure (in.)" = "Pressure.in.",
                                                                  "Visibility (mi.)" = "Visibility.mi."),
                                                      selected = "Temperature.F.")
                                   ),
                                   # Choose color
                                   column(4,
                                          selectInput("selected_color", "Choose your favorite color!",
                                                      choices = c("Blue" = "#AFC7E8", "Pink" = "#FF9896",
                                                                  "Green" = "#99CC99", "Orange" = "#F09148",
                                                                   "Brown" = "#C59D94"),
                                                      selected = "#99CC99")
                                   ),
                                   # Curves
                                   column(4,
                                          awesomeCheckbox(
                                            inputId = "show_normal",
                                            label = "Display Normal Distribution Curve",
                                            value = TRUE,
                                            status = "success",
                                          ),
                                          awesomeCheckbox(
                                            inputId = "show_density",
                                            label = "Display Density Curves",
                                            value = TRUE,
                                            status = "success",
                                          )
                                   )
                                 )
                               )
                             ),
                             fluidRow(
                               # Histogram
                               box(
                                 title = "Histogram",
                                 status = "success",
                                 width = 12,
                                 plotOutput("selected_plot")
                               )
                             )
                    ),
                    
                    # Time
                    tabPanel("Time",
                             fluidRow(
                               box(
                                 status = "success",
                                 width = 12,
                                 fluidRow(
                                   # By year
                                   column(3,
                                          sliderInput("f_time_year_range", "Select Year Range:",
                                                      min = min(us_accident_data$Year, na.rm = TRUE),
                                                      max = max(us_accident_data$Year, na.rm = TRUE),
                                                      value = c(min(us_accident_data$Year, na.rm = TRUE), max(us_accident_data$Year, na.rm = TRUE)),
                                                      sep = "")
                                   ),
                                   # By state
                                   column(3,
                                          selectInput("f_time_state", "Select State:",
                                                      choices = c("All" = "All", states_df$State),
                                                      selected = "All")
                                   ),
                                   # By day or night
                                   column(3,
                                          selectInput("f_time_day_night", "Day or Night:",
                                                      choices = c("All", "Day", "Night"),
                                                      selected = "All")
                                   ),
                                   # By weather
                                   column(3,
                                          selectInput("f_time_weather", "Select Weather Condition:",
                                                      choices = c("All", weather_conditions),
                                                      selected = "All")
                                   )
                                 )
                               )
                             ),
                             h3("Time Factors"),
                             # Plot part
                             fluidRow(
                               column(width = 12,
                                      prettyRadioButtons(inputId = "time_period", label = "Select Time Period:",
                                                   choices = c("Monthly" = "month", "Hourly" = "hour"),
                                                   selected = "month",
                                                   status = "success")
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Line Plot",
                                 status = "success",
                                 width = 12,
                                 plotOutput("accidents_time_plot")
                               )
                             )
                    )
                  )
                )
              )
      ),
      
      # Conclusion
      tabItem(tabName = "conclusion",
              fluidRow(
                column(width = 12,
                       h2("Conclusion"),
                       includeHTML("www/conclusion.Rhtml")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive: filter data for map
  filtered_data <- reactive({
    us_accident_data %>%
      filter(
        Year >= input$year_part[1],
        Year <= input$year_part[2],
        if (input$severity_part != "All") Severity == input$severity_part else TRUE,
        if (input$state_part != "All") State == input$state_part else TRUE,
        if (input$day_night_part != "All") Sunrise_Sunset == input$day_night_part else TRUE
      )
  })
  
  # Reactive: filter data for table
  summarized_data <- reactive({
    filtered_data() %>%
      group_by(State, State_Name) %>%
      summarise(Total_Accidents = n(), .groups = "drop") %>%
      arrange(State_Name)
  })
  
  # Reactive: total accidents calculation
  total_accidents_text <- reactive({
    data <- summarized_data()
    year_range <- input$year_part
    paste("The total number of accidents is", sum(data$Total_Accidents), "between", year_range[1], "and", year_range[2])
  })
  
  # Leaflet map
  output$map_part <- renderLeaflet({
    data <- filtered_data()
    # Create it
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>% # USA
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste(
          "<strong>Severity:</strong>", Severity, "<br>",
          "<strong>State:</strong>", State_Name, "<br>",
          "<strong>Year:</strong>", Year, "<br>",
          "<strong>Start Time:</strong>", Start_Time
        ),
        clusterOptions = markerClusterOptions()
      ) 
  })
  
  # Reactive: for download
  download_data_part <- reactive({
    summarized_data()
  })
  
  # Download as .csv
  output$download_part <- downloadHandler(
    filename = function() {
      paste("accident_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(download_data_part(), file, row.names = FALSE)
    }
  )
  
  # RenderText for total accidents
  output$result_count <- renderText({
    total_accidents_text()
  })
  
  # RenderDynamicTable for accidents
  output$accident_table <- renderDT({
    datatable(summarized_data(), options = list(pageLength = 10))
  })
  
  # Weather plot
  output$weather_plot_filtered <- renderPlot({
    tryCatch({
      # Filter
      filtered_weather_data <- us_accident_data %>%
        filter(
          Year >= input$weather_year_range[1],
          Year <= input$weather_year_range[2]
        )
      
      if (input$weather_state != "All") {
        filtered_weather_data <- filtered_weather_data %>% filter(State %in% input$weather_state)
      }
      
      if (input$weather_day_night != "All") {
        filtered_weather_data <- filtered_weather_data %>% filter(Sunrise_Sunset == input$weather_day_night)
      }
      
      # Only select the top 9 most weather conditions
      weather_counts <- filtered_weather_data %>%
        filter(Weather_Condition != "") %>%
        group_by(Weather_Condition) %>%
        summarise(total_count = n(), .groups = "drop") %>%
        arrange(desc(total_count)) %>%
        slice_head(n = 9) # 选择事故最多的前9种
      
      # Weather conditions after filtering
      top_weather_conditions <- weather_counts$Weather_Condition
      
      # 进一步过滤数据，仅保留前9种天气状况
      final_weather_data <- filtered_weather_data %>%
        filter(Weather_Condition %in% top_weather_conditions) %>%
        group_by(Weather_Condition, Severity) %>%
        summarise(count = n(), .groups = "drop") %>%
        left_join(weather_counts, by = "Weather_Condition") %>% # 添加 total_count
        group_by(Weather_Condition) %>%
        mutate(percent = count / sum(count)) %>%
        ungroup()
      
      # Choose the color I like!
      color_scheme <- paletteer_dynamic("cartography::green.pal", 5)
      # Apply to the severity meanwhile
      names(color_scheme) <- levels(us_accident_data$Severity)
      
      # Bar plot for weather
      ggplot(final_weather_data, aes(x = reorder(Weather_Condition, -total_count), y = percent, fill = Severity)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = color_scheme) +
        labs(
          title = "Proportion of Accident Severity under Top 9 Weather Conditions",
          x = "Weather Condition",
          y = "Proportion",
          fill = "Severity"
        ) +
        coord_flip() +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5))
    })
  })
  
  # Environment data filtering
  filtered_environment_data <- reactive({
    us_accident_data %>%
      filter(
        Year >= input$f_env_year_range[1],
        Year <= input$f_env_year_range[2],
        if (input$f_env_state != "All") State == input$f_env_state else TRUE,
        if (input$f_env_day_night != "All") Sunrise_Sunset == input$f_env_day_night else TRUE,
        if (input$f_env_weather != "All") Weather_Condition == input$f_env_weather else TRUE
      )
  })
  
  # Environment histogram
  output$selected_plot <- renderPlot({
    selected_var <- input$selected_variable
    selected_color <- input$selected_color
    show_normal <- input$show_normal
    show_density <- input$show_density
    data <- filtered_environment_data()
    
    # Create it
    p <- ggplot(data, aes(x = .data[[selected_var]])) +
      geom_histogram(aes(y = after_stat(density)), bins = 80, fill = selected_color, color = "white")
    
    # Add curves
    if (show_normal) {
      p <- p + stat_function(fun = dnorm,
                             args = list(mean = mean(data[[selected_var]], na.rm = TRUE),
                                         sd = sd(data[[selected_var]], na.rm = TRUE)),
                             linewidth = 1, color = "red")
    }
    
    if (show_density) {
      p <- p + geom_density(color = "blue", linewidth = 1)
    }
    # Add title and x/y
    p <- p +
      labs(
        title = paste(selected_var, "Distribution"),
        x = selected_var,
        y = "Density"
      ) +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5) )
    # Show it
    p
  })
  
  # Time data after filtering
  filtered_time_data <- reactive({
    us_accident_data %>%
      filter(
        Year >= input$f_time_year_range[1],
        Year <= input$f_time_year_range[2],
        if (input$f_time_state != "All") State == input$f_time_state else TRUE,
        if (input$f_time_day_night != "All") Sunrise_Sunset == input$f_time_day_night else TRUE,
        if (input$f_time_weather != "All") Weather_Condition == input$f_time_weather else TRUE
      ) %>%
      mutate(
        hour = hour(Start_Time),
        month_num = month(Start_Time),
        month = month(Start_Time, label = TRUE, abbr = FALSE)
      )
  })
  
  # Reactive: time aggregated data
  aggregated_time_data <- reactive({
    if (input$time_period == "hour") {
      filtered_time_data() %>%
        group_by(hour) %>%
        summarise(count = n(), .groups = "drop")
    } else if (input$time_period == "month") {
      filtered_time_data() %>%
        group_by(month_num, month) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(month_num)
    }
  })
  
  # Time plot
  output$accidents_time_plot <- renderPlot({
    data <- aggregated_time_data()
    if (input$time_period == "hour") {
      ggplot(data, aes(x = hour, y = count)) +
        geom_line(color = "#93CC82", linewidth = 1, group = 1) +
        geom_point(color = "#93CC82", size = 3) +
        geom_area(fill = "pink", alpha = 0.7) +
        scale_x_continuous(breaks = 0:23, name = "Hour") +
        labs(title = "Accidents by Hour", x = "Hour", y = "Number of Accidents") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (input$time_period == "month") {
      ggplot(data, aes(x = month_num, y = count)) +
        geom_line(color = "#93CC82", linewidth = 1, group = 1) +
        geom_point(color = "#93CC82", size = 3) +
        geom_area(fill = "pink", alpha = 0.7) +
        scale_x_continuous(
          breaks = 1:12,
          labels = month.name,
          name = "Month"
        ) +
        labs(title = "Accidents by Month", x = "Month", y = "Number of Accidents") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
}

# App
shinyApp(ui = ui, server = server)