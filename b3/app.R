library(shiny)
library(dplyr)
library(DT)

# load data
us_accident_data <- read.csv("./us_accident.csv")

# Extract year information and map state names
us_accident_data <- us_accident_data %>%
  mutate(
    Year = as.numeric(format(as.Date(Start_Time), "%Y")),
    State_Name = ifelse(State == "DC", "District of Columbia",
                        state.name[match(State, state.abb)])
  )
ui <- fluidPage(
  ## Feature 1: I add some beautiful font styles and some colors by CSS, making the UI look nicer for users.
  includeCSS("www/style.css"),
  titlePanel("US Accidents Exploration"),
  sidebarLayout(
    sidebarPanel(
      ## Feature 2: A slider (description in server)
      sliderInput(
        inputId = "year_range",
        label = "Select the range of years",
        min = min(us_accident_data$Year, na.rm = TRUE),
        max = max(us_accident_data$Year, na.rm = TRUE),
        value = c(min(us_accident_data$Year, na.rm = TRUE), max(us_accident_data$Year, na.rm = TRUE))
      ),
      ## Feature 3: Download the table (description in server)
      downloadButton(
        outputId = "download_table",
        label = "Download as a .csv file"
      ),
      ## Feature 4: The image I added highlights the topics to be researched in this app for users: accidents, also makes the app look nicer.
      div(
        img(src = "dataset-cover.jpg", style = "width: 100%; margin-top: 20px; border-radius: 10px;")
      )
    ),

    mainPanel(
      ## Feature 5: Show the number of results found when the filters change (description in server)
      textOutput("result_count"),
      ## Feature 6:Interactive data table (description in server)
      DT::dataTableOutput("accident_table")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  ## Feature 2: The slider allows filter accident data based on a specific range of years,
  ## helping users focus on data from the desired time period, which is useful for identifying trends over time.
  filtered_data <- reactive({
    us_accident_data %>%
      filter(
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      ) %>%
      group_by(State, State_Name) %>%
      summarise(Total_Accidents = n(), .groups = "drop") %>%
      arrange(State_Name)
  })
  ## Feature 3: The download button enables users to export the accident data of states to a .csv file that they filtered,
  ## making it easier to perform following specific analysis according to the file.
  output$download_table <- downloadHandler(
    filename = function() {
      paste("accident_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  ## Feature 5: The text output dynamically updates to display the total number of accidents during the specified year range,
  ## providing users with a quick summary of the number of accidents based on their selected filters.
  output$result_count <- renderText({
    data <- filtered_data()
    year_range <- input$year_range
    paste( "The total number of accidents is", sum(data$Total_Accidents), "betwwen",year_range[1], "and", year_range[2])
  })
  ## Feature 6: The interactive table allows users to sort and browse the filtered accident data conveniently within the application,
  ## making it more convenient to explore and understand the dataset.
  output$accident_table <- renderDataTable({
    datatable(filtered_data(), options = list(pageLength = 10))
  })

}

shinyApp(ui = ui, server = server)
