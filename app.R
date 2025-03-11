library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Ensure data files exist before loading
load_data <- function(year) {
  file_path <- paste0("data/", year, "-epi-all-countries-winsorization.csv")
  if (file.exists(file_path)) {
    epi_data <- read.csv(file_path)
    epi_data <- epi_data[, !duplicated(names(epi_data))]
    epi_data <- epi_data %>%
      mutate(Population2005 = as.numeric(Population2005),
             EPI = as.numeric(EPI),
             ENVHEALTH = as.numeric(ENVHEALTH),
             ECOSYSTEM = as.numeric(ECOSYSTEM),
             Year = as.integer(year)) %>%
      filter(!is.na(Population2005))
    return(epi_data)
  } else {
    return(NULL)
  }
}

# Load and combine data
years <- 2008:2012
epi_data_list <- lapply(years, load_data)
epi_data_all <- bind_rows(epi_data_list)

epi_data_all <- epi_data_all %>%
  filter(!is.na(EPI))

# Define UI
ui <- fluidPage(
  titlePanel("Global EPI Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("epi_range", "EPI Score Range:",
                  min = min(epi_data_all$EPI, na.rm = TRUE),
                  max = max(epi_data_all$EPI, na.rm = TRUE),
                  value = c(min(epi_data_all$EPI, na.rm = TRUE), max(epi_data_all$EPI, na.rm = TRUE))),
      selectInput("plot_type", "Select Visualization:",
                  choices = c("Histogram", "Scatter Plot", "Bar Chart", "Pie Chart")),
      uiOutput("year_slider")
    ),
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$year_slider <- renderUI({
    sliderInput("year", "Select Year:",
                min = min(epi_data_all$Year, na.rm = TRUE),
                max = max(epi_data_all$Year, na.rm = TRUE),
                value = min(epi_data_all$Year, na.rm = TRUE),
                step = 1)
  })
  
  filtered_data <- reactive({
    epi_data_all %>%
      filter(Year == input$year & EPI >= input$epi_range[1] & EPI <= input$epi_range[2])
  })
  
  output$main_plot <- renderPlot({
    data <- filtered_data()
    
    if (input$plot_type == "Histogram") {
      ggplot(data, aes(x = EPI)) +
        geom_histogram(fill = "steelblue", bins = 20) +
        labs(title = "Histogram of EPI Scores", x = "EPI Score", y = "Frequency") +
        theme_minimal()
    } else if (input$plot_type == "Scatter Plot") {
      ggplot(data, aes(x = ENVHEALTH, y = ECOSYSTEM)) +
        geom_point(aes(size = Population2005), color = "darkgreen", alpha = 0.7) +
        labs(title = "Environmental Health vs. Ecosystem", x = "Environmental Health", y = "Ecosystem Score") +
        theme_minimal()
    } else if (input$plot_type == "Bar Chart") {
      ggplot(data, aes(x = reorder(Country, -EPI), y = EPI)) +
        geom_bar(stat = "identity", fill = "orange") +
        coord_flip() +
        labs(title = "EPI Scores by Country", x = "Country", y = "EPI Score") +
        theme_minimal()
    } else if (input$plot_type == "Pie Chart") {
      region_counts <- data %>%
        group_by(EPI_regions) %>%
        summarize(count = n())
      ggplot(region_counts, aes(x = "", y = count, fill = EPI_regions)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Distribution of EPI Regions") +
        theme_void()
    }
  })
}

# Run the application
shinyApp(ui, server)
