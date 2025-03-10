install.packages("shiny")
install.packages("rio")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("shinythemes")
install.packages("tidyverse")
install.packages("leaflet")
install.packages("tidyverse")




library(shiny)
library(rio)  # For easy data import
library(dplyr)
library(ggplot2)  # For plotting
library(shinythemes)
library(tidyverse)



# 1. Load the data
epi_data <- import("data/2008-epi-all-countries-winsorization.csv")
str(epi_data)

# Remove duplicate columns (keeping the first occurrence)
epi_data <- epi_data[, !duplicated(names(epi_data))]

# Ensure Country column exists
if(!"Country" %in% names(epi_data)){
  stop("Error: 'Country' column not found in the data.")
}

# Keep only the top 10 countries by population (or whatever criteria)
epi_data <- epi_data %>%
  mutate(Population2005 = as.numeric(Population2005)) %>%
  filter(!is.na(Population2005)) %>%
  arrange(desc(Population2005)) %>%
  head(10)

# Convert Population2005 back to numeric after filtering
epi_data <- epi_data %>%
  mutate(Population2005 = as.numeric(Population2005),
         EPI = as.numeric(EPI),
         ENVHEALTH = as.numeric(ENVHEALTH),
         ECOSYSTEM = as.numeric(ECOSYSTEM))

# Define UI
ui <- fluidPage(
  theme = shinytheme("simplex"),
  titlePanel("Global EPI Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Choose an Indicator:",
                  choices = c("EPI", "ENVHEALTH", "ECOSYSTEM", "GDP_capita", "Population2005", "DALY_SC")),
      sliderInput("epi_range", "EPI Score Range:",
                  min = min(epi_data$EPI, na.rm = TRUE),
                  max = max(epi_data$EPI, na.rm = TRUE),
                  value = c(min(epi_data$EPI, na.rm = TRUE), max(epi_data$EPI, na.rm = TRUE))),
      selectInput("region", "Choose a region:",
                  choices = c("All", levels(epi_data$EPI_regions)), selected = "All")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Chart", plotOutput("epi_plot")),  # Bar chart
        tabPanel("Scatter Plot 1", plotOutput("scatter_plot1")), # New scatter plot tab
        tabPanel("Scatter Plot 2", plotOutput("scatter_plot2")), # New scatter plot tab
        tabPanel("Pie Chart", plotOutput("pie_chart"))    # New pie chart tab
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Filtered data based on user input
  filtered_data <- reactive({
    data <- epi_data
    if (input$region != "All") {
      data <- data %>% filter(EPI_regions == input$region)
    }
    data %>%
      filter(EPI >= input$epi_range[1] & EPI <= input$epi_range[2])
  })

  # Create the bar chart
  output$epi_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Country, y = .data[[input$indicator]])) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = paste(input$indicator, "by Country"),
           x = "Country",
           y = input$indicator) +
      theme_minimal()
  })

  # Create Scatter Plot 1 (EPI vs. PRODUCTIVE_NATURAL_RESOURCES)
  output$scatter_plot1 <- renderPlot({
    ggplot(filtered_data(), aes(x = EPI, y = PRODUCTIVE_NATURAL_RESOURCES)) +
      geom_point(aes(size = Population2005), color = "darkgreen", alpha = 0.7) + #Population for sizing
      labs(title = "EPI vs. PRODUCTIVE_NATURAL_RESOURCES",
           x = "EPI Score",
           y = "PRODUCTIVE_NATURAL_RESOURCES",
           size = "Population") +
      theme_minimal()
  })

  # Create Scatter Plot 2 (ENVHEALTH vs. ECOSYSTEM)
  output$scatter_plot2 <- renderPlot({
    ggplot(filtered_data(), aes(x = ENVHEALTH, y = ECOSYSTEM)) +
      geom_point(aes(size = Population2005), color = "purple", alpha = 0.7) + #Population for sizing
      labs(title = "Environmental Health vs. Ecosystem Vitality",
           x = "Environmental Health",
           y = "Ecosystem Vitality",
           size = "Population") +
      theme_minimal()
  })

  # Create Pie Chart (Distribution of EPI Regions)
  output$pie_chart <- renderPlot({
    region_counts <- filtered_data() %>%
      group_by(EPI_regions) %>%
      summarize(count = n())

    ggplot(region_counts, aes(x = "", y = count, fill = EPI_regions)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Distribution of EPI Regions") +
      theme_void()
  })
}

# Run the app
shinyApp(ui = ui, server = server)