library(shiny)
library(rio)  # For easy data import
library(dplyr)
library(ggplot2)  # For plotting
library(shinythemes)
library(tidyverse)
library(bslib) # For enhanced UI

# **1. Data Preparation (Modify this to load your actual time-series data)**

# Create a dummy time series data (replace this with your actual data loading)
set.seed(123) # for reproducibility
years <- 2008:2012
epi_data_list <- list()

for (year in years) {
  # Create a dummy dataset for each year, based on the original epi_data
  epi_data <- import("data/2008-epi-all-countries-winsorization.csv") # Or read from your sources
  epi_data <- epi_data[, !duplicated(names(epi_data))]
  epi_data <- epi_data %>%
    mutate(Population2005 = as.numeric(Population2005)) %>%
    filter(!is.na(Population2005)) %>%
    arrange(desc(Population2005)) %>%
    head(10)
  epi_data <- epi_data %>%
    mutate(Population2005 = as.numeric(Population2005),
           EPI = as.numeric(EPI) + rnorm(nrow(epi_data), 0, 2), # Add some random variation for each year
           ENVHEALTH = as.numeric(ENVHEALTH) + rnorm(nrow(epi_data), 0, 1),
           ECOSYSTEM = as.numeric(ECOSYSTEM) + rnorm(nrow(epi_data), 0, 1)) %>%
    mutate(Year = year) # Add a year column
  epi_data_list[[as.character(year)]] <- epi_data # Store the dataframe
}
epi_data_all <- bind_rows(epi_data_list) # combine all years' data
epi_data_all <- epi_data_all %>%
  mutate(Year = as.integer(Year)) # Ensure Year is integer

# Define UI ---------------------------------------------------------------
ui <-
  page_fluid(
    theme = bs_theme(bootswatch = "journal"), # Apply a Bootswatch theme
    titlePanel("Global EPI Explorer"),  # Set the main title
    
    layout_sidebar(
      sidebar = sidebar(
        width = "250px",  # Adjust sidebar width as needed
        selectInput("indicator", "Choose an Indicator:",
                    choices = c("EPI", "ENVHEALTH", "ECOSYSTEM", "GDP_capita", "Population2005", "DALY_SC")),
        sliderInput("epi_range", "EPI Score Range:",
                    min = min(epi_data_all$EPI, na.rm = TRUE),
                    max = max(epi_data_all$EPI, na.rm = TRUE),
                    value = c(min(epi_data_all$EPI, na.rm = TRUE), max(epi_data_all$EPI, na.rm = TRUE))),
        selectInput("region", "Choose a region:",
                    choices = c("All", levels(epi_data_all$EPI_regions)), selected = "All"), # Changed epi_data to epi_data_all
        selectInput("plot_theme", "Choose Plot Theme:",
                    choices = c("Classic", "Minimal", "Dark")), # Added theme selection
        sliderInput("year", "Select Year:", # Added year slider
                    min = min(epi_data_all$Year, na.rm = TRUE),
                    max = max(epi_data_all$Year, na.rm = TRUE),
                    value = min(epi_data_all$Year, na.rm = TRUE),
                    step = 1,
                    sep = "")
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box("Average EPI", textOutput("avg_epi")),
        value_box("Highest EPI", textOutput("max_epi")),
        value_box("Number of Countries", textOutput("country_count"))
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Bar Chart"),
          full_screen = TRUE,
          card_body(plotOutput("epi_plot"))
        ),
        card(
          card_header("Scatter Plot (EPI vs. Resources)"),
          full_screen = TRUE,
          card_body(plotOutput("scatter_plot1"))
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Boxplot (ENVHEALTH)"),
          full_screen = TRUE,
          card_body(plotOutput("boxplot_envhealth"))
        ),
        card(
          card_header("Region Distribution"),
          full_screen = TRUE,
          card_body(plotOutput("pie_chart"))
        )
      ),
      card(
        card_header("EPI Over Time"),
        full_screen = TRUE,
        card_body(plotOutput("timeline_plot"))
      )
      
    )
  )

# Define server logic -----------------------------------------------------
server <- function(input, output) {
  
  # Reactive expression for filtered data (for main plots)
  filtered_data <- reactive({
    data <- epi_data_all %>% filter(Year == input$year) # Filter by year
    
    if (input$region != "All") {
      data <- data %>% filter(EPI_regions == input$region)
    }
    
    data <- data %>%
      filter(EPI >= input$epi_range[1] & EPI <= input$epi_range[2])
    
    data # Return the filtered data
  })
  
  # Theme choice based on user input
  theme_choice <- reactive({
    switch(input$plot_theme,
           "Classic" = theme_classic(),
           "Minimal" = theme_minimal(),
           "Dark" = theme_dark())
  })
  
  
  # Value Box outputs
  output$avg_epi <- renderText({
    req(filtered_data()) # Ensure data is available
    paste(round(mean(filtered_data()$EPI, na.rm = TRUE), 2))
  })
  
  output$max_epi <- renderText({
    req(filtered_data())
    paste(round(max(filtered_data()$EPI, na.rm = TRUE), 2))
  })
  
  output$country_count <- renderText({
    req(filtered_data())
    paste(nrow(filtered_data()))
  })
  
  
  # Create the bar chart
  output$epi_plot <- renderPlot({
    req(filtered_data()) # Ensure data is available
    
    ggplot(filtered_data(), aes(x = Country, y = .data[[input$indicator]])) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = paste(input$indicator, "by Country"),
           x = "Country",
           y = input$indicator) +
      theme_minimal() + theme_choice()  # Apply selected theme
  })
  
  # Create Scatter Plot 1 (EPI vs. PRODUCTIVE_NATURAL_RESOURCES)
  output$scatter_plot1 <- renderPlot({
    req(filtered_data())
    
    ggplot(filtered_data(), aes(x = EPI, y = PRODUCTIVE_NATURAL_RESOURCES)) +
      geom_point(aes(size = Population2005), color = "darkgreen", alpha = 0.7) + #Population for sizing
      labs(title = "EPI vs. PRODUCTIVE_NATURAL_RESOURCES",
           x = "EPI Score",
           y = "PRODUCTIVE_NATURAL_RESOURCES",
           size = "Population") +
      theme_minimal() + theme_choice()
  })
  
  # Create Boxplot (ENVHEALTH)
  output$boxplot_envhealth <- renderPlot({
    req(filtered_data())
    
    ggplot(filtered_data(), aes(y = ENVHEALTH)) +
      geom_boxplot(fill = "orange", color = "black") +
      labs(title = "Boxplot of Environmental Health",
           y = "Environmental Health") +
      theme_minimal() + theme_choice()
  })
  
  # Create Pie Chart (Distribution of EPI Regions)
  output$pie_chart <- renderPlot({
    req(filtered_data())
    
    region_counts <- filtered_data() %>%
      group_by(EPI_regions) %>%
      summarize(count = n())
    
    ggplot(region_counts, aes(x = "", y = count, fill = EPI_regions)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Distribution of EPI Regions") +
      theme_void() + theme_choice()
  })
  
  # Timeline plot
  output$timeline_plot <- renderPlot({
    req(input$indicator) # Ensure an indicator is selected
    
    # Group by year and calculate the average indicator value
    timeline_data <- epi_data_all %>%
      group_by(Year) %>%
      summarize(avg_indicator = mean(.data[[input$indicator]], na.rm = TRUE))
    
    ggplot(timeline_data, aes(x = Year, y = avg_indicator)) +
      geom_line(color = "blue") +
      geom_point(color = "blue") +
      labs(title = paste("Average", input$indicator, "Over Time"),
           x = "Year",
           y = input$indicator) +
      theme_minimal() + theme_choice()
  })
}

# Run the application -----------------------------------------------------
shinyApp(ui, server)