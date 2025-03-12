library(shiny)
library(rio)  # For easy data import
library(dplyr)
library(ggplot2)  # For plotting
library(shinythemes)
library(tidyverse)
library(bslib) # For enhanced UI
library(shinyWidgets) # added install.packages("shinyWidgets")
library(shinyjs) # For show/hide elements

# **1. Data Preparation (Modify this to load your actual time-series data)**

# Create a dummy time series data (replace this with your actual data loading)
set.seed(123) # for reproducibility
years <- 2008:2012
epi_data_list <- list()

for (year in years) {
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
    useShinyjs(), # for virtual select, show/hide elements
    # Dynamic Theme
    uiOutput("dynamic_theme"),
    titlePanel("Global EPI Explorer"),  # Set the main title
    
    layout_sidebar(
      sidebar = sidebar(
        width = "280px",  # Adjust sidebar width as needed (increased width)
        actionButton("show_indicator_modal", "Select Indicator"),  # Button to trigger the modal
        sliderInput("epi_range", "EPI Score Range:",
                    min = min(epi_data_all$EPI, na.rm = TRUE),
                    max = max(epi_data_all$EPI, na.rm = TRUE),
                    value = c(min(epi_data_all$EPI, na.rm = TRUE), max(epi_data_all$EPI, na.rm = TRUE))),
        selectInput("region", "Choose a region:",
                    choices = c("All", levels(epi_data_all$EPI_regions)), selected = "All"), # Changed epi_data to epi_data_all
        selectInput("plot_theme", "Choose Plot Theme:",
                    choices = c("Classic", "Minimal", "Dark")), # Added theme selection
        uiOutput("year_slider"), # Dynamic year slider (rendered in server)
        
        # NEW: Multi-select widget for countries
        pickerInput(
          inputId = "countries",
          label = "Select Countries:",
          choices = unique(epi_data_all$Country),
          options = list(`actions-box` = TRUE),
          multiple = TRUE,
          selected = unique(epi_data_all$Country)  # Initially select all
        ),
        
        # Add the prettyCheckbox
        prettyCheckbox(
          inputId = "Id023",
          label = "Filter High Population",
          value = FALSE,  # Starts unchecked
          status = "success",
          fill = TRUE
        ),
        # Add the checkboxGroupButtons
        checkboxGroupButtons( # or radioGroupButtons
          inputId = "id",
          label = "Choice: ",
          choices = c("A", "B", "C")
        ),
        
        # Select menu
        pickerInput(
          inputId = "month_picker",
          label = "Select:",
          choices = month.name,
          options = pickerOptions(
            actionsBox = TRUE,
            size = 10,
            selectedTextFormat = "count > 3"
          ),
          multiple = TRUE
        ),
        
        # Virtual select
        virtualSelectInput(
          inputId = "season_picker",
          label = "Select Season:",
          choices = list(
            "Spring" = c("March", "April", "May"),
            "Summer" = c("June", "July", "August"),
            "Autumn" = c("September", "October", "November"),
            "Winter" = c("December", "January", "February")
          ),
          showValueAsTags = TRUE,
          search = TRUE,
          multiple = TRUE
        ),
        
        # Dark Mode Toggle
        switchInput(
          inputId = "dark_mode",
          label = "Dark Mode",
          value = FALSE
        ),
        
        # Theme Color Chooser
        selectInput(
          inputId = "theme_color",
          label = "Theme Color:",
          choices = c("superhero", "flatly", "sandstone", "united", "cosmo", "lumen", "simplex", "yeti"),
          selected = "superhero"
        ),
        
        hr(),  # Add a horizontal rule for visual separation
        tags$p("Data Source: [NASA_EPI]", style = "font-size: 80%"), # Add a data source acknowledgement
        tags$p("App by: [Fariba]", style = "font-size: 80%")  # Add a creator acknowledgement
        
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box("Average EPI", textOutput("avg_epi"), showcase = "chart-line"), # changed icon
        value_box("Highest EPI", textOutput("max_epi"), showcase = "trophy"),   # changed icon
        value_box("Number of Countries", textOutput("country_count"), showcase = "flag") # changed icon
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
      ),
      # Hidden input to store selected indicator
      tags$div(id = "selected_indicator", style = "display:none;")
    )
  )

# Define server logic -----------------------------------------------------
server <- function(input, output, session) {
  
  # Dynamic Theme (bslib)
  output$dynamic_theme <- renderUI({
    bs_theme(bootswatch = input$theme_color, version = 5)
  })
  
  # **Observe the prettyCheckbox Input**
  observeEvent(input$Id023, {
    if (input$Id023) {
      print("Checkbox is checked: Filtering High Population")
      # Add code here to do something when the checkbox is checked
    } else {
      print("Checkbox is unchecked: No High Population Filter")
      # Add code here to do something when it's unchecked
    }
  })
  
  # **Observe checkboxGroupButtons input**
  observeEvent(input$id, {
    print(paste("Selected choices:", paste(input$id, collapse = ", ")))
    # Add code here to do something with the selected choices from the checkboxGroupButtons
  })
  
  # **Observe pickerInput (month_picker) input**
  observeEvent(input$month_picker, {
    print(paste("Selected months:", paste(input$month_picker, collapse = ", ")))
    # Add code here to do something with the selected months
  })
  
  # **Observe virtualSelectInput (season_picker) input**
  observeEvent(input$season_picker, {
    print(paste("Selected seasons/months:", paste(input$season_picker, collapse = ", ")))
    # Add code here to do something with the selected seasons/months
  })
  
  # **Modal Dialog for Indicator Selection**
  observeEvent(input$show_indicator_modal, {
    showModal(modalDialog(
      title = "Choose an Indicator",
      selectInput("modal_indicator", "Indicator:",
                  choices = c("EPI", "ENVHEALTH", "ECOSYSTEM", "GDP_capita", "Population2005", "DALY_SC")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_indicator", "Confirm")
      )
    ))
  })
  
  # **Update selected_indicator when confirmed**
  observeEvent(input$confirm_indicator, {
    updateTextInput(session, "selected_indicator", value = input$modal_indicator)
    removeModal()
  })
  
  # Reactive expression to get the selected indicator (from the hidden input)
  selected_indicator <- reactive({
    req(input$selected_indicator)
    input$selected_indicator
  })
  
  # Reactive expression for filtered data (for main plots)
  filtered_data <- reactive({
    data <- epi_data_all %>% filter(Year == input$year) # Filter by year
    
    if (input$region != "All") {
      data <- data %>% filter(EPI_regions == input$region)
    }
    
    data <- data %>%
      filter(EPI >= input$epi_range[1] & EPI <= input$epi_range[2])
    
    # NEW: Filter by selected countries
    data <- data %>%
      filter(Country %in% input$countries)
    
    # NEW: Additional filter based on checkbox
    if (input$Id023) {
      # Checkbox is checked: Filter for countries with population > 1 million
      data <- data %>% filter(Population2005 > 1000000)  # Or whatever criteria you want
    }
    
    data # Return the filtered data
  })
  
  # Theme choice based on user input (for plots)
  theme_choice <- reactive({
    switch(input$plot_theme,
           "Classic" = theme_classic(),
           "Minimal" = theme_minimal(),
           "Dark" = theme_dark())
  })
  
  # **Dynamic Year Slider**
  output$year_slider <- renderUI({
    sliderInput("year", "Select Year:",
                min = min(epi_data_all$Year, na.rm = TRUE),
                max = max(epi_data_all$Year, na.rm = TRUE),
                value = min(epi_data_all$Year, na.rm = TRUE),
                step = 1,
                sep = "")
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
    req(filtered_data(), selected_indicator()) # Ensure data and indicator are available
    
    ggplot(filtered_data(), aes(x = Country, y = .data[[selected_indicator()]])) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = paste(selected_indicator(), "by Country"),
           x = "Country",
           y = selected_indicator()) +
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
    req(selected_indicator()) # Ensure indicator is selected
    
    # Group by year and calculate the average indicator value
    timeline_data <- epi_data_all %>%
      group_by(Year) %>%
      summarize(avg_indicator = mean(.data[[selected_indicator()]], na.rm = TRUE))
    
    ggplot(timeline_data, aes(x = Year, y = avg_indicator)) +
      geom_line(color = "blue") +
      geom_point(color = "blue") +
      labs(title = paste("Average", selected_indicator(), "Over Time"),
           x = "Year",
           y = selected_indicator()) +
      theme_minimal() + theme_choice()
  })
}

# Run the application -----------------------------------------------------
shinyApp(ui, server)