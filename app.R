library(shiny)
library(rio)        # For easy data import
library(dplyr)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(bslib)       # Enhanced UI
library(shinyWidgets) # Enhanced input widgets
library(shinyjs)      # Show/hide UI elements

# **1. Data Preparation**

# Store the file path
data_file <- "data/2008-epi-all-countries-winsorization.csv"

# Function to load and process data
load_epi_data <- function(file_path) {
  years <- 2008:2012
  epi_data_list <- list()
  
  for (year in years) {
    epi_data <- import(file_path)
    epi_data <- epi_data[, !duplicated(names(epi_data))]
    epi_data <- epi_data %>%
      mutate(Population2005 = as.numeric(Population2005)) %>%
      filter(!is.na(Population2005)) %>%
      arrange(desc(Population2005)) %>%
      head(10)
    epi_data <- epi_data %>%
      mutate(Population2005 = as.numeric(Population2005),
             EPI = as.numeric(EPI) + rnorm(nrow(epi_data), 0, 2),
             ENVHEALTH = as.numeric(ENVHEALTH) + rnorm(nrow(epi_data), 0, 1),
             ECOSYSTEM = as.numeric(ECOSYSTEM) + rnorm(nrow(epi_data), 0, 1)) %>%
      mutate(Year = year)
    epi_data_list[[as.character(year)]] <- epi_data
  }
  epi_data_all <- bind_rows(epi_data_list)
  epi_data_all <- epi_data_all %>%
    mutate(Year = as.integer(Year))
  return(epi_data_all)
}

# Load data initially
epi_data_all <- load_epi_data(data_file)

# Initialize filtered data with the full dataset
epi_data_all_filtered <- epi_data_all


# Define UI ---------------------------------------------------------------
ui <-
  page_fluid(
    useShinyjs(), # Required for shinyjs
    theme = bs_theme(bootswatch = "superhero"), # Apply a Bootswatch theme
    
    titlePanel("Global EPI Explorer"),  # Set the main title
    
    layout_sidebar(
      sidebar = sidebar(
        width = "280px",  # Adjust sidebar width
        actionButton("show_indicator_modal", "Select Indicator"),  # Modal button
        
        # Pretty Checkbox to Show/Hide EPI Range
        prettyCheckbox(
          inputId = "show_epi_range",
          label = "Show EPI Range Slider",
          value = TRUE,  # Starts checked
          status = "success",
          fill = TRUE
        ),
        
        # Slider Input (initially visible)
        sliderInput("epi_range", "EPI Score Range:",
                    min = min(epi_data_all$EPI, na.rm = TRUE),
                    max = max(epi_data_all$EPI, na.rm = TRUE),
                    value = c(min(epi_data_all$EPI, na.rm = TRUE), max(epi_data_all$EPI, na.rm = TRUE))),
        
        selectInput("region", "Choose a region:",
                    choices = c("All", levels(epi_data_all$EPI_regions)), selected = "All"),
        selectInput("plot_theme", "Choose Plot Theme:",
                    choices = c("Classic", "Minimal", "Dark")),
        uiOutput("year_slider"), # Dynamic year slider
        
        # Multi-select widget for countries
        pickerInput(
          inputId = "countries",
          label = "Select Countries:",
          choices = unique(epi_data_all$Country),
          options = list(`actions-box` = TRUE),
          multiple = TRUE,
          selected = unique(epi_data_all$Country)  # Initially select all
        ),
        
        # Checkbox group for categories
        checkboxGroupButtons(
          inputId = "category_selection",
          label = "Category Selection:",
          choices = c("Category A", "Category B", "Category C") # Example categories
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
        
        # **Add the actionBttn**
        actionBttn(
          inputId = "update_data",  # Unique ID for the button
          label = "Update Data",   # Text on the button
          style = "material-flat", # Style of the button
          color = "primary"      # Color of the button
        ),
        
        hr(),
        tags$p("Data Source: [NASA_EPI]", style = "font-size: 80%"),
        tags$p("App by: [Fariba]", style = "font-size: 80%")
        
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box("Average EPI", textOutput("avg_epi"), showcase = "chart-line"),
        value_box("Highest EPI", textOutput("max_epi"), showcase = "trophy"),
        value_box("Number of Countries", textOutput("country_count"), showcase = "flag")
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
  
  # **Observe the prettyCheckbox Input**
  observeEvent(input$show_epi_range, {
    if (input$show_epi_range) {
      shinyjs::show("epi_range")
    } else {
      shinyjs::hide("epi_range")
    }
  })
  
  # **Action Button Logic (actionBttn)**
  observeEvent(input$update_data, {
    showNotification("Data Updated!", type = "message")  # Display a notification
    
    epi_data_all <<- load_epi_data(data_file)  # Reload data
    
    # Update UI Elements: Slider, Countries, and Regions
    updateSliderInput(session, "epi_range",
                      min = min(epi_data_all$EPI, na.rm = TRUE),
                      max = max(epi_data_all$EPI, na.rm = TRUE),
                      value = c(min(epi_data_all$EPI, na.rm = TRUE), max(epi_data_all$EPI, na.rm =TRUE)))
    updatePickerInput(session, "countries",
                      choices = unique(epi_data_all$Country),
                      selected = unique(epi_data_all$Country))
    updateSelectInput(session, "region",
                      choices = c("All", levels(epi_data_all$EPI_regions)), selected = "All")
  })
  
  # **Observe checkboxGroupButtons input**
  observeEvent(input$category_selection, {
    # Filter data based on checkbox group selection
    if (!is.null(input$category_selection)) { # Check if something is selected
      epi_data_all_filtered <<- epi_data_all %>%
        filter(Category %in% input$category_selection)  # Replace Category if there exists.
    } else {
      epi_data_all_filtered <<- epi_data_all
    }
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
    data <- epi_data_all_filtered %>% filter(Year == input$year) # Use filtered dataset
    
    if (input$region != "All") {
      data <- data %>% filter(EPI_regions == input$region)
    }
    
    if (input$show_epi_range) {
      data <- data %>%
        filter(EPI >= input$epi_range[1] & EPI <= input$epi_range[2])
    }
    
    data <- data %>%
      filter(Country %in% input$countries)
    
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