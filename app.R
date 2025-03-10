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

# Remove duplicate columns 
epi_data <- epi_data[, !duplicated(names(epi_data))]




#i want to show the EPI for the 10 countries that they had the most epi
# Keep only the top 10 countries by population
epi_data <- epi_data %>%
  mutate(Population2005 = as.numeric(Population2005)) %>%  # Convert to numeric
  filter(!is.na(Population2005)) %>%  # Remove rows with NA in Population2005
  arrange(desc(Population2005)) %>%
  head(10)
print(head(epi_data))





#Data cleaning if needed
epi_data <- epi_data %>%
  mutate(across(where(is.character), as.factor))



# 2. Define the UI
ui <- fluidPage(
  theme = shinytheme("simplex"),
  titlePanel("Global EPI Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Choose an Indicator:",
                  choices = c("EPI", "ENVHEALTH", "ECOSYSTEM", "GDP_capita", "Population2005","DALY_SC")),
      sliderInput("epi_range", "EPI Score Range:",
                  min = min(epi_data$EPI, na.rm = TRUE),
                  max = max(epi_data$EPI, na.rm = TRUE),
                  value = c(min(epi_data$EPI, na.rm = TRUE), max(epi_data$EPI, na.rm = TRUE))),
      selectInput("region", "Choose a region:",
                  choices = c("All", levels(epi_data$EPI_regions)), selected = "All")
    ),
    mainPanel(
      plotOutput("epi_plot"),
      dataTableOutput("epi_table")
    )
  )
)

# 3. Define the server logic
server <- function(input, output) {
  
  # Filtered data based on user input
  filtered_data <- reactive({
    data <- epi_data
    if(input$region != "All") {
      data <- data %>% filter(EPI_regions == input$region)
    }
    data %>%
      filter(EPI >= input$epi_range[1] & EPI <= input$epi_range[2])
  })
  
  # Create the plot
  output$epi_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Country, y = .data[[input$indicator]])) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = paste(input$indicator, "by Country"),
           x = "Country",
           y = input$indicator) +
      theme_minimal()
  })
  
  #Create a table to display data
  output$epi_table <- renderDataTable({
    filtered_data()
  })
}

# 4. Run the app
shinyApp(ui = ui, server = server)


