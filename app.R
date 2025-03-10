install.packages("shiny")
install.packages("rio")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("shinythemes")

library(shiny)
library(rio)  # For easy data import
library(dplyr)
library(ggplot2)  # For plotting
library(shinythemes)




# 1. Load the data
epi_data <- import("data/2008-epi-all-countries-winsorization.csv")
epi_data <- epi_data %>%
  mutate(across(where(is.character), as.factor))
