
getwd()

# Load necessary libraries
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load datasets
gini <- read.csv("gini.csv")
lex <- read.csv("lex.csv")
sug <- read.csv("sugar_per_person.csv")

# Function to remove the first character from column names
remove_first_character <- function(df) {
  names(df) <- sub("^X", "", names(df))
  return(df)
}

gini <- remove_first_character(gini) %>% gather(year, gini, -country)
lex <- remove_first_character(lex)  %>% gather(year, lex, -country)
sug <- remove_first_character(sug) %>% gather(year, sug, -country)


# Merge datasets by country and filter years

data <- gini %>%
  inner_join(lex, by = c("country", "year")) %>%
  inner_join(sug, by = c("country", "year"))%>%
  mutate(year = as.numeric(year))

# Define the UI

ui <- fluidPage(
  titlePanel("Comparison of Inequality, Sugar Consumption, and Life Expectancy Around the World"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Countries"),
      selectInput("country1", "Country 1", choices = unique(data$country)),
      selectInput("country2", "Country 2", choices = unique(data$country)),
      selectInput("country3", "Country 3", choices = unique(data$country)),
      hr(),
      h4("Select Data"),
      selectInput("data1", "Data 1", choices = c("gini", "lex", "sug")),
      selectInput("data2", "Data 2", choices = c("gini", "lex", "sug")),
      sliderInput("years", "Select Years", min = min(data$year), max = max(data$year), value = c(1970, 2018)),
      actionButton("update_button", "Update Plot"),
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        # First tab: Evolution through time
        tabPanel("Evolution through time",
                 fluidRow(
                   plotOutput("plot1"),
                   plotOutput("plot2")
                 )
        ),
        # Second tab: Regression and Prediction
        tabPanel("Regression and Prediction",
                 fluidRow(
                  plotOutput("plot3"),
                  h3("Regression Coefficient"),
                  verbatimTextOutput("prediction_output")
                                 )
        )
      )
    )
  )
)
