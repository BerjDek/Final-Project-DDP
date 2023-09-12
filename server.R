
getwd()

# Define the server logic
server <- function(input, output, session) {
  # Create a reactive dataset filtered by selected countries and years
  filtered_data <- reactive({
    filtered <- data %>%
      filter(country %in% c(input$country1, input$country2, input$country3),
             year >= input$years[1], year <= input$years[2])
    return(filtered)
  })
  
  # Create a regression model between selected "data1" and "data2"
  model <- reactive({
    lm(formula = paste(input$data1, "~", input$data2), data = filtered_data())
  })
  
  # Plot 1: Line graph of "data1" for selected countries and years
  output$plot1 <- renderPlot({
    filtered <- filtered_data()
    ggplot(filtered, aes(x = year, y = .data[[input$data1]], color = country)) +
      geom_line() +
      labs(title = paste("Line Graph of", input$data1),
           x = "Year",
           y = input$data1)
  })
  
  # Plot 2: Line graph of "data2" for selected countries and years
  output$plot2 <- renderPlot({
    filtered <- filtered_data()
    ggplot(filtered, aes(x = year, y = .data[[input$data2]], color = country)) +
      geom_line() +
      labs(title = paste("Line Graph of", input$data2),
           x = "Year",
           y = input$data2)
  })
  
  # Plot 3: Scatter plot with regression line between "data1" and "data2"
  output$plot3 <- renderPlot({
    model_data <- filtered_data()
    ggplot(model_data, aes(x = .data[[input$data1]], y = .data[[input$data2]])) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = paste("Regression Plot between", input$data1, "and", input$data2),
           x = input$data1,
           y = input$data2)
  })
  
  # Calculate and display the regression coefficient as a prediction
  output$prediction_output <- renderText({
    coef(model())[2]  # Index 2 corresponds to the coefficient for input$data2
  })
}

# Create the Shiny app object and run it
shinyApp(ui, server)
