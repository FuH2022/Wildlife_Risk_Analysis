# Interactive Risk Matrix Shiny App
library(shiny)
library(plotly)


# Define the UI for the application
ui <- fluidPage(
  titlePanel("Interactive Risk Matrix by Month"),
  sidebarLayout(
    sidebarPanel(
      selectInput("airportInput", "Select Airport:", choices = c("SAC", "LAX")),
      selectInput("monthInput", "Select Month:", choices = NULL)
    ),
    mainPanel(
      plotlyOutput("riskPlot")
    )
  )
)

# Define the server logic required to draw the risk level plot
server <- function(input, output, session) {
  
  # Create a reactive expression for the data filtered by the selected airport and month
  filtered_data <- reactive({
    airport_data <- switch(input$airportInput,
                           "SAC" = size_month_distribution_sac,
                           "LAX" = size_month_distribution_lax)
    
    airport_data %>%
      filter(INCIDENT_MONTH == input$monthInput)
  })
  
  # Update the month choices based on the selected airport
  observe({
    airport_data <- switch(input$airportInput,
                           "SAC" = size_month_distribution_sac,
                           "LAX" = size_month_distribution_lax)
    
    updateSelectInput(session, "monthInput", choices = unique(airport_data$INCIDENT_MONTH))
  })
  
  # Generate the plotly plot based on the filtered data
  output$riskPlot <- renderPlotly({
    data <- filtered_data()
    fig <- plot_ly(
      data = data,
      x = ~SIZE,
      y = ~RiskLevel,
      type = 'bar',
      marker = list(
        color = ~RiskColor
      ),
      text = ~paste('Size:', SIZE,
                    '<br>Incidents:', Incidents,
                    '<br>Risk Level:', RiskLevel)
    ) %>%
      layout(
        title = paste('Risk Matrix for', input$monthInput, 'at', input$airportInput, 'Airport'),
        xaxis = list(title = 'Wildlife Size (Severity)'),
        yaxis = list(title = 'Risk Level (Incidents * Severity)'),
        showlegend = FALSE
      )
    return(fig)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
