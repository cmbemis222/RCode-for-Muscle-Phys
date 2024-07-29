setwd("~/Desktop/UCI/InSitu Prep/Data")
library(shiny)
library(ggplot2)
library(forecast)
library(rsconnect)

ui <- fluidPage(
  titlePanel("Muscle Force Data Entry"),
      # Row for Passive Length and Force
      fluidRow(
        column(6, 
               numericInput("passiveLength", "Passive Length (mm):", value = NULL, width = '50%')),
        column(6, 
               numericInput("passiveForce", "Passive Force (N):", value = NULL, width = '50%'))
      ),
      # Row for Active Length and Force
      fluidRow(
        column(6, 
               numericInput("activeLength", "Active Length (mm):", value = NULL, width = '50%')),
        column(6, 
               numericInput("activeForce", "Active Force (N):", value = NULL, width = '50%'))
      ),
      # Action buttons and download
      actionButton("addPoint", "Add Point"),
      actionButton("removePoint", "Remove Last Point"),
      downloadButton("downloadData", "Download Data"),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Entry", tableOutput("dataTable")),
        tabPanel("Removed Data", tableOutput("removedDataTable")),
        tabPanel("Plot", plotOutput("forcePlot"))
      )
    )
)

server <- function(input, output, session) {
  # Reactive values to store data
  values <- reactiveValues(
    data = data.frame(PassiveLength = numeric(0),
                      Passive = numeric(0),
                      ActiveLength = numeric(0),
                      Active = numeric(0)),
    removedData = data.frame(PassiveLength = numeric(0),
                             Passive = numeric(0),
                             ActiveLength = numeric(0),
                             Active = numeric(0)),
    predictions = NULL
  )
  
  # Add data point
  observeEvent(input$addPoint, {
    new_data <- data.frame(PassiveLength = input$passiveLength,
                           Passive = input$passiveForce,
                           ActiveLength = input$activeLength,
                           Active = input$activeForce)
    values$data <- rbind(values$data, new_data)
  })
  
  # Remove last data point
  observeEvent(input$removePoint, {
    if (nrow(values$data) > 0) {
      last_point <- tail(values$data, 1)
      values$data <- head(values$data, -1)
      values$removedData <- rbind(values$removedData, last_point)
    }
  })
  
  # Export data to CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("muscle_force_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$data, file, row.names = FALSE)
    }
  )
  
  # Output the table of entered data
  output$dataTable <- renderTable({
    values$data
  })
  
  # Output the table of removed data
  output$removedDataTable <- renderTable({
    values$removedData
  })
  
  # Automatically generate and update predictions
  observe({
    req(nrow(values$data) >= 3)  # Ensure there are enough points for polynomial fit
    
    # Polynomial fit for passive force
    passive_fit <- lm(Passive ~ poly(PassiveLength, 2), data = values$data)
    active_fit <- lm(Active ~ poly(ActiveLength, 2), data = values$data)
    
    # Create a sequence of x-values for predictions
    x_passive <- seq(min(values$data$PassiveLength, na.rm = TRUE), max(values$data$PassiveLength, na.rm = TRUE) + 5, length.out = 100)
    x_active <- seq(min(values$data$ActiveLength, na.rm = TRUE), max(values$data$ActiveLength, na.rm = TRUE) + 5, length.out = 100)
    
    # Create data for plotting
    predictions <- data.frame(
      Length = c(x_passive, x_active),
      Force = c(predict(passive_fit, newdata = data.frame(PassiveLength = x_passive)),
                predict(active_fit, newdata = data.frame(ActiveLength = x_active))),
      Type = c(rep("Passive", length(x_passive)),
               rep("Active", length(x_active)))
    )
    
    values$predictions <- predictions
  })
  
  # Plot the data with polynomial fit
  output$forcePlot <- renderPlot({
    if (!is.null(values$predictions)) {
      ggplot() +
        geom_line(data = subset(values$predictions, Type == "Passive"),
                  aes(x = Length, y = Force, color = "Passive"), size = 1) +
        geom_line(data = subset(values$predictions, Type == "Active"),
                  aes(x = Length, y = Force, color = "Active"), size = 1) +
        geom_point(data = values$data, aes(x = PassiveLength, y = Passive), color = "blue") +
        geom_point(data = values$data, aes(x = ActiveLength, y = Active), color = "red") +
        theme_minimal() +
        labs(title = "FL Curves", x = "Length (mm)", y = "Force (N)", color = "Force Type")
    } else {
      ggplot() + 
        labs(title = "Enter More Data", x = "Length (mm)", y = "Force (N)") +
        theme_minimal()
    }
  })
}

shinyApp(ui = ui, server = server)

