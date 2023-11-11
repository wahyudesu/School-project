#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define user interface
ui <- fluidPage(
  
  # Application title
  titlePanel("Distribusi Eksponensial"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Input field for lambda
      numericInput("lambda_input", "Masukkan Nilai Lambda:", value = 2, min = 1, max = 10, step = 0.1),
      
      # Input field for x batas atas
      numericInput("x_input_ba", "Masukkan Nilai batas atas:", value = 5, min = 0.1, max = 20, step = 0.1),
      
      # Input field for x batas bawah
      numericInput("x_input_bb", "Masukkan Nilai batas bawah:", value = 2, min = 0.1, max = 20, step = 0.1),
      
      # Input field for sample
      numericInput("sampel", "Masukkan Nilai sampel:", value = 20, min = 5, max = 100, step = 1),
      
      # Button to update plot
      actionButton("update_plot", "Update Plot")
    ),
    
    # Main panel for displaying the plot
    mainPanel(
      plotOutput("exponential_plot"),
      textOutput("probability_output")
    )
  )
)


# Define server logic
server <- function(input, output) {
  generate_exponential_plot <- function(lambda, xb,xa , sampel) {
    lambda = 1/lambda
    x_values <- seq(0, sampel, by = 0.1)
    pdf_values <- dexp(x_values, rate = lambda)
    
    ggplot() +
      geom_line(aes(x = x_values, y = pdf_values), color = "blue", linetype = "solid") +
      labs(title = "Distribusi Eksponensial",
           x = "Waktu (menit)",
           y = "Density") +
      theme_minimal() +
      theme(legend.position = "none") +
      
      # Add vertical line at x (user input)
      geom_vline(xintercept = xa, linetype = "dashed", color = "red") + 
      geom_vline(xintercept = xb, linetype = "dashed", color = "red")
  }
  
  # Render the initial plot
  output$exponential_plot <- renderPlot({
    generate_exponential_plot(input$lambda_input, input$x_input_bb, input$x_input_ba, input$sampel)
  })
  
  # Update plot and calculate probability when button is clicked
  observeEvent(input$update_plot, {
    output$exponential_plot <- renderPlot({
      generate_exponential_plot(input$lambda_input, input$x_input_bb, input$x_input_ba, input$sampel)
    })
    
    # Calculate the probability
    bb <- input$x_input_bb  # Batas bawah
    ba <- input$x_input_ba  # Batas atas
    lambda = input$lambda_input # lambda value
    probability <- pexp(ba, rate = 1/lambda) - pexp(bb, rate = 1/lambda)
    output$probability_output <- renderText({
      paste("Probabilitas (", input$x_input_bb, "< X < ", input$x_input_ba, ") =", probability)
    })
  })
}


# Run the application
shinyApp(ui = ui, server = server)

