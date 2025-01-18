#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Install necessary packages
install.packages(c("shiny", "haven", "ggplot2", "dplyr", "DT"))

# Load libraries
library(shiny)
library(haven)
library(ggplot2)
library(dplyr)
library(DT)


# UI for the Shiny App
ui <- fluidPage(
  titlePanel("Alzheimer Dataset Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Select Visualization Options"),
      
      # Select variable for plotting
      selectInput("x_var", "Select X Variable:", choices = names(data)),
      selectInput("y_var", "Select Y Variable (optional):", choices = c("None", names(data))),
      
      # Select plot type
      radioButtons("plot_type", "Select Plot Type:",
                   choices = c("Histogram", "Boxplot", "Scatter Plot")),
      
      # Filters for numeric variables
      sliderInput("bins", "Number of Bins (for Histogram):", min = 5, max = 50, value = 10),
      
      hr(),
      h3("Dataset Table Options"),
      
      # Data table options
      checkboxInput("show_table", "Show Dataset Table", value = FALSE)
    ),
    
    mainPanel(
      # Output plots and table
      plotOutput("main_plot"),
      conditionalPanel(condition = "input.show_table == true", DTOutput("data_table"))
    )
  )
)

# Server for the Shiny App
server <- function(input, output) {
  
  # Generate plots based on user input
  output$main_plot <- renderPlot({
    req(input$x_var)  # Require an x-variable to proceed
    
    # Histogram
    if (input$plot_type == "Histogram") {
      ggplot(data, aes_string(x = input$x_var)) +
        geom_histogram(bins = input$bins, fill = "skyblue", color = "black") +
        labs(title = "Histogram", x = input$x_var, y = "Count")
      
      # Boxplot
    } else if (input$plot_type == "Boxplot") {
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_boxplot(fill = "lightgreen") +
        labs(title = "Boxplot", x = input$x_var, y = input$y_var)
      
      # Scatter Plot
    } else if (input$plot_type == "Scatter Plot" && input$y_var != "None") {
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(color = "blue", size = 2) +
        labs(title = "Scatter Plot", x = input$x_var, y = input$y_var)
    }
  })
  
  # Render dataset table
  output$data_table <- renderDT({
    datatable(data, options = list(pageLength = 10))
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
