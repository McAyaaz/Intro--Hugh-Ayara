library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

# Import Dataset
setwd("~/GitHub/Intro--Hugh-Ayara")
alzheimers_disease <- read_xlsx("alzheimers_disease.xlsx", sheet = 1)

# Define UI for application that draws Different Plots
ui <- fluidPage(
  
  # Application title
  titlePanel("Alzheimer Disease Analysis"),
  
  # Sidebar with a slider input for Age 
  sidebarLayout(
    sidebarPanel(
      
      # Drop down Menu to select the different Visualizations
      selectInput("plot_type", 
                  "Select Alzheimer's Visualization:", 
                  choices = c("Sleep Quality Across Ethnicity", 
                              "Scatter Plot of BMI by Ethnicity", 
                              "Sleep Quality Across Gender", 
                              "Box Plot of Age Vs Sleep Quality",
                              "Density Plot of Diet Quality",
                              "Density Plot of Sleep Quality",
                              "Relationship Between Sleep Quality and Gender"),
                  selected = "Sleep Quality Across Ethnicity"),
      
      # Sliders for continuous variable comparison
      conditionalPanel(
        condition = "input.plot_type == 'Box Plot of Age vs Sleep Quality'",
        sliderInput("Age_slider", "Select Age Category:", 
                    min = 60, max = 90, value = c(60, 90))
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("selected_plot")
    )
  )
)

# Define server logic required to Visualize
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on slider inputs
  filtered_data <- reactive({
    data <- alzheimers_disease
    
    if (input$plot_type == "Box Plot of Age vs Sleep Quality") {
      data <- data %>%
        filter(Age >= input$Age_slider[1] & Age <= input$Age_slider[2])
    }
    
    return(data)
  })
  
  # Reactive expression to generate the selected plot
  output$selected_plot <- renderPlot({
    data <- filtered_data()
    
    # Based on user selection, render different plots
    if (input$plot_type == "Sleep Quality Across Ethnicity") {
      data <- data %>%
        mutate(
          Ethnicity = factor(Ethnicity, levels = c(0, 1, 2, 3), 
                             labels = c("Caucasian", "African American", "Asian", "Other"))
        )
      
      ggplot(data, aes(x = Ethnicity, y = SleepQuality, fill = Ethnicity)) +
        geom_boxplot() +
        labs(
          title = "Sleep Quality Across Ethnicity",
          x = "Ethnicity",
          y = "Sleep Quality",
          fill = "Ethnicity"
        ) +
        theme_minimal() + 
        scale_fill_manual(values = c("Caucasian" = "lightblue", 
                                     "African American" = "orange", 
                                     "Asian" = "red", 
                                     "Other" = "brown"))
    }
    else if (input$plot_type == "Sleep Quality Across Gender") {
      data <- data %>%
        mutate(
          Gender = factor(Gender, levels = c(0, 1), labels = c("Male", "Female"))
        )
      
      ggplot(data, aes(x = Gender, y = SleepQuality, fill = Gender)) +
        geom_boxplot() +
        labs(
          title = "Sleep Quality Across Gender",
          x = "Gender",
          y = "Sleep Quality",
          fill = "Gender"
        ) +
        theme_minimal() +
        scale_fill_manual(values = c("Male" = "lightblue",
                                     "Female" = "orange"))
    }
    else if (input$plot_type == "Relationship Between Sleep Quality and Gender") {
      data <- data %>%
        mutate(
          Gender = factor(Gender, levels = c(0, 1), labels = c("Male", "Female"))
        )
      
      ggplot(data, aes(x = Gender, y = SleepQuality)) +
        geom_point(color = "lightblue", alpha = 0.7) +
        geom_smooth(method = "lm", color = "red", se = TRUE) +
        labs(
          title = "Relationship Between Sleep Quality and Gender",
          x = "Gender",
          y = "Sleep Quality"
        ) +
        theme_minimal()
    }
    else if (input$plot_type == "Density Plot of Diet Quality") {
      ggplot(data, aes(x = DietQuality)) +
        geom_density(fill = "purple", alpha = 0.7) +
        labs(
          title = "Density Plot of Diet Quality",
          x = "Diet Quality",
          y = "Density"
        ) +
        theme_minimal()
    }
    else if (input$plot_type == "Density Plot of Sleep Quality") {
      ggplot(data, aes(x = SleepQuality)) +
        geom_density(fill = "purple", alpha = 0.7) +
        labs(
          title = "Density Plot of Sleep Quality",
          x = "Sleep Quality",
          y = "Density"
        ) +
        theme_minimal()
    }
      # Scatter Plot of BMI by Ethnicity 
      else if (input$plot_type == "Scatter Plot of BMI by Ethnicity") {      
        data <- data %>%        filter(Age >= 60 & Age <= 90) %>%  
          mutate(Ethnicity = factor(Ethnicity,
                                    levels = c(0, 1, 2, 3),                                  
                                    labels = c("Caucasian", "African American", "Asian", "Other")))            
        ggplot(data, aes(x = BMI, y = Age, color = Ethnicity)) +  
        geom_point(alpha = 0.7, size = 3) +  # Add transparency for better visualization     
        geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") + # Trendline       
        scale_color_manual(values = c("Caucasian" = "blue","African American" = "red","Asian" = "green","Other" = "purple")) + # Custom colors  
          labs(title = "Scatter Plot of BMI by Ethnicity",
               x = "BMI",    
               y = "Age",       
               color = "Ethnicity") +   
          theme_minimal() +   
          theme(          
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
            axis.title = element_text(size = 14), 
            legend.title = element_text(size = 12), legend.position = "right"
          )
    }
    else if (input$plot_type == "Box Plot of Age Vs Sleep Quality") {
      data <- data %>%
        mutate(
          Age_category = cut(Age, breaks = c(60, 70, 80, 90), 
                             labels = c("60-70", "70-80", "80-90"))
        )
      
      ggplot(data, aes(x = Age_category, y = SleepQuality, fill = Age_category)) +
        geom_boxplot() +
        labs(
          title = "Box Plot of Age vs Sleep Quality",
          x = "Age Category",
          y = "Sleep Quality",
          fill = "Age Category"
        ) +
        theme_minimal() +
        scale_fill_manual(values = c("60-70" = "lightblue",
                                     "70-80" = "orange",
                                     "80-90" = "red"))
    }
  })
}  

# Run the application 
shinyApp(ui = ui, server = server)