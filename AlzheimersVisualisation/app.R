#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
install.packages("pacman")
library(pacman)
pacman::p_load(shiny, ggplot2, dplyr, readxl)

# Load Dataset
setwd("~/GitHub/Intro--Hugh-Ayara")
alzheimers_disease <- read_excel("alzheimers_disease.xlsx", col_types = "text")
ui <- fluidPage(

# Data Cleaning
alzheimers_disease <- alzheimers_disease %>%
  mutate(
    Gender = factor(Gender, levels = c(0, 1), labels = c("Male", "Female")),
    Age = as.numeric(Age),
    Age_category = cut(Age, breaks = c(60, 70, 80, 90), labels = c("60-70", "70-80", "80-90")),
    Ethnicity = factor(Ethnicity, levels = c(0, 1, 2, 3), labels = c("Caucasian", "African American", "Asian", "Other")),
    EducationLevel = factor(EducationLevel, levels = c(0, 1, 2, 3), labels = c("None", "High School", "Bachelor's", "Higher")),
    DietQuality = as.numeric(DietQuality),
    SleepQuality = as.numeric(SleepQuality),
    BMI = as.numeric(BMI),
    AlcoholConsumption = as.numeric(AlcoholConsumption),
    PhysicalActivity = as.numeric(PhysicalActivity),
    ADL = as.numeric(ADL),
    Smoking = factor(Smoking, levels = c(0, 1), labels = c("No", "Yes"))
  ),
)
server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(alzheimers_disease, aes_string(x = input$xvar, y = input$yvar, color = input$colorvar)) +
      geom_point(alpha = 0.7) +
      labs(
        x = input$xvar,
        y = input$yvar,
        color = input$colorvar,
        title = paste(input$yvar, "vs", input$xvar)
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)


