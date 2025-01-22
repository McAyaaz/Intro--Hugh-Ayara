#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Alzheimers Breakdown"),
    includeHTML("../index.html")
)
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Age:",
                        min = 65,
                        max = 100,
                        value = 76)
        ),
# Define server logic required to draw a histogram
server <- function(input, output) {
  
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )

