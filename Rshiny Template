# Rshiny test
library(rsconnect)
library(shiny)
library(ggplot2)
library(shiny)
rsconnect::setAccountInfo(name='jmushailov', token='xxxxxxxxxxx', secret='xxxxxxxxx')

# Uses script provided by Rshiny to design server and UI

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# Deploy the App
  # After publishing/saving, set WD to the directory where the app resides
setwd("C:/users/joe/desktop/Shiny/Test")
 library(rsconnect)
 deployApp()

