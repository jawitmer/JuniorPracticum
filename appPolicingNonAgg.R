
library(tidyverse)
library(mosaicData)
library(shiny)
df <- read.csv("NewestCitiesDatabase.csv")

# Define UI for application that draws a scatterplot
ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            selectInput("x", label = "X variable",
                        choices = names(df), selected = "WhitePoliceShare"),
            selectInput("y", label = "Y variable",
                        choices = names(df), selected = "RatioBlackWhite"),
            selectInput("transform", label = "transformation",
                        choices = c("identity", "log", "sqrt"), selected = "identity"),
            selectInput(inputId = "color", 
                    label = "Color by:",
                    choices = colnames(df),
                    selected = "Region"),
            checkboxInput("line",
                          label = "Add a line?",
                          value = FALSE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
    
    dataset <- reactive({
        transform_fun <-
            switch(
                input$transform,
                identity = function(x) {x},
                log = function(x) {log(1 + x)},
                sqrt = function(x) {sqrt(x)}
            )
        df %>%
            mutate(
                y = transform_fun(.data[[input$y]])
            )
    })
    
    output$distPlot <- renderPlot({
        p <- ggplot(dataset(),
                   aes_string(x = input$x, y = "y", color = input$color)) +
            geom_point() +
            labs(y = paste0(input$y, " (", input$transform, ")"))
        if (input$line) {
            p <- p + stat_smooth(method = lm, se=FALSE)
        }
        p
    })
}

# Run the application
shinyApp(ui = ui, server = server)