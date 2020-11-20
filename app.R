#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(dplyr)
library(readr)
library(plyr)
library(ggplot2)
library(mosaic)

####### Read in the NewCities dataset #######
NewCities <- read.csv("NewCitiesAggregated2.csv")


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "xaxis", 
                label = "Choose a Variable for the X-axis of the  Graph", 
                choices = colnames(NewCities)
            ),
            selectInput(
                inputId = "yaxis", 
                label = "Choose a Variable for the Y-axis of the  Graph", 
                choices = colnames(NewCities)
            ),
            selectInput(inputId = "z", 
                        label = "Color by:",
                        choices = colnames(NewCities),
                        selected = "Region"),
        ),
        mainPanel(
            plotOutput(outputId = "scatterplot"))
    )
) 

#
# Define server logic required to draw a scatterplot
server <- function(input, output) {
    output$scatterplot <- renderPlot({
        req(input$xaxis)
        req(input$yaxis)
        ggplot(data = NewCities, aes_string(x = paste0("`", input$xaxis, "`"), 
                                            y = paste0("`", input$yaxis, "`"), color = input$z)) + geom_point()
    })
} 



# Run the application 
shinyApp(ui = ui, server = server)