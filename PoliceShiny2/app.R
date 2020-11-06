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
NewCities <- read.csv("~/JuniorPracticumGitHub/NewCitiesNov5.csv")


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
            )
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
                                  y = paste0("`", input$yaxis, "`"))) + geom_point()
    })
} 



# Run the application 
shinyApp(ui = ui, server = server)
