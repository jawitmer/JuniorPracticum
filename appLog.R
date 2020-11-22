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
df <- read.csv("NewCitiesAggregated2.csv")
tran_func <- function(data, pred, trans) {
    switch(trans,
           "None" = data[[pred]],
           "Square Root" = sqrt(data[[pred]]),
           "Log" = log(data[[pred]]),
    )
}

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "xaxis", 
                label = "Choose a Variable for the X-axis of the  Graph", 
                choices = colnames(df),
                selected = "WhitePoliceShare",
            ),
            selectInput(
                inputId = "yaxis", 
                label = "Choose a Variable for the Y-axis of the  Graph", 
                choices = colnames(df),
                selected = "RatioBlackWhite",               
            ),
            selectInput("Ytransform",
                        "Select Y Transformation",
                        choices = c("None", "Square Root", "Log"),
                        selected = "None"),
            checkboxInput("LogY", "Log Transform Y", FALSE),
            checkboxInput("line", "Show regression line?", value = FALSE),
            selectInput(inputId = "color", 
                        label = "Color by:",
                        choices = colnames(df),
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
        req(input$color)
        req(input$Ytransform)
#       dfPlot <- df
#       dfPlot <- df %>% mutate(input$yaxis <- tran_func(df, input$yaxis, input$Ytransform))
#       p <- df %>% mutate(input$yaxis <- tran_func(cur_data(), input$yaxis, input$Ytransform)) %>%        
        p <- df %>% ggplot(aes_string(x = paste0("`", input$xaxis, "`"), 
                y = paste0("`", input$yaxis, "`"), color = input$color)) + geom_point() 
        if(input$line) {
            p <- p + stat_smooth(method = lm, se=FALSE)
        }
        p
    })
} 


# Run the application 
shinyApp(ui = ui, server = server)