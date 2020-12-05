
library(tidyverse)
library(mosaicData)
library(shiny)
df <- read.csv("ShinyPoliceAll2.csv")

# Define UI for application that draws a scatterplot
ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            selectInput("dataset", label="Data Options", choices=c("Disaggregated","Aggregated")),
            selectInput("x", label = "X variable",
                        choices = names(df), selected = "WhitePoliceShare"),
            selectInput("y", label = "Y variable",
                        choices = names(df), selected = "RatioBlackWhite"),
            selectInput("transform", label = "Y transformation",
                        choices = c("identity", "log", "sqrt"), selected = "identity"),
            selectInput(inputId = "color", 
                        label = "Color by:",
                        choices = c("None", "Region", "animus_score", "implicit_score_mean"),
                        selected = "None"),
            checkboxInput("facet",
                          label = "facet by Region?",
                          value = FALSE),  
            checkboxInput("refLine",
                          label = "reference line",
                          value = FALSE),  
            selectInput(inputId = "line", 
                        label = "Add Line?",
                        choices = c("None", "Line", "Smoother"),
                        selected = "None",
                        multiple = FALSE)
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
        
        if (input$dataset=="Disaggregated"){
            df <- read.csv(url("https://github.com/jawitmer/JuniorPracticum/raw/master/ShinyPoliceAll2.csv"))}
        if (input$dataset=="Aggregated"){
            df <- read.csv(url("https://github.com/jawitmer/JuniorPracticum/raw/master/ShinyPoliceAgg2.csv"))}    
           
        transform_fun <-
            switch(
                input$transform,
                identity = function(x) {x},
                log = function(x) {log(0.01 + x)},
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
        if (input$refLine == TRUE) {
            if (input$transform == "identity") {
                p <- p + geom_hline(yintercept=1, linetype="dashed", color = "red") 
            }
            if (input$transform == "sqrt") {
                p <- p + geom_hline(yintercept=1, linetype="dashed", color = "red") 
            }
            if (input$transform == "log") {
                p <- p + geom_hline(yintercept=0, linetype="dashed", color = "red") 
            }   
        }
        p        
        if (input$facet == TRUE) {
            p <- p + facet_wrap(~Region, ncol = 4)
        }
        p
        if (input$line == "Line") {
            p <- p + stat_smooth(method = lm, se=FALSE)
        }
        if (input$line == "Smoother") {
            p <- p + geom_smooth(se=FALSE)
        }
        p
    })
}

# Run the application
shinyApp(ui = ui, server = server)