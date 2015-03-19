#' setwd("C:/Users/dell e6530/Documents/rcharts")
require(shiny)
require(rCharts)
data(iris)
shinyUI(
           pageWithSidebar(
             headerPanel("shiny, rCharts and Highcharts.js:"),
             sidebarPanel(
               tags$div(
                 HTML('<h2><strong>Iris dataset:</strong></h2>')
               ),
               selectInput(inputId = "x",
                           label = "Choose Sepal characteristics:",
                           choices = c('Sepal.Length', 'Sepal.Width'),
                           selected = c('Sepal.Length', 'Sepal.Width'),multiple=TRUE),
               selectInput(inputId = "specx",
                           label = "Choose Species:",
                           choices = unique(as.character(iris$Species))
               ),
               selectInput(inputId = "y",
                           label = "Choose Petal characteristics",
                           choices = c('Petal.Length', 'Petal.Width'),
                           selected = c('Petal.Length', 'Petal.Width'),multiple=TRUE),
               selectInput(inputId = "specy",
                           label = "Choose Species:",
                           choices = unique(as.character(iris$Species))
               )
             ),
  mainPanel(
    tabsetPanel(
      tabPanel("Number of species:",showOutput("piechart", "highcharts")),
      tabPanel("Comparison of Petal:",showOutput("multiline_petal", "highcharts")),
      tabPanel("Comparison of Sepal:",showOutput("multiline_sepal", "highcharts")),
      tabPanel("Sepal vs Petal by species:",showOutput("scatterplot", "highcharts")),
      tabPanel("Sepal vs Petal by species(+ regression lines)",showOutput("regression", "highcharts")),
      tabPanel("Mean Sepal by species (+Average)",showOutput("combo_sepal", "highcharts")),
      tabPanel("Mean Petal by species (+Average)",showOutput("combo_petal", "highcharts")),
#       tabPanel("single bar chart",showOutput("single_barchart", "highcharts")),
#        tabPanel("Mean of characteristics by species",showOutput("barstacked", "highcharts")),
      tabPanel("column range",showOutput("colrange", "highcharts"))
      #tabPanel("multiple bar",showOutput("multibar", "highcharts"))
    )
  )
))
