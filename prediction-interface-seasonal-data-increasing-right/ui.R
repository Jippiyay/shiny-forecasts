library(ggplot2)
library(shiny)

ui <- fluidPage(
  plotOutput("plot1", click = "plot_click"),
  fluidRow(
    column(4,
           actionButton("resetbutton", "Zurücksetzen"),
           actionButton("submitbutton", "Fertig", style="background-color: green; color: white;"),
           div(style = "height: 20px;", ""),
           verbatimTextOutput("info")),
    column(8,
           DT::dataTableOutput("table"))
  )
)
