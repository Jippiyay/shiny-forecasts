library(ggplot2)
library(shiny)

ui <- fluidPage(
  sidebarLayout(
    mainPanel(
      plotOutput("plot1", click = "plot_click")
    ),
    sidebarPanel(
      verbatimTextOutput("info"),
      DT::dataTableOutput("table"),
      div(style = "height: 20px;", ""),
      actionButton("resetbutton", "Zurücksetzen"),
      actionButton("submitbutton", "Fertig")
    )
  )
)
