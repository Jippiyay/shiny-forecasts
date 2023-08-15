library(ggplot2)
library(Cairo)  # For nicer ggplot2 output when deployed on Linux
library(shiny)
library(dplyr)

ui <- fluidPage(
  fluidRow(
    column(width = 12, class = "well",
           h4("Bereich markieren und Klicken für Zoom"),
           plotOutput("plot1", 
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )
           )
    )
  )
)
