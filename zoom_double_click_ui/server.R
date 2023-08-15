library(shiny)
library(dplyr)
library(ggplot2)
library(Cairo) 

mt <- read.csv("visits.csv")
colnames(mt)[1] = 'counter'
colnames(mt)[2] = 'visits'
data_mt_vis <- data.frame(mt[1],mt[2])


# Define server logic required to draw a histogram

server <- function(input, output) {
  
  # -------------------------------------------------------------------
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    data_mt_vis %>% 
      ggplot(aes(x=counter, y=visits)) +
      geom_line(color="#69b3a2") +
      ylim(0,150000000) + scale_y_continuous(name = "Videoabrufe",breaks=c(25000000,50000000, 75000000, 100000000, 125000000),labels = scales::number_format(accuracy = 1)) +
      scale_x_continuous(name = "Zeiteinheiten",breaks=c(5,10,15,20,25,30,35,40,45,50), labels = scales::number_format(accuracy = 1)) + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      theme_bw()
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_click, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
}