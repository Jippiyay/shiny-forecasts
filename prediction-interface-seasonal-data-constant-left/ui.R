library(ggplot2)
library(shiny)

tags$head(
  HTML(
    "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
  )
)

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
