library(shiny)
library(dplyr)
library(ggplot2)
library(Cairo)
library(DT)

lager_data <- read_excel("lager_beispiel.xlsx")
View(lager_data)
lager_data["Monate"] <- as.Date(lager_data$Monate)

mt <- read.csv("visits.csv")
colnames(mt)[1] = 'counter'
colnames(mt)[2] = 'visits'
data_mt_template <<- data.frame(mt[1],mt[2])
data_mt <<- data_mt_template

point <- list(x=vector("numeric", 0), y=vector("numeric", 0)) 
vals <<- reactiveValues(x=1,y=1)

server <- function(input, output) {
  refreshPlot <- function(input, output, vals, data_mt) {
    output$table <- DT::renderDataTable({
      DT::datatable(
        data_mt[data_mt$clicked == TRUE, -which(names(data_mt) == "clicked")],
        editable = "cell",
        options = list(
          searching = FALSE,
          lengthChange = FALSE,
          bPaginate = FALSE,
          info = FALSE,
          language = list(
            emptyTable = " Noch keine Prognose-Werte"
          ),
          columnDefs = list(
            list(targets = which(colnames(data_mt) == "visits"), name = "Visits", render = JS("function(data, type, row, meta) { return parseInt(data); }"), editable = TRUE),
            list(targets = 0, visible = FALSE)
          )
        )
      )
    })
    
    output$info <- renderText({
      paste0("Ausgewählter Punkt:\nx = ", floor(vals$x), "\ny = ", format(round(as.double(data_mt$visits[data_mt$counter == floor(vals$x)])),big.mark=".", decimal.mark = ","))
      
    })
    
    output$plot1 <- renderPlot({
      print(tail(data_mt$visits, n= 1))
      unclicked_vals <- subset(data_mt, clicked %in% c(FALSE))
      clicked_vals <- subset(data_mt, clicked %in% c(TRUE))
      clicked_vals <- rbind(clicked_vals, unclicked_vals[nrow(unclicked_vals),])
      
      unclicked_vals %>% 
        ggplot(aes(x=counter, y=visits)) +
        geom_line(color="#05095E", data=unclicked_vals) + ylim(0,400000000) + 
        scale_y_continuous(name = "Visits",limits = c(0, max(data_mt$visits, na.rm = TRUE)*1.2), breaks = scales::breaks_width(25000000),labels = scales::number_format(accuracy = 1)) +
        scale_x_continuous(name = "Zeit",breaks = scales::breaks_width(5), labels = scales::number_format(accuracy = 1)) +
        theme_bw() +
        geom_vline(xintercept = 55, linetype = "dashed", color = "darkgrey") + 
        geom_vline(xintercept = 60, linetype = "dashed",  color = "darkgrey") + 
        geom_vline(xintercept = 65, linetype = "dashed",  color = "darkgrey") +
        geom_line(data=clicked_vals, color = "#E6B700", size = 1) +
        geom_point(aes(x=floor(vals$x), y=data_mt$visits[data_mt$counter == floor(vals$x)]), colour="#000BE6")
    })
  }
  
  data_mt <<- data_mt_template
  data_mt$clicked <<- rep(FALSE, nrow(data_mt))
  last_x <<- tail(data_mt$counter, n=1)
  offset_tens <<- 10 - last_x %% 10 + last_x
  print(10 - last_x %% 10)
  observeEvent(input$resetbutton, {
    data_mt <<- data_mt_template
    data_mt$clicked <<- rep(FALSE, nrow(data_mt))
    vals$x <- 1
    output$table <- renderTable({})
    refreshPlot(input, output, vals, data_mt)
  }
  )
  
  selected_values <- data.frame(x=c(1), y=c(1))
  observeEvent(input$plot_click, {
    print(input)
    print(tail(data_mt$counter, n=1))
    if(input$plot_click$x < 1) {
      vals$x <- 1
    } else if(input$plot_click$x - 2.5 > tail(data_mt_template$counter, n=1)){
      selected_values[nrow(selected_values) + 1,] <- c(input$plot_click$x, input$plot_click$y)
      if(input$plot_click$x < offset_tens+7.5 && input$plot_click$x > offset_tens+2.5) {
        vals$x <- offset_tens + 5
        if(rlang::is_empty(data_mt$visits[data_mt$counter == vals$x])) {
          data_mt[nrow(data_mt) + 1,] <<- c(vals$x, visits=input$plot_click$y, clicked = TRUE)
        } else {
          data_mt$visits[data_mt$counter == vals$x] <<- input$plot_click$y
        }
      } else if(input$plot_click$x < offset_tens+12.5 && input$plot_click$x > offset_tens+7.5) {
        vals$x <- offset_tens + 10
        if(rlang::is_empty(data_mt$visits[data_mt$counter == vals$x])) {
          data_mt[nrow(data_mt) + 1,] <<- c(vals$x, visits=input$plot_click$y, clicked = TRUE)
        } else {
          data_mt$visits[data_mt$counter == vals$x] <<- input$plot_click$y
        }      
      } else if(input$plot_click$x < offset_tens+17.5 && input$plot_click$x > offset_tens+12.5) {
        vals$x <- offset_tens + 15
        if(rlang::is_empty(data_mt$visits[data_mt$counter == vals$x])) {
          data_mt[nrow(data_mt) + 1,] <<- c(vals$x, visits=input$plot_click$y, clicked = TRUE)
        } else {
          data_mt$visits[data_mt$counter == vals$x] <<- input$plot_click$y
        }
      } else {
        vals$x <- tail(data_mt_template$counter, n=1)
      }
    } else {
      if(rlang::is_empty(data_mt$counter == input$plot_click$x)) {
        vals$x <- tail(data_mt_template$counter, n=1)
      } else {
        vals$x <- input$plot_click$x
      }
    }
    
    print(vals$x)
    refreshPlot(input, output, vals, data_mt)
    
  })
  
  refreshPlot(input, output, vals, data_mt)
  
}


