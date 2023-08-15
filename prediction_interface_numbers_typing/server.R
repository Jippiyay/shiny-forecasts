library(shiny)
library(dplyr)
library(ggplot2)
library(Cairo)
library(DT)

mt <- read.csv("visits.csv")
colnames(mt)[1] = 'counter'
colnames(mt)[2] = 'visits'
data_mt_template <<- data.frame(mt[1],mt[2])
data_mt <<- data_mt_template

point <- list(x=vector("numeric", 0), y=vector("numeric", 0)) 
vals <<- reactiveValues(x=1,y=1)
color <- c("darkgrey", "darkgrey", "darkgrey")
counter <- c(55,60,65)
linecolors <<- data.frame(counter,color)
submitted_earlier <<- FALSE

server <- function(input, output) {
  validatePoints <- function(){
    if(!submitted_earlier) {
      return(TRUE)
    }
    clicked_vals <- subset(data_mt, clicked %in% c(TRUE))
    linecolors$color <<- "darkgrey"
      if(nrow(clicked_vals) < 3) {
        linecolors[!linecolors$counter %in% clicked_vals$counter, "color"] <<- "red"
          return(FALSE)
      }
    return(TRUE)
  }
  
  refreshPlot <- function(input, output, vals, data_mt) {
    output$table <- DT::renderDataTable({
      DT::datatable(
        data_mt[data_mt$clicked == TRUE, -which(names(data_mt) == "clicked")],
        editable = "cell",
        colnames = c('Zeit', 'Visits'),
        options = list(
          searching = FALSE,
          lengthChange = FALSE,
          bPaginate = FALSE,
          info = FALSE,
          language = list(
            emptyTable = " Noch keine Prognose-Werte"
          ),
          columnDefs = list(
            list(targets = which(colnames(data_mt) == "visits"), 
                 render = JS("function(data, type, row, meta) { return parseInt(Math.round(data)).toLocaleString('de-DE') }"), 
                 editable = TRUE),
            list(targets = 0, visible = FALSE)
          )
        )
      )
    })
    
    
    output$info <- renderText({
      paste0("Ausgewählter Punkt:\nZeit = ", floor(vals$x), "\nVisits = ", format(round(as.double(data_mt$visits[data_mt$counter == floor(vals$x)])),big.mark=".", decimal.mark = ","))
      
    })
    
    output$plot1 <- renderPlot({
      print(tail(data_mt$visits, n= 1))
      unclicked_vals <- subset(data_mt, clicked %in% c(FALSE))
      clicked_vals <- subset(data_mt, clicked %in% c(TRUE))
      clicked_vals <- rbind(clicked_vals, unclicked_vals[nrow(unclicked_vals),])
      
      
      unclicked_vals %>% 
        ggplot(aes(x=counter, y=visits)) +
        geom_line(color="#05095E", data=unclicked_vals) + ylim(0,400000000) + 
        scale_y_continuous(name = "Visits",limits = c(0, max(data_mt$visits)+10000000), breaks = scales::breaks_width(25000000),labels = scales::number_format(accuracy = 1)) +
        scale_x_continuous(name = "Zeit",breaks = scales::breaks_width(5), labels = scales::number_format(accuracy = 1)) +
        theme_bw() +
        geom_vline(xintercept = 55, linetype = "dashed", color = linecolors[1, "color"]) + 
        geom_vline(xintercept = 60, linetype = "dashed", color = linecolors[2, "color"]) + 
        geom_vline(xintercept = 65, linetype = "dashed", color = linecolors[3, "color"]) +
        geom_point(data=data_mt[data_mt$counter == floor(vals$x), ], aes(x=counter, y=visits), colour="#000BE6", size =2.3)
        #geom_line(data=clicked_vals, color = "#E6B700", size = 1) +
        #geom_point(data=clicked_vals[clicked_vals$counter != 48, ], aes(x=counter, y=visits), colour="#F2A60C")
    })
  }
  
  data_mt <<- data_mt_template
  data_mt$clicked <<- rep(FALSE, nrow(data_mt))
  last_x <<- tail(data_mt$counter, n=1)
  offset_tens <<- 10 - last_x %% 10 + last_x
  print(10 - last_x %% 10)
  observeEvent(input$resetbutton, {
    submitted_earlier <<- FALSE
    data_mt <<- data_mt_template
    data_mt$clicked <<- rep(FALSE, nrow(data_mt))
    vals$x <- 1
    output$table <- renderTable({})
    color <- c("darkgrey", "darkgrey", "darkgrey")
    counter <- c(55,60,65)
    linecolors <<- data.frame(counter,color)
    refreshPlot(input, output, vals, data_mt)
  })
  
  observeEvent(input$submitbutton, {
    submitted_earlier <<- TRUE
    if(!validatePoints()) {
      showNotification(duration = 5,"Unvollständige Prognose", type = "warning")
    }else showNotification(duration = 5,"Prognose wurde gespeichert")
    refreshPlot(input, output, vals, data_mt)
  })
  
  
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
    validatePoints()
  })
  
  dataTableProxy('table')
  observeEvent(input$table_cell_edit, {
    info = input$table_cell_edit
    clicked_vals <- subset(data_mt, clicked %in% c(TRUE))
    #clicked_vals[info$row, "visits"] <- info$value
    data_mt[data_mt$counter == clicked_vals[info$row, "counter"],"visits"] <<- info$value
    print(info)
    refreshPlot(input, output, vals, data_mt)
  })
  
  refreshPlot(input, output, vals, data_mt)
  
}


