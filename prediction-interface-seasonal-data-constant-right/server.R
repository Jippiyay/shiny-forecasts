library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(httr)
library(cookies)
library(masterapi)

set.seed(123)
n <- 48 # Maximum number of data points
time <- seq(1, n) # Independent variable (time)

# Define the seasonal components with greater variance
amp <- c(30, 2, 1.5,20) # Amplitude of seasonal components
y_season1 <- amp[1] * sin(2*pi*time/12)
y_season2 <- amp[2] * sin(4*pi*time/12)
y_season3 <- amp[3] * sin(6*pi*time/12)
y_season4 <- amp[4] * sin(8*pi*time/12)
y_seasonal <- y_season1 + y_season2 + y_season3 + y_season4
y_noise <- rnorm(n, 10000, 10) # Noise
seasonal_steady <-  y_seasonal + y_noise

# Create data frame
df_up <- data.frame(time_seasonal = seq(1, 48), seasonal_steady = seasonal_steady)
# Add year labels
df_up$year <- rep(1:4, each = 12)
df_up_template <<- df_up

# Part of declaring useful variables 
point <- list(x=vector("numeric", 0), y=vector("numeric", 0)) 
vals <<- reactiveValues(x=1,y=1)
color <- c("red","red","red","red","red","red")
time_seasonal <- seq(49,54)
linecolors <<- data.frame(time_seasonal,color)
submitted_earlier <<- FALSE
endval <<- 54

# server 
server <- function(input, output) {
  validatePoints <- function(){
    clicked_vals <- subset(df_up, clicked %in% c(TRUE))
    linecolors$color <<- "darkgrey"
      if(nrow(clicked_vals) < 6) {
        linecolors[!linecolors$time_seasonal %in% clicked_vals$time_seasonal, "color"] <<- "red"
          return(FALSE)
      }
    return(TRUE)
  }
  
  reset <- function() {
    submitted_earlier <<- FALSE
    df_up <<- df_up_template
    df_up$clicked <<- rep(FALSE, nrow(df_up))
    vals$x <- 1
    output$table <- renderTable({})
    color <- c("red","red","red","red","red","red")
    time_seasonal <- seq(49,endval)
    linecolors <<- data.frame(time_seasonal,color)
  }
  
  reset()
  
  refreshPlot <- function(input, output, vals, df_up) {
    output$table <- DT::renderDataTable({
      DT::datatable(
        df_up[df_up$clicked == TRUE, c("time_seasonal", "seasonal_steady")],
        editable = "cell",
        colnames = c('Zeit', 'Verkäufe'),
        options = list(
          searching = FALSE,
          lengthChange = FALSE,
          bPaginate = FALSE,
          info = FALSE,
          language = list(
            emptyTable = "Noch keine Prognose-Werte"
          ),
          columnDefs = list(
            list(targets = which(colnames(df_up) == "seasonal_steady"), 
                 render = JS("function(data, type, row, meta) { return parseInt(Math.round(data)).toLocaleString('de-DE') }"), 
                 editable = TRUE),
            list(targets = 0, visible = FALSE)
          )
        )
      )
    })
    
    
    output$info <- renderText({
      paste0("Ausgewählter Punkt:\nZeit = ", floor(vals$x), "\nVerkäufe = ", format(round(as.double(df_up$seasonal_steady[df_up$time_seasonal == floor(vals$x)])),big.mark=".", decimal.mark = ","))
      
    })
    
    output$plot1 <- renderPlot({
      print(tail(df_up$seasonal_steady, n= 1))
      unclicked_vals <- subset(df_up, clicked %in% c(FALSE))
      clicked_vals <- subset(df_up, clicked %in% c(TRUE))
      clicked_vals <- rbind(clicked_vals, unclicked_vals[nrow(unclicked_vals),])
      
      
      unclicked_vals %>% 
        ggplot(aes(x=time_seasonal, y=seasonal_steady)) +
        geom_line(color="#05095E", data=unclicked_vals) + 
        scale_y_continuous(name = "Verkäufe\n",
                           limits = c(min(df_up$seasonal_steady,na.rm = TRUE)- 20, max(df_up$seasonal_steady, na.rm = TRUE)*1.002), 
                           labels = scales::number_format(accuracy = 1),
                           position = "right") +
        scale_x_continuous(name = "\nZeit",
                           breaks = scales::breaks_width(2), 
                           labels = scales::number_format(accuracy = 1),
                           expand = c(0.02, 0)) +
        theme_bw() +
        geom_vline(xintercept = 49, linetype = "dashed", color = linecolors[1, "color"]) + 
        geom_vline(xintercept = 50, linetype = "dashed", color = linecolors[2, "color"]) + 
        geom_vline(xintercept = 51, linetype = "dashed", color = linecolors[3, "color"]) +
        geom_vline(xintercept = 52, linetype = "dashed", color = linecolors[4, "color"]) +
        geom_vline(xintercept = 53, linetype = "dashed", color = linecolors[5, "color"]) +
        geom_vline(xintercept = 54, linetype = "dashed", color = linecolors[6, "color"]) +
        geom_line(data=clicked_vals, color = "#E6B700", size = 1) +
        geom_point(data=df_up[df_up$time_seasonal == floor(vals$x), ], aes(x=time_seasonal, y=seasonal_steady), colour="#000BE6", size =2.3) +
        geom_point(data=clicked_vals[clicked_vals$time_seasonal !=48, ], aes(x=time_seasonal, y=seasonal_steady), colour="#F2A60C")
    })
  }
  
  df_up <<- df_up_template
  df_up$clicked <<- rep(FALSE, nrow(df_up))
  last_x <<- tail(df_up$time_seasonal, n=1)
  #print(last_x)
  offset_tens <<- last_x
  print(offset_tens)
  #print(10 - last_x %% 10)
  observeEvent(input$resetbutton, {
    event <- list(
      name = "resetclick",
      date = date(),
      session_id = get_cookie(cookie_name="PHPSESSID"),
      app_id = Sys.getenv("SERVICE"),
      reset = input$resetbutton
    ) 
    track(event)
    reset()
    refreshPlot(input, output, vals, df_up)
  })
  
  observeEvent(input$submitbutton, {
    event <- list(
      name = "submitclick",
      date = date(),
      session_id = get_cookie(cookie_name="PHPSESSID"),
      app_id = Sys.getenv("SERVICE"),
      submit = input$submitbutton
    ) 
    track(event)
    submitted_earlier <<- TRUE
    if(!validatePoints()) {
      showNotification(duration = 5,"Unvollständige Prognose", type = "warning")
    } else {
      # saving
      resultpoints <- subset(df_up, clicked %in% c(TRUE))
      colnames(resultpoints) <- c("x", "y", "year", "clicked")
      result <- list(
        date = date(),
        session_id = get_cookie(cookie_name="PHPSESSID"),
        app_id = Sys.getenv("SERVICE"),
        points = resultpoints
      )
      store(result)
    }
    refreshPlot(input, output, vals, df_up)
  })
  
  
  selected_values <- data.frame(x=c(1), y=c(1))
  observeEvent(input$plot_click, {
    event <- list(
      name = "plotclick",
      date = date(),
      session_id = get_cookie(cookie_name="PHPSESSID"),
      app_id = Sys.getenv("SERVICE"),
      clicks = input$plot_click,
      input_x = round(input$plot_click$x)
    )
    track(event)
    print(input)
    print(tail(df_up$time_seasonal, n=1))
    if(input$plot_click$x < 1) {
      vals$x <- 1
    } else if(input$plot_click$x > tail(df_up_template$time_seasonal, n=1)){
      selected_values[nrow(selected_values) + 1,] <- c(input$plot_click$x, input$plot_click$y)
      if(input$plot_click$x <= endval && input$plot_click$x > offset_tens+1) {
        vals$x <- round(input$plot_click$x)
        print(input)
        if(rlang::is_empty(df_up$seasonal_steady[df_up$time_seasonal == vals$x])) {
          df_up[nrow(df_up) + 1,] <<- c(vals$x, seasonal_steady=input$plot_click$y, year = 5, clicked = TRUE)
        } else {
          df_up$seasonal_steady[df_up$time_seasonal == vals$x] <<- input$plot_click$y
        }
     } else {
        vals$x <- tail(df_up_template$time_seasonal, n=1)
      }
    } else {
      if(rlang::is_empty(df_up$time_seasonal == input$plot_click$x)) {
        vals$x <- tail(df_up_template$time_seasonal, n=1)
      } else {
        vals$x <- input$plot_click$x
      }
    }
    
    print(vals$x)
    refreshPlot(input, output, vals, df_up)
    validatePoints()
  })
  
  dataTableProxy('table')
  observeEvent(input$table_cell_edit, {
    info = input$table_cell_edit
    clicked_vals <- subset(df_up, clicked %in% c(TRUE))
    event <- list(
      name = "table",
      date = date(),
      session_id = get_cookie(cookie_name="PHPSESSID"),
      app_id = Sys.getenv("SERVICE"),
      y = input$table_cell_edit$value,
      x = clicked_vals[info$row, "time_seasonal"]
    )
    track(event)
    df_up[df_up$time_seasonal == clicked_vals[info$row, "time_seasonal"],"seasonal_steady"] <<- info$value
    print(info)
    refreshPlot(input, output, vals, df_up)
  })
  
  refreshPlot(input, output, vals, df_up)
  
}