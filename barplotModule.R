
barplotModuleInput <- function(id){
  
  ns <- NS(id)
  
  div(id = frameModuleInput(id, 1),plotlyOutput(ns("plotstd")))
}

barplotModule <- function(input, output, session, type.choices, meas.choices, getfunc){
  
  gdps.data <- NULL
  
  last.pars <- list()
  msgs <- reactiveValues(errors = "", warnings = "")
  
  dlHandler <- downloadHandler(
    filename = function() { "gdp_data.csv" },
    content = function(file) {
      conn <- file(file,encoding="UTF-8")
      write.csv(gdps.data, file = conn, row.names = F)
    },
    contentType = "text/csv"
  )
  
  observeEvent(input$create, {
    
    output$download <- dlHandler
  
    countries = input$country
    type = input$type
    freq = input$freq
    meas = input$measure[1]
    date = as.character(input$date)
    
    if(length(freq) == 0){
      freq = "A"
    }
    
    pars <- list(countries, type, freq, meas, date)
    eq = identical(pars, last.pars)
    
    errors = vector(length = 0)
    
    if(!eq && length(countries) > 1){
      
      show("loading")
      hide("plots")
      
      p = plot_ly()

      first = TRUE
      
      data <- data.frame()
      nms <- vector(length = 0)
      
      for(country in countries){
        
        series = getfunc(country, type = type, freq = freq,
                         meas = meas, start = date[1], end = date[2])
        
        if(class(series) == "ts"){
          
          if(first){

            p <- add_trace(p, x = as.Date.ts(series), y = series, name = country, type = "bar")
            
            data <- series
            nms[1:2] <- c("date",country)
            
            first = FALSE
            
          } else {
            
            p <- add_trace(p, x = as.Date.ts(series), y = series, name = country, type = "bar")
            
            data <- cbind(data,series)
            nms <- c(nms,country)
          }
        } else {
          errors = c(errors, series)
        }
      }
      
      countries = countrycode(countries, "iso3c","country.name")
      countries = countries[!(countries %in% errors)]
      title = paste0(names(which(type.choices == type))," - ", paste(countries, collapse = ", "))
      title = paste0("<b>",title, "</b>")
      
      p <- p %>% layout(title = title,
                        yaxis = list(title = names(which(meas.choices == meas))),
                        xaxis = list(title = ""),
                        titlefont = list(size = 13))
      
      if(length(errors) != 0){
        msgs$errors <<- paste(errors,collapse = ",")
      }
      else {
        msgs$errors <<- ""
      }
      
      output$plotstd <- renderPlotly(p)
      
      show("plots")
      hide("loading")
      
      data <- cbind(data.frame(as.Date.ts(data)),data.frame(data))
      names(data) <- nms
      gdps.data <<- data
      
      last.pars <<- pars
    }
  })
  
  return(msgs)
}