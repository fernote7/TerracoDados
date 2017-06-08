doubleplotModuleInput <- function(id){
  
  ns <- NS(id)
  
  div(id = frameModuleInput(id, 2), splitLayout(cellWidths = c("60%", "40%"), 
                                  plotlyOutput(ns("plotgdp")), 
                                  div(plotlyOutput(ns("plotcomp")),
                                                   messageModuleInput(ns("ins"))
      )))
}


doubleplotModule <- function(input, output, session, type.choices, meas.choices, getfunc, compfunc){
  
  last.pars.comp <- list()
  last.pars.gdp <- list()
  
  comp.data <- NULL
  gdp.data <- NULL
  
  msgs <- reactiveValues(errors = "", warnings = "")
  
  dlHandler <- downloadHandler(
    filename = 'gdp_data.zip',
    content = function(fname){ 
      fs <- c("gdp_series.csv", "gdp_components.csv")
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      conn1 <- file(fs[1],encoding="UTF-8")
      conn2 <- file(fs[2],encoding="UTF-8")
      
      write.csv(gdp.data, file = conn1, row.names = F)
      write.csv(comp.data, file = conn2, row.names = F)
      
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
  observeEvent(input$create, {
    
    country = input$country
    
    pars.comp <- country
    eq <- identical(pars.comp, last.pars.comp)
    
    if(!eq && length(country) == 1){
      
      data = compfunc(country)
      
      if(!is.na(data)){
        
        show("plotcomp")
        
        s = sum(data$value[2:nrow(data)])
        
        exp = round(data[data$component == "Exportações Líquidas","value"]/s,2)
        an = list()
        
        i <- 1 
        
        if(exp < 0){
          an = list(
            list(x = 0 , y = -0.18, text = paste0("Exportações Líquidas: ", exp),
                 showarrow = F, xref='paper', yref='paper'))
          i <- i + 1
        }
        
        an[[i]] <- list(
          x = 0.5,
          y = 1.4,
          text = paste0('<b>Componentes do PIB em ', data$unit[1] ,' - ', country,' (',data[1,"date"],")</b>"),
          xref = "paper",
          yref = "paper",
          showarrow = F,
          font = list(size = 13.5)
        )
        
        p <- plot_ly(data = data[-1,]) %>%
                add_pie(labels = ~component, values = ~value, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'label+text+percent',
                     text = ~paste('US$', value, ' bilhões'),
                     showlegend = F) %>%
                layout(annotations = an,
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        output$plotcomp <- renderPlotly(p)
        
        last.pars.comp <<- pars.comp
        msgs$warnings <<- ""
        
        comp.data <<- data
      }
      else {
        hide("plotcomp") 
        cnt = countrycode(country,"iso3c","country.name")
        msgs$warnings <<- paste0("Os dados dos componentes do PIB não estão <br> disponíveis para ",cnt)
      }
    }
  })
  
  observeEvent(input$create, {
    
    output$download <- dlHandler

    country = input$country
    type = input$type
    freq = input$freq
    meas = input$measure
    date = as.character(input$date)
    
    pars.gdp <- list(country, type, freq, meas, date)
    eq <- identical(pars.gdp, last.pars.gdp)
    
    if(!eq && length(country) == 1){
      
      hide("plots")
      show("loading")

      ay = NULL
      first = TRUE
      
      p = plot_ly(type = "scatter")

      last.quar = meas.choices[["% Trim. Ant."]]
      last.year = meas.choices[["% Ano Ant."]]
      perc = meas.choices[["%"]]
    

      if(length(meas) == 2 && all(meas %in% c(last.quar,last.year))){
        y2 = FALSE
      } else if(length(meas) == 1 && identical(perc,meas)){
        y2 = FALSE
      } else if(any(meas %in% c(perc, last.quar, last.year))){
        y2 = TRUE
      } else {
        y2 = FALSE
      }

      if(y2){
        ay <- list(
          overlaying = "y",
          side = "right",
          title = "%",
          zeroline = FALSE,
          showgrid = FALSE
        )
      }

      data <- data.frame()
      nms <- vector(length = 0)
      
      for(measure in meas){

        if(length(freq) == 0){
          freq = "A"
        }

        series = getfunc(country, type = type, freq = freq,
                                 meas = measure, start = date[1], end = date[2])

        if(class(series) == "ts"){

          name = names(which(meas.choices == measure))

          if(first){
            p <- add_trace(p, x = as.Date.ts(series), y = series, name = name, mode = "lines+markers")
            first = FALSE
            
            data <- series
            nms <- c("date",name)
          } else {

            if(y2 && (measure %in% c(perc,last.year,last.quar))){
              p <- add_trace(p, x = as.Date.ts(series), y = series, yaxis = "y2", name = name,  mode = "lines+markers")
            }
            else {
              p <- add_trace(p, x = as.Date.ts(series), y = series, name = name,  mode = "lines+markers")
            }
            
            data <- cbind(data,series)
            nms <- c(nms,name)
          }
        }
      }

      title = paste0("<b>", names(which(type.choices == type)), " - ",country, "</b>")

      p <- p %>% layout(title = title,
                        yaxis = list(title = ""),
                        xaxis = list(title = "", rangeslider = list(type = "date")),
                        yaxis2 = ay,
                        titlefont = list(size = 14),
                        legend = list(orientation = 'h', x = 0.17, y = -0.33))

      output$plotgdp <- renderPlotly(p)
      
      show("plots")
      hide("loading")

      data <- cbind(data.frame(as.Date.ts(data)),data.frame(data))
      names(data) <- nms
      gdp.data <<- data
      
      last.pars.gdp <<- pars.gdp

    }
  
  })
  
  return(msgs)
}