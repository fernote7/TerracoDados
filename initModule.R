initModule <- function(input, output, session, meas.choices, type.choices, getfunc, compfunc){
  
  country = "BRA"
  type = type.choices[[1]]
  freq = "A"
  meas = meas.choices[[1]]
  date = c(as.Date("2008-01-01"),as.Date("2017-01-01"))
  
  series = getfunc(country, type = type, freq = freq,
                     meas = meas, start = date[1], end = date[2])

      
  p1 <- plot_ly(type = "scatter") %>%
     add_trace(p, x = as.Date.ts(series), y = series, name = "USD", mode = "lines+markers") %>%
        layout(title = "<b>PIB - BRA</b>",
               yaxis = list(title = ""),
               xaxis = list(title = "", rangeslider = list(type = "date")),
               titlefont = list(size = 14),
               legend = list(orientation = 'h', x = 0.17, y = -0.33))
  
  
  output$plotgdp <- renderPlotly(p1)
  series = cbind(data.frame(as.Date.ts(series)),data.frame(series))
  
  data = compfunc(country)
  
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

  p2 <- plot_ly(data = data[-1,]) %>%
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

  output$plotcomp <- renderPlotly(p2)
  show("fr2")
  
  
  output$download <- downloadHandler(
    filename = 'gdp_data.zip',
    content = function(fname){ 
      fs <- c("gdp_series.csv", "gdp_components.csv")
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      conn1 <- file(fs[1],encoding="UTF-8")
      conn2 <- file(fs[2],encoding="UTF-8")
      
      write.csv(series, file = conn1, row.names = F)
      write.csv(data, file = conn2, row.names = F)
      
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
}