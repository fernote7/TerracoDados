#' @author Talitha Speranza \email{talitha.speranza@gmail.com}

dfs <- list("oecd" = NULL, "wb" = NULL, "imf" = NULL)
lens <- list("oecd" = 0, "wb" = 0, "imf" = 0)

tableModuleInput <- function(id, webpage) {
  ns <- NS(id)
  
  tagList(DT::dataTableOutput(ns("table")),
          HTML(paste0('<p>Fonte: <a href="',webpage,'">',toupper(id),'</a></p>')))
}

tableModule <- function(input, output, session, id, tabfunc) {
  
  data <- reactive({
    
    new.len = length(input$country)
    
    if(new.len != 0){
      
      ct = input$country[[new.len]]
      temp = tabfunc(ct)
      
      if(is.null(dfs[[id]])){
        dfs[[id]] <<- temp
      } else {
        
        if(new.len > lens[[id]]){
          dfs[[id]] <<- rbind(temp,dfs[[id]])
        } else {
          last = as.vector(dfs[[id]][,1])
          rmvd = !(last %in% countrycode(input$country,"iso3c", "country.name"))
          
          if(length(last[rmvd]) > 0){
            rmvd = last[rmvd][[1]]
          }
          
          dfs[[id]] <<- dfs[[id]][dfs[[id]][,1] != rmvd,]
        }
      }
      
      lens[[id]] <<- new.len
    }
    else {
      dfs[[id]] <<- NULL
    }
    
    dfs[[id]]
    print(dfs[[id]])
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable(data(), options = list(paging = F, searching = F), class = 'cell-border stripe', rownames = F)
  )
}