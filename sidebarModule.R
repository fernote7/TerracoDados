sidebarModuleInput <- function(id) {
  ns <- NS(id)
  
  freq.radio = radioButtons(ns("freq"), p(icon("line-chart"),"Frequência"), 
                            choices = list("Anual" = "A", "Trimestral" = "Q"), selected = "A", inline = T)
  
  if(id == "oecd"){
    panel <- 1
  } else if(id == "wb"){
    panel <- 2
    freq.radio = NULL
  } else {
    panel <- 3
  }
  
  conditionalPanel(class = "panel panel-body", style = "border:none;",
                   condition= paste0("input.conditionedPanels==", panel),
                   selectInput(ns("country"), p(icon("globe"),"Países"), 
                               choices = list(), multiple = T),
                   checkboxGroupInput(ns("measure"), p(icon("scale", lib = "glyphicon"),"Medida"), choices = list()),
                   radioButtons(ns("type"), p(icon("toggle-on"),"Tipo"), 
                                choices = list(), selected = "", inline = T),
                   freq.radio,
                   dateRangeInput(ns("date"), p(icon("calendar"),"Período"),
                                  start = Sys.Date() - 10*365, end = Sys.Date(), max = Sys.Date(),
                                  separator = " até ", format = "dd/mm/yyyy",
                                  startview = 'year', language = 'pt', weekstart = 1, width = "80%"),
                   downloadLink(ns("download"), label = p(icon("download-alt", lib = "glyphicon"),"Baixar dados"), style = "float:left; margin-top:10px"),
                   actionButton(ns("create"),p(icon("bar-chart-o"),"Criar gráfico", style = "padding:0px 5px 0px 5px; margin:0px;"), 
                                style = "float:right;", class = "btn-primary"),
                   messageModuleInput(ns("side"))
  )
}


sidebarModule <- function(input, output, session, countries, types, meas.A, meas.Q = NULL) {
  
  meas.choices <- reactive({
    
    if(length(input$freq) != 0){
      if(input$freq == "A"){
        meas.A
      } else if(input$freq == "Q"){
        meas.Q
      } else {
        NULL
      }
    } else {
      meas.A 
    }
  })
  
  
  if(!is.null(meas.Q)){
    
    observeEvent(input$freq, {
      
      if(input$freq == "A"){
        meas.choices <- meas.A
      } else if(input$freq == "Q"){
        meas.choices <- meas.Q
      } 
      
      updateCheckboxGroupInput(session,"measure", inline = T,
                               choices = meas.choices, selected = meas.choices[[1]])
      
    })
    
  } else {
    
    updateCheckboxGroupInput(session,"measure", inline = T,
                             choices = meas.A, selected = meas.A[[1]])
  }
  
  updateSelectInput(session, "country",
                    choices = countries, selected = "BRA")
  
  updateRadioButtons(session, "type",
                    choices = types, selected = types[[1]], inline = T)
  
  return(meas.choices)
  
}

