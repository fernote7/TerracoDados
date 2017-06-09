#' @author Talitha Speranza \email{talitha.speranza@gmail.com}

messageModuleInput <- function(id){
  ns <- NS(id)

  div(bsAlert(ns("alert")), style = "clear:both;margin-top:60px;")
}

messageModule <- function(input, output, session, id, msg){
  
  observeEvent(input$create, {
    
    error.id =  paste0(id,"-error")
    warn.id = paste0(id, "-warn")
    
    closeAlert(session, error.id)
    closeAlert(session, warn.id)
    
    Sys.sleep(1)
    
    if(msg$errors != ""){

      createAlert(session, id, error.id, title = "É uma pena", style = "danger",
                  content = paste("Não há valores para", msg$errors, "nas bases da instituição."), append = FALSE)

    } 
    
    if(msg$warnings != ""){
      
      createAlert(session, id, warn.id, title = "Dados não disponíveis", style = "warning",
                  content = msg$warnings, append = FALSE)
      
    } 

  })
  
}