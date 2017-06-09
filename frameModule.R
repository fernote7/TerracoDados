#' @author Talitha Speranza \email{talitha.speranza@gmail.com}


frameModuleInput <- function(id, num){
  return(paste0(id,"-fr",num))
}

frameModule <- function(input, output, session, id){

  hide("fr1")
  hide("fr2")
  hide("loading")
   
  observeEvent(input$create, {
    
    if(length(input$country) == 1){
      
      hide("fr1")
      show("fr2")
    }
    else if(length(input$country) > 1){
        
        hide("fr2")
        show("fr1")
    }
  })
}
