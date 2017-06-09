#' @author Talitha Speranza \email{talitha.speranza@gmail.com}

loadingModuleInput <- function(id){
  ns = NS(id)
  
  div(
    id = ns("loading"), style = "position:relative;margin-top:50px;left:45%;height:360px;",
    div(class = "loader alert"),
    p("Carregando...", style = "margin-top:15px;margin-left:15px;color:#337ab7;font-weight:bold")
  )
}