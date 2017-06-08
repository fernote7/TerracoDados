#' @author Talitha Speranza \email{talitha.speranza@gmail.com}

fluidPage(
  theme = "sb-admin-2.css",
  useShinyjs(),
  bsAlert("start-alert"),
  loadingModuleInput("main"),
  hidden(
    div(id = "main-content",
      fluidRow(
        column(3, class = "panel panel-primary", style = "border:none;margin:0px;padding:5px;",
               div(class = "panel panel-heading",icon("gear"),"Opções"),
               sidebarModuleInput("oecd"),
               sidebarModuleInput("wb"),
               div(bsAlert("email-alert"),style="font-size:12px")
               #,sidebarModuleInput("imf")
        ),
        column(9,
               tabsetPanel(
                 tabPanel(p(img(height = "15", src = "oecd.png", style = "margin-top:-6px;"),"OCDE", style = "margin-top:10px;"), value=1,
                          div(id = "oecd-plots", style="height:55vh; margin-bottom:50px",
                              doubleplotModuleInput("oecd"), 
                              barplotModuleInput("oecd")),
                          loadingModuleInput("oecd"),
                          tableModuleInput("oecd", "http://stats.oecd.org/")),
                 tabPanel(p(img(height = "15", src = "wb.png", style = "margin-top:-6px;"), "Banco Mundial", style = "margin-top:10px;"), value=2,
                          div(id = "wb-plots", style="height:55vh; margin-bottom:50px", 
                              doubleplotModuleInput("wb"), 
                              barplotModuleInput("wb")),
                          loadingModuleInput("wb"),
                          tableModuleInput("wb", "http://data.worldbank.org/")),
                 # tabPanel(p(img(height = "15", src = "imf.png", style = "margin-top:-6px;"),
                 #            "FMI", style = "margin-top:10px;"), value=3), 
                 id = "conditionedPanels"
               )
        )
      )
    )
  )
)
