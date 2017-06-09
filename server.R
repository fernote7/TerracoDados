#' @author Talitha Speranza \email{talitha.speranza@gmail.com}

function(input, output, session) {
  
  callModule(frameModule, "oecd", "oecd")
  callModule(frameModule, "wb", "wb")
  
  createAlert(session, "start-alert", "start", title = "Bem vindo!", style = "info",
              content = "Comece a explorar o PIB dos países no app do Terraço! Fique atento: as opções mudam os tipos de gráfico que aparecem. Se escolher apenas um país, dois gráficos aparecerão: um com a série do PIB e outro com os seus componentes no último ano disponível. Se escolher mais de um país, um gráfico de barras será exibido, com as séries do PIB desses países. Repare que você pode baixar as séries (clicando no link 'Baixar Dados') e salvar os gráficos (passe o mouse por cima dos gráficos e veja as opções). Buscamos os dados diretamente nas bases das instituições, então estarão sempre atualizados.", append = FALSE)

  
  createAlert(session, "email-alert", "email", title = "Encontrou um bug?", style = "info",
              content = "Mande um email para o Indiano do T.I. do Terraço! <a href='mailto:talitha.speranza@gmail.com'>Clique aqui para enviar.</a>", append = FALSE)
  
  oecd.countries = get.oecd.countries()
  oecd.meas.A = get.oecd.measures("A")
  oecd.meas.Q = get.oecd.measures("Q")
  oecd.types = get.oecd.types()
  oecd.meas.choices <- callModule(sidebarModule, "oecd", oecd.countries, oecd.types, oecd.meas.A, oecd.meas.Q)
  
  wb.countries = get.wb.countries()
  wb.meas = get.wb.measures("A")
  wb.types = get.wb.types()
  wb.meas.choices <- callModule(sidebarModule, "wb", wb.countries, wb.types, wb.meas)
  
  msgs.oecd.barplot <- callModule(barplotModule, "oecd", oecd.types, oecd.meas.choices, get.oecd.series)
  msgs.wb.barplot <- callModule(barplotModule, "wb", wb.types, wb.meas.choices, get.wb.series)
  
  callModule(messageModule, "oecd", "oecd-side-alert", msgs.oecd.barplot)
  callModule(messageModule, "wb", "wb-side-alert", msgs.wb.barplot)
  
  msgs.oecd.dbplot <- callModule(doubleplotModule, "oecd", oecd.types, oecd.meas.choices, get.oecd.series, get.oecd.comp)
  msgs.wb.dbplot <- callModule(doubleplotModule, "wb", wb.types, wb.meas.choices, get.wb.series, get.wb.comp)
  
  callModule(messageModule, "oecd", "oecd-comp-alert", msgs.oecd.dbplot)
  callModule(messageModule, "wb", "wb-comp-alert", msgs.wb.dbplot)
  
  callModule(messageModule, "oecd", "oecd-gdp-alert", msgs.oecd.dbplot)
  callModule(messageModule, "wb", "wb-gdp-alert", msgs.wb.dbplot)
  
  callModule(tableModule, "oecd", "oecd", get.oecd.tab)
  callModule(tableModule, "wb", "wb", get.wb.tab)
  
  callModule(initModule, "oecd", oecd.meas.A, oecd.types, get.oecd.series, get.oecd.comp)
  callModule(initModule, "wb", wb.meas, wb.types, get.wb.series, get.wb.comp)

  hide("main-loading")
  show("main-content")  
}