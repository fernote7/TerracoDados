require(wbstats)
require(countrycode)

get.wb.countries <- function(){
  
  data <- wbcountries()
  
  suppressWarnings(codes <- countrycode(data$country,"country.name", "iso3c"))
  codes <- na.omit(codes)
  wb.countries <- as.list(codes)
  names(wb.countries) <- countrycode(codes, "iso3c", "country.name")
  
  return(wb.countries)
}

get.wb.types <- function(){
  list("PIB" = "MKTP", "PIB per capita" = "PCAP")
}

get.wb.measures <- function(freq){
  
  if(freq == "A"){
    return(list("USD" = "CD","USD Const." = "KD","PPP" = "PP.KD","%" = "KD.ZG"))
  } else {
    return(NULL)
  }
}

get.wb.tab <- function(country){
  
  query = c("FP.CPI.TOTL.ZG", "NY.GDP.MKTP.KD.ZG", "SP.POP.TOTL","SI.POV.GINI","SP.DYN.LE00.IN", "FR.INR.RINR", "SL.GDP.PCAP.EM.KD", "NY.GDP.PCAP.KD")
  lvls = c("Taxa de Inflação","Crescimento do PIB Real","População","Gini","Expectativa de Vida","Taxa de Juros Real","PIB por Pessoa Empregada","PIB per capita")
  
  data <- tryCatch({
    
    data <- wb(country = country, indicator = query, mrv = 1)
    
    data$indicator <- lvls
    data$value <- round(data$value, 2)
    data$value[3] = as.integer(data$value[3])
    data <- data[,c(1,2,4)]
    
    data[c(1,2,6),"value"] <- paste0(data[c(1,2,6),"value"], "%")
    data[5,"value"] <- paste(data[5,"value"], "anos")
    data[c(7,8),"value"] <- paste(data[c(7,8),"value"], "[PPP]")
    data[, "value"] <- paste0(data[,"value"], " (", data[,"date"], ")")
    
    nms = data[,"indicator"]
    data <- data.frame(t(data[,"value"]))
    names(data) <- nms
    
    cnt <- countrycode(country,"iso3c","country.name")
    data <- cbind(cnt,data)
    names(data)[1] <- "País"
    
    data
  },
  error = function(e){
    
    data = data.frame(t(c(countrycode(country,"iso3c","country.name"),rep("-",8))))
    names(data) = c("País", lvls)
    
    return(data)
  })
  
  data
}

get.wb.series <- function(country, type, freq, meas, start, end){
   
  code = paste0("NY.GDP.",type, ".", meas)
  start = format(as.Date(start),"%Y")
  end = format(as.Date(end),"%Y")
  
  ret <- tryCatch({
    data <- wb(country = country, indicator = code, startdate = start, enddate = end)
    data <- round(data[,"value"],2)
    data <- data[length(data):1]
    ts(data, start = as.integer(start), end = as.integer(end), frequency = 1)
  },
  error = function(e){
    countrycode(country,"iso3c","country.name")
  })
  
  return(ret)
}

get.wb.comp <- function(country){
  
  query = c("NY.GDP.MKTP.KD","NE.CON.PETC.KD","NE.CON.GOVT.KD","NE.GDI.FTOT.KD","NE.EXP.GNFS.KD","NE.IMP.GNFS.KD") 
  data <- wb(country = country, indicator = query, mrv = 1)
  
  data[5,"value"] <- data[5,"value"] - data[6,"value"]
  data <- data[-6,]
  data$indicator <- c("Total","Consumo - Famílias", "Consumo - Governo", "Formação Bruta de Capital Fixo", "Exportações Líquidas")
  data$value <- round(data$value/(10^9), 2)
  data <- data[,c(4,2,1)]
  
  s = sum(data$value[2:5])
  na = round(data$value[1] - s,2) 
  
  data <- rbind(data, c("Não Disponível", data$date[1], na))
  
  data <- cbind(data,"USD Const.")
  names(data)[c(1,4)] = c("component","unit")
  data$value <- as.numeric(data$value)
  
  return(data)
}