#' @author Talitha Speranza \email{talitha.speranza@gmail.com}

require(OECD)
require(countrycode)

get.oecd.measures <- function(freq){
  
  if(freq == "A"){
    return(list("USD (mil)" = "CXC","USD Const. (mil)" = "VXCOB","PPP" = "VPVOB","%" = "G"))
  } else if(freq == "Q"){
    return(list("Nominal" = "CPCARSA","Real" = "VPVOBARSA","% Trim. Ant." = "GPSA","% Ano Ant." = "GYSA"))
  } else {
    return(NULL)
  }
}

get.oecd.types <- function(){
  list("PIB" = "", "PIB per capita" = "H")
}

get.oecd.countries <- function(){
  
  codes = strsplit("AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+BRA+CHN+COL+CRI+IND+IDN+LTU+PER+RUS+ZAF","\\+")[[1]]
  cnts <- countrycode(codes,"iso3c", "country.name")
  oecd_cnts <- as.list(codes)
  names(oecd_cnts) <- cnts
  
  return(oecd_cnts)
}

get.oecd.comp <- function(country){
  
  query = paste0(country, ".B1_GE+P31S14_S15+P3S13+P51+P52_P53+B11+DB1_GE.VPVOB")
  data <- get_dataset("SNA_TABLE1",filter = query, start_time = "2010-01-01", end_time = as.character(Sys.Date()))
  data = as.data.frame(data)
  max_date = max(data[,"obsTime"])
  data = data[data$obsTime == max_date,c("TRANSACT","obsTime","obsValue")]
  names(data) = c("component","date","value")
  
  data$component[data$component == "B1_GE"] <- "Total"
  data$component[data$component == "P31S14_S15"] <- "Consumo - Famílias"
  data$component[data$component == "P3S13"] <- "Consumo - Governo"
  data$component[data$component == "P51"] <- "Formação Bruta de Capital"
  data$component[data$component == "P52_P53"] <- "Variação de Estoques"
  data$component[data$component == "B11"] <- "Exportações Líquidas"
  data$component[data$component == "DB1_GE"] <- "Discrepância Estatística"
  
  target = c("Total","Discrepância Estatística", "Consumo - Famílias","Consumo - Governo","Formação Bruta de Capital","Variação de Estoques","Exportações Líquidas")
  data = na.omit(data[match(target,data$component),])
  
  data[2,"value"] = -data[2,"value"]
  
  s = sum(data[-1,"value"])
  data = rbind(data, c("Não Disponível","",data[1,"value"] - s))
  data$value = round(as.numeric(data$value)/100,2)
  
  
  if(any(is.na(data$value))){
    return(NA)
  } else {
    data <- cbind(data,"PPP")
    names(data)[4] <- "unit"
    return(data)
  }
}

get.oecd.series <- function(country, type, freq, meas, start, end){
  
  if(freq == "A"){
    ds = "SNA_TABLE1"
    f = ""
    freq = 1
  } else if(freq == "Q"){
    ds = "QNA"
    f = ".Q"
    freq = 4
  } else {
    ds = "SNA_TABLE1"
    f = ""
    freq = 1
  }
  
  ## SNA_TABLE1/BRA.B1_GA.G
  
  if(type == "H" && !(meas %in% c("G","GYSA","GPSA"))){
    meas = paste0("H",meas)
  }
  
  data = tryCatch({
    
      query = paste0(country,".B1_GE.",meas,f)
      data <- get_dataset(ds,filter = query, start_time = start, end_time = end)
      start = strsplit(gsub("-Q","-",data[1,"obsTime"]),"-")[[1]]
      data = ts(round(data[,"obsValue"],2), start = as.integer(start), frequency = freq)
      
      },
      error = function(e){
        
        return(countrycode(country,"iso3c","country.name"))
      }
  )

  return(data)
}

get.oecd.tab <- function(country){
  
  query = paste0("EVOPOP_T1+SIZEGDP_T1+EVOGDP_T1+PRODINCOM_G1+INCINEQUAL_T1A+CPI_T1A+LTINTRST_T1+LIFEEXPY_G1.",country)
  lvls = c("Taxa de Inflação","Crescimento do PIB Real","População","Gini","Expectativa de Vida","Juros de Longo Prazo","PIB por Horas Trabalhadas","PIB per capita")
  
  data = tryCatch({
    
        data <- get_dataset("CSPCUBE",filter =  query, start_time = "2012-01-01", end_time = as.character(Sys.Date()))
        
        data$SUB[data$SUB == "CPI_T1A"] <- lvls[1]
        data$SUB[data$SUB == "EVOGDP_T1"] <- lvls[2]
        data$SUB[data$SUB == "EVOPOP_T1"] <- lvls[3]
        data$SUB[data$SUB == "INCINEQUAL_T1A"] <- lvls[4]
        data$SUB[data$SUB == "LIFEEXPY_G1"] <- lvls[5]
        data$SUB[data$SUB == "LTINTRST_T1"] <- lvls[6]
        data$SUB[data$SUB == "PRODINCOM_G1"] <- lvls[7]
        data$SUB[data$SUB == "SIZEGDP_T1"] <- lvls[8]
                
        data$UNIT[data$UNIT == "HAB"] <- "" 
        data$UNIT[data$UNIT == "GRWH"] <- "%"
        data$UNIT[data$UNIT == "PC"] <- "%"
        data$UNIT[data$UNIT == "0_TO_1"] <- ""
        data$UNIT[data$UNIT == "YR"] <- "anos"
        
        data$SUB <- as.factor(data$SUB)
        data = data[is.na(data$obsValue) == F,]
        df = NULL
        
        for(stat in levels(data$SUB)){
          
          s = data[data$SUB == stat,]
          
          max = max(s$obsTime)
          last = s[s$obsTime == max,"obsValue"]
          unit = s[s$obsTime == max,"UNIT"]
          last = round(last,2)
          
          val = paste0(last," ",unit," (",max,")")
          
          df = rbind(df,c(stat,val))
        }
        
        nms = as.character(df[,1])
        mis = lvls[!(lvls %in% nms)]
        
        if(is.na(mis[1])){
          mis = NULL
        }
        
        nms = c(nms,mis)
        vals = c(df[,2],rep("-",length(mis)))
        
        data = data.frame(t(vals))
        names(data) = t(nms)
        data = data[lvls]
        
        data = cbind(countrycode(country,"iso3c","country.name"), data)
        names(data)[1] = "País"
        
        data
      },
      error = function(e){
        
        data = data.frame(t(c(countrycode(country,"iso3c","country.name"),rep("-",8))))
        names(data) = c("País", lvls)
        
        return(data)
      }
  )
  
  return(data)
}



