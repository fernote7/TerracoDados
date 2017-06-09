#' @author Talitha Speranza \email{talitha.speranza@gmail.com}

library(shiny)
library(OECD)
library(countrycode)
library(DT)
library(zoo)
library(shinyjs)
library(plotly)
library(V8)
library(shinyBS)
library(wbstats)

source("sidebarModule.R", encoding = 'utf-8')
source("messageModule.R", encoding = 'utf-8')
source("tableModule.R", encoding = 'utf-8')
source("doubleplotModule.R", encoding = 'utf-8')
source("barplotModule.R", encoding = 'utf-8')
source("frameModule.R", encoding = 'utf-8')
source("loadingModule.R", encoding = 'utf-8')
source("oecd.R", encoding = 'utf-8')
source("wb.R", encoding = "utf-8")
source("initModule.R", encoding = "utf-8")

