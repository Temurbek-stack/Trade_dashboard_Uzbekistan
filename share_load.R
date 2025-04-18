# ## set working directory to shiny folder
# library("rstudioapi")     
# setwd(dirname(getActiveDocumentContext()$path))    
### Share load should be sourced by both ui and server.
##  load library --------------------
library(rjson)
library(shinydashboard)
library(shiny)
library(shinythemes)
#library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(highcharter)
library(plotly)
library(lubridate)
library(stringr)
library(withr)
#library(treemap)
library(DT)
#library(shinyBS)
#library(shinyjs)
#library(WDI)
#library(geosphere)
library(shinycssloaders)
options(spinner.color="#006272")
library(timevis)
#library(RCurl)
#library(jsonlite)
#library(comtradr)
#library(memoise)
#library(networkD3)
#library(promises)
#library(future)
#plan(multisession)
library("viridis")
library("geojsonio")
library("sf")
library(openxlsx)
library(leaflet)
library(leaflet.minicharts)
#library(magrittr)





## load data
load('data/ex_im_uz.rda')
load('data/trade_data_uz.rda')
load('data/trade_balance_data_uz.rda')
load('data/trade_partners.rda')
load('data/KeyProdExp.rda')
load('data/KeyProdExpPerc.rda')
load('data/KeyProdImpPerc.rda')
load('data/KeyProdImp.rda')
load('data/trade_partners_with_sf.rda')
load('data/df_countries_95_21_totals.rda')
load('data/df_countries_95_21_goods.rda')
load('data/df_countries_95_21_services.rda')


world_as_sf <- read_sf("world-shape-files")
## sf to geojson. highcharter uses geojson



maxYear <- max(ex_im_uz$Period)-1
maxYear <- gsub('q1', 'March', maxYear)
maxYear <- gsub('q2', 'June', maxYear)
maxYear <- gsub('q3', 'September', maxYear)
maxYear <- gsub('q4', 'December', maxYear)

### CAGR function------------------
CAGR <- 
  function (ratio, period, digits = 1) {
    round((exp(log(ratio)/period) - 1) * 100, digits)
  }

