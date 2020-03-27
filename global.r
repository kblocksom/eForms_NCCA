library(shiny)
library(shinyjs)
library(magrittr)
library(purrr)
library(RJSONIO)
library(stringr)
library(writexl)
library(zip)
library(shinyBS)

source('functions/eFormsParseJSON_basic.r')
source('functions/eFormsParseJSONtext.r') 
source('functions/eFormsOrganizeData_byTable_NCCA.r')
source('functions/parseData.R')


metadata <- readRDS("data/metadata.rds")