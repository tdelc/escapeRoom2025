library(shiny)
library(shinyWidgets)
library(shinyjs)
library(jsonlite)
library(tidyverse)
library(stringr)
library(dplyr)
library(DT)
library(googlesheets4)
library(TheOpenAIR)
library(text2speech)
library(googleLanguageR)
library(later)

source(".env.R")

# Google
gl_auth("google_key.json")
load("token_gs4")
gs4_auth(token=token_gs4)

# OpenAI
openai_api_key(id_chatgpt)

# numéro de la question
# i_etape <- reactiveVal(1)

# texte explicatif sous la question
text <- reactiveVal("")

# DB
values <- reactiveValues(db_enigmes=load_db_enigmes(id_drive),
                         db_scans=load_db_scans(id_drive),
                         db_IA=load_db_IA(id_drive),
                         i_etape = 1,
                         nb_scan = 0,
                         message_vocal = "",
                         texte_output = "",
                         text_scan = "",
                         text_IA_admin = "",
                         text_IA = "",
                         id_drive = id_drive,
                         id_chatgpt = id_chatgpt,
                         nb_mails = 0,
                         active_mails = FALSE,
                         )

shinyServer(function(input, output, session) {

  # GESTION USERS
  local <- reactiveValues(secret = sample(6, 1))

  observe({
    port <- session$clientData$url_port
    if (port != "") port <- paste0(":",port)
    base_url <- paste0(session$clientData$url_protocol,"//",
                       session$clientData$url_hostname,
                       port,
                       session$clientData$url_pathname)

    isolate({
      local$base_url <- base_url
      local$userType <- str_extract(session$clientData$url_search,"[A-Z]+")
      local$userType <- ifelse(is.na(local$userType),"",local$userType)

      # ATTENTION, peut être problématique pour gérer plusieurs écrans de scan
      local$userEcran <- str_extract(session$clientData$url_search,"[0-9]+")
      local$userEcran <- ifelse(is.na(local$userEcran),"",local$userEcran)

      EcranAdminServer("EcranAdmin",values)
      EcranControleServer("EcranControle",local$base_url)
      EcranScanServer("EcranScan",values,local)

      # id <- paste0("EcranQR",local$userType,local$userEcran)
      # EcranQRServer(id,values,local)
      EcranQRServer("EcranQR",values,local)
      EcranIAServer("EcranIA",values)

      EcranSourceServer("EcranSource",values,local)
    })

  })

  # Gestion de l'envois de mails (timer de l'escape room)
  # Que dans l'écran de listing de scan
  observe({
    if (local$userType == "L" & values$active_mails){
      invalidateLater(1000*runif(1,1,60), session)
      isolate({
        values$nb_mails <- values$nb_mails + runif(1,10,1000)
      })
    }
  })

  ### UI

  output$admin <- renderUI({
    if (local$userType == "A")
      EcranAdminUI("EcranAdmin")
  })

  output$controle <- renderUI({
    if (local$userType == "C")
      EcranControleUI("EcranControle")
  })

  # Scanner
  output$scan <- renderUI({
    if (local$userType %in% c("L","S"))
      EcranScanUI("EcranScan",values,local)
  })

  # Q & R
  output$questrep <- renderUI({
    if (local$userType %in% c("Q","I"))
      EcranQRUI("EcranQR",values,local)
  })

  # Chat
  output$IA <- renderUI({
    if (local$userType == "IA")
      EcranIAUI("EcranIA")
  })

  # Source
  output$IA <- renderUI({
    if (local$userType == "O")
      EcranSourceUI("EcranSource")
  })
}
)

