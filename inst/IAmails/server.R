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
library(sortable)
# library(escapeRoom)

source(".env.R")

# Google
gl_auth("google_key.json")
load("token_gs4")
gs4_auth(token=token_gs4)

# OpenAI
openai_api_key(id_chatgpt)

set_chatlog(chatlog_id = "Question_perso",initial_content = personality_AI())
set_chatlog(chatlog_id = "Fin_du_jeu",initial_content = "")

# numéro de la question
# i_etape <- reactiveVal(1)

# texte explicatif sous la question
text <- reactiveVal("")

# DB
values <- reactiveValues(db_enigmes=load_db_enigmes(id_drive),
                         db_scans=load_db_scans(id_drive),
                         db_AI=load_db_AI(id_drive),
                         db_trad=load_db_trad(id_drive),
                         i_etape = 1,
                         nb_scan = 0,
                         message_vocal = "",
                         texte_output = "",
                         language = "fr",
                         text_scan = "",
                         text_AI_admin = "",
                         vocal_AI_admin = "",
                         text_AI = "",
                         id_drive = id_drive,
                         id_chatgpt = id_chatgpt,
                         nb_mails_tot = 11000000,
                         nb_mails_per_scan = 11000000 /
                           nrow(load_db_scans(id_drive) %>%
                                  filter(CD_admin == "init")),
                         nb_mails_load = 0,
                         nb_mails_send = 0,
                         active_mails_load = FALSE,
                         active_mails_send = FALSE,
                         heure_fin = lubridate::hour(Sys.time())+1,
                         minute_fin = lubridate::minute(Sys.time()),
                         help_ecran = "",
                         help_texte = ""
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
      EcranAIServer("EcranAI",values)
      EcranSourceServer("EcranSource",values,local)
    })

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
  output$AI <- renderUI({
    if (local$userType == "AI")
      EcranAIUI("EcranAI",values)
  })

  # Source
  output$source <- renderUI({
    if (local$userType == "O")
      EcranSourceUI("EcranSource",values,local)
  })
}
)

