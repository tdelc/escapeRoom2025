

#' Serveur Admin
#'
#' @param id id
#' @param values valeurs réactives
#'
#' @returns shiny server
#' @export
EcranAdminServer <- function(id,values) {
  moduleServer(
    id,
    function(input, output, session) {

      updateTextInput(session,"heure_fin",value = lubridate::hour(Sys.time())+1)
      updateTextInput(session,"minute_fin",value = lubridate::minute(Sys.time()))

      observeEvent(input$language,{
        values$language <- input$language
        set_chatlog(chatlog_id = "Question_perso",
                    initial_content = personality_AI(values$language))
      })

      observeEvent(input$send_message_text,{
        values$text_AI_admin <- input$message_text
      })

      observeEvent(input$send_message_vocal,{
        values$vocal_AI_admin <- input$message_vocal
      })

      observeEvent(input$reboot, {
        sheet_write(values$id_drive, data = info_enigmes(values),sheet = "db_enigmes")
        sheet_write(values$id_drive, data = info_scans(values),sheet = "db_scans")
        sheet_write(values$id_drive, data = info_AI(values),sheet = "db_AI")
        values$i_etape <- 1
        values$nb_scan <- 0
        values$db_enigmes <- load_db_enigmes(values$id_drive)
        values$db_scans <- load_db_scans(values$id_drive)
        values$db_AI <- load_db_AI(values$id_drive)
      })

      observeEvent(input$avancer,{
        new_row <- info_enigmes(values) %>%
          filter(ID_enigme == i_question(values)) %>%
          select(ID_bloc,ID_enigme,ID_step,Ecran,Type) %>% unique() %>%
          mutate(CD_admin = "action", timer = Sys.time(),FL_Valid = 1) %>%
          select(CD_admin,timer,ID_bloc,ID_enigme,ID_step,FL_Valid,Ecran,Type)

        if (nrow(new_row) > 0) {
          sheet_append(values$id_drive, data = new_row,sheet = "db_enigmes")
          values$db_enigmes <- load_db_enigmes(values$id_drive)
        }

        values$help_texte <- ""
        values$help_ecran <- ""
      })

      observeEvent(input$reculer,{
        new_row <- info_enigmes(values) %>%
          filter(ID_enigme == i_question(values)-1) %>%
          select(ID_bloc,ID_enigme,ID_step,Ecran,Type) %>% unique() %>%
          mutate(CD_admin = "action", timer = Sys.time(),FL_Valid = 0) %>%
          select(CD_admin,timer,ID_bloc,ID_enigme,ID_step,FL_Valid,Ecran,Type)

        if (nrow(new_row) > 0) {
          sheet_append(values$id_drive, data = new_row,sheet = "db_enigmes")
          values$db_enigmes <- load_db_enigmes(values$id_drive)
        }
      })

      gicon <- function(x) as.character(icon(x, lib = "glyphicon"))

      output$synthese_admin <- renderDT({

        synthese_questions <- info_enigmes(values) %>%
          filter(Type == "Q") %>%
          select(ID_bloc,ID_step,LabelStep_fr,ID_enigme,Reponse1,
                 Info_admin,`Ecran Question` = Ecran)

        synthese_resultats <- actu_enigmes(values) %>%
          filter(Type == "Q") %>%
          mutate(FL_Valid = case_when(
            FL_Valid == 1 ~  gicon("ok"),
            FL_Valid == 0 ~ gicon("remove"),
            TRUE ~ gicon("remove")
          )) %>%
          select(ID_bloc,ID_step,ID_enigme,FL_Valid)

        synthese_indices <- info_enigmes(values) %>%
          filter(Type == "I") %>%
          group_by(ID_bloc,ID_step,ID_enigme) %>%
          summarise(`Ecran Indices` = paste(Ecran,collapse = "-"))

        synthese <- synthese_questions %>%
          full_join(synthese_resultats,by = c("ID_bloc","ID_step","ID_enigme")) %>%
          full_join(synthese_indices,by = c("ID_bloc","ID_step","ID_enigme")) %>%
          select(ID_bloc,ID_step,LabelStep_fr,ID_enigme,Info_admin,Reponse1,
                 FL_Valid,`Ecran Question`,`Ecran Indices`)

        DT::datatable(synthese,
                      selection = 'none',
                      rownames = FALSE,
                      escape = FALSE,
                      extensions = c('FixedColumns',"FixedHeader"),
                      options = list(lengthChange = FALSE,pageLength = 100,
                                     info = FALSE,searching = FALSE,
                                     paging=FALSE, fixedHeader=TRUE,ordering=FALSE)
        )})

      # Aide aux PJ dans les énigmes
      observeEvent(input$help_send,{
        values$help_ecran <- input$help_ecran
        values$help_texte <- input$help_texte
      })

      # Gestion des mails et des scans
      output$nb_mails_load <- renderText({
        pc_mails_load <- round(100*values$nb_mails_load/values$nb_citoyens_act)
        if (values$active_mails_load) check_mails_load <- " (active)"
        else check_mails_load <- ""
        paste0("Avancement du chargement des mails : ",
          format(values$nb_mails_load, big.mark = " ", scientific = F), " / ",
          format(values$nb_citoyens_act, big.mark = " ", scientific = F), " (",
          pc_mails_load,"%) ",check_mails_load)
      })

      output$nb_mails_send <- renderText({
        pc_mails_send <- round(100*values$nb_mails_send/values$nb_mails_load)
        if (values$active_mails_send) check_mails_send <- " (active)"
        else check_mails_send <- ""
        if (values$nb_mails_load == 0) pc_mails_send <- 0
        paste0("Avancement des envois de mails : ",
          format(values$nb_mails_send, big.mark = " ", scientific = F), " / ",
          format(values$nb_mails_load, big.mark = " ", scientific = F), " (",
          pc_mails_send,"%) ",check_mails_send)
      })

      # Nombre de scans
      output$nb_scans <- renderText({
        actu_scans <- info_scans(values) %>%
          select(ID,Texte) %>%
          left_join(actu_scans(values) %>% select(ID,FL_Valid))

        values$nb_citoyens_act <- values$nb_citoyens_tot -
          values$nb_mails_per_scan * sum(actu_scans$FL_Valid)

        paste0("Documents manquants à scanner : ",
          sum(actu_scans$FL_Valid), " / ",
          nrow(actu_scans), " (",
          round(100*sum(actu_scans$FL_Valid)/nrow(actu_scans)),"%)")
      })

      # Chargement des mails
      observeEvent(input$active_mails_load,{
        values$active_mails_load <- input$active_mails_load
      })
      observeEvent(input$nvx_mails_load_button,{
        values$nb_mails_load <- as.numeric(input$nvx_mails_load)
        updateTextInput(session,"nvx_mails_load",value = "")
      })
      # Envoi des mails
      observeEvent(input$active_mails_send,{
        values$active_mails_send <- input$active_mails_send
      })
      observeEvent(input$nvx_mails_send_button,{
        values$nb_mails_send <- as.numeric(input$nvx_mails_send)
        updateTextInput(session,"nvx_mails_send",value = "")
      })

      # Gestion de l'envois de mails (timer de l'escape room)
      # Que dans l'écran de listing de scan

      observeEvent(input$heure_fin,{
        values$heure_fin <- as.numeric(input$heure_fin)
      })

      observeEvent(input$minute_fin,{
        values$minute_fin <- as.numeric(input$minute_fin)
      })

      observe({
        invalidateLater(5000, session)
        values$db_scans <- load_db_scans(values$id_drive)
      })

      observe({
        invalidateLater(1000*5, session)
        isolate({
          heure_fin <- as.numeric(input$heure_fin)
          minute_fin <- as.numeric(input$minute_fin)
          nb_secondes_restant <- temps_restant(heure_fin,minute_fin)$total_secondes

          values$nb_secondes_restant <- nb_secondes_restant

          nb_reload_restant <- nb_secondes_restant / 5

          nb_mails_load <- round(values$nb_citoyens_act/nb_reload_restant)
          nb_mails_send <- round(values$nb_mails_load/nb_reload_restant)

          if (!values$active_mails_load) nb_mails_load <- 0
          if (!values$active_mails_send) nb_mails_send <- 0

          values$nb_mails_load <- values$nb_mails_load + nb_mails_load
          values$nb_mails_load <- values$nb_mails_load - nb_mails_send
          values$nb_mails_send <- values$nb_mails_send + nb_mails_send
          # values$nb_mails_tot <- values$nb_mails_tot - nb_mails_send

          if (values$nb_mails_load > values$nb_citoyens_act){
            values$nb_mails_load <- values$nb_citoyens_act
            values$active_mails_load <- F
          }
          if (values$nb_mails_load < 0){
            values$nb_mails_load <- 0
            values$active_mails_send <- F
          }
          # if (values$nb_citoyens_act < 0){
          #   values$nb_mails_tot <- 0
          #   values$active_mails_load <- F
          #   values$active_mails_send <- F
          # }
        })
      })
    }
  )
}

#' UI Admin
#'
#' @param id id
#'
#' @returns shiny ui
#' @export
EcranAdminUI <- function(id) {
  ns <- NS(id)

  tagList(
    # style_global(),
    style_admin_theme(),
    fluidRow(
      column(10,div(class = "card center_text",h1("Gestion de l'escape room"))),
      column(2,div(class = "card center_text",
                   radioButtons(ns('language'), choices = c("fr","nl"),
                                 label="Choisir la langue",inline=T)))
    ),
    fluidRow(
      column(8,
        div(class = "card",
            h2("Progression de l'escape room"),
            actionButton(ns("reboot"), "Reboot"),
            actionButton(ns("avancer"), "Avancer d'une étape"),
            actionButton(ns("reculer"), "Reculer d'une étape"),
            DT::dataTableOutput(ns("synthese_admin")),
            br(),
            fluidRow(
              column(3,textInput(ns("help_texte"),label="Texte d'aide")),
              column(3,textInput(ns("help_ecran"),label="Écran")),
              column(3,actionButton(ns("help_send"), "Envoyer message aide"))
            )
        )
      ),
      column(4,
             div(class = "card",
                 h2("Heure de fin de l'escape room"),
                 fluidRow(
                   column(width = 6,textInput(ns("heure_fin"),label = "Heure")),
                   column(width = 6,textInput(ns("minute_fin"),label = "Minute"))
                 )
             ),
             div(class = "card",
                 h2("Progression des scans et mails"),
                 div(class = "panel-title",textOutput(ns("nb_mails_load"))),
                 div(class = "panel-title",textOutput(ns("nb_mails_send"))),
                 div(class = "panel-title",textOutput(ns("nb_scans"))),
                 materialSwitch(ns("active_mails_load"),status = "primary",
                                label = "Activer le chargement de mails",value = F),
                 materialSwitch(ns("active_mails_send"), status = "danger",
                                label = "Activer l'envoi de mails",value = F),
                 fluidRow(
                   column(width = 8,textInput(ns("nvx_mails_load"),
                                                     label = NULL,
                                    placeholder = "Changer le nombre de mails chargés")
                   ),
                   column(width = 4,actionButton(ns("nvx_mails_load_button"),"Changer"))
                 ),
                 fluidRow(
                   column(width = 8,textInput(ns("nvx_mails_send"),
                                              label = NULL,
                                              placeholder = "Changer le nombre de mails envoyés")
                   ),
                   column(width = 4,actionButton(ns("nvx_mails_send_button"),"Changer"))
                 )
             ),
             div(class = "card",
                 fluidRow(
                   column(6,textInput(ns("message_text"),
                                      label = "Envoyer un message dans SYNAPSE")),
                   column(6,actionButton(ns("send_message_text"),"Envoyer"))
                 ),
                 fluidRow(
                   column(6,textInput(ns("message_vocal"),
                                      label = "Envoyer un message vocal")),
                   column(6,actionButton(ns("send_message_vocal"),"Envoyer"))
                 )
             )
      )
    )
  )
}
