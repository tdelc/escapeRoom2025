

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
          filter(Type == "Question") %>%
          select(ID_bloc,ID_step,ID_enigme,Ecran_Question = Ecran)

        synthese_indices <- actu_enigmes(values) %>%
          filter(Type == "Question") %>%
          select(ID_bloc,ID_step,ID_enigme,Ecran,FL_Valid) %>%
          mutate(FL_Valid = case_when(
            FL_Valid == 1 ~  gicon("ok"),
            FL_Valid == 0 ~ gicon("remove"),
            TRUE ~ gicon("remove")
          )) %>%
          pivot_wider(id_cols = c(ID_bloc,ID_step,ID_enigme),
                      names_from = Ecran,
                      names_prefix = "Ecran_Indice_",
                      values_from = FL_Valid)

        synthese <- full_join(
          synthese_questions,synthese_indices,
          by = c("ID_bloc","ID_step","ID_enigme")
        )

        DT::datatable(synthese,
                      selection = 'none',
                      rownames = FALSE,
                      escape = FALSE,
                      extensions = c('FixedColumns',"FixedHeader"),
                      options = list(lengthChange = FALSE,pageLength = 100,
                                     info = FALSE,searching = FALSE,
                                     paging=FALSE, fixedHeader=TRUE,ordering=FALSE)
        )})

      # Gestion des mails et des scans
      output$nb_mails_load <- renderText({
        pc_mails_load <- round(100*values$nb_mails_load/values$nb_mails_tot)
        if (values$active_mails_load) check_mails_load <- " (active)"
        else check_mails_load <- ""
        paste0("Avancement du chargement des mails : ",
          format(values$nb_mails_load, big.mark = " ", scientific = F), " / ",
          format(values$nb_mails_tot, big.mark = " ", scientific = F), " (",
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
        actu_scans <- actu_scans(values) %>% select(ID,FL_Valid) %>%
          left_join(info_scans(values) %>% select(ID,Texte))

        paste0("Documents manquants à scanner : ",
          sum(actu_scans$FL_Valid), " / ",
          nrow(actu_scans), " (",
          round(100*sum(actu_scans$FL_Valid)/nrow(actu_scans)),"%)")
      })

      # Chargement des mails
      observeEvent(input$active_mails_load,{
        values$active_mails_load <- TRUE
      })
      observeEvent(input$desactive_mails_load,{
        values$active_mails_load <- FALSE
      })
      observeEvent(input$nvx_mails_load_button,{
        values$nb_mails_load <- as.numeric(input$nvx_mails_load)
        updateTextInput(session,"nvx_mails_load",value = "")
      })

      # Envoi des mails
      observeEvent(input$active_mails_send,{
        values$active_mails_send <- TRUE
      })
      observeEvent(input$desactive_mails_send,{
        values$active_mails_send <- FALSE
      })
      observeEvent(input$nvx_mails_send_button,{
        values$nb_mails_send <- as.numeric(input$nvx_mails_send)
        updateTextInput(session,"nvx_mails_send",value = "")
      })

      # Gestion de l'envois de mails (timer de l'escape room)
      # Que dans l'écran de listing de scan
      observe({
        invalidateLater(1000*5, session)
        isolate({
          heure_fin <- as.numeric(input$heure_fin)
          minute_fin <- as.numeric(input$minute_fin)
          nb_secondes_restant <- temps_restant(heure_fin,minute_fin)$total_secondes
          nb_reload_restant <- nb_secondes_restant / 5

          nb_mails_load <- round(values$nb_mails_tot/nb_reload_restant)
          nb_mails_send <- round(values$nb_mails_load/nb_reload_restant)

          if (!values$active_mails_load) nb_mails_load <- 0
          if (!values$active_mails_send) nb_mails_send <- 0

          values$nb_mails_load <- values$nb_mails_load + nb_mails_load
          values$nb_mails_load <- values$nb_mails_load - nb_mails_send
          values$nb_mails_send <- values$nb_mails_send + nb_mails_send
          values$nb_mails_tot <- values$nb_mails_tot - nb_mails_send

          if (values$nb_mails_load > values$nb_mails_tot){
            values$nb_mails_load <- values$nb_mails_tot
            values$active_mails_load <- F
          }
          if (values$nb_mails_load < 0){
            values$nb_mails_load <- 0
            values$active_mails_send <- F
          }
          if (values$nb_mails_tot < 0){
            values$nb_mails_tot <- 0
            values$active_mails_load <- F
            values$active_mails_send <- F
          }
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
    style_global(),
    div(class = "card center_text",h1("Gestion de l'escape room")),
    fluidRow(
      column(6,
        div(class = "card",
            h2("Progression de l'escape room"),
            actionButton(ns("reboot"), "Reboot"),
            actionButton(ns("avancer"), "Avancer d'une étape"),
            actionButton(ns("reculer"), "Reculer d'une étape"),
            radioButtons(ns('language'), choices = c("fr","nl"),label=NULL),
            DT::dataTableOutput(ns("synthese_admin"))
        )
      ),
      column(6,
             div(class = "card",
                 h2("Progression des scans et mails"),
                 div(class = "panel-title",textOutput(ns("nb_mails_load"))),
                 div(class = "panel-title",textOutput(ns("nb_mails_send"))),
                 div(class = "panel-title",textOutput(ns("nb_scans"))),
                 actionButton(ns("active_mails_load"),"Activer le chargement de mails"),
                 actionButton(ns("desactive_mails_load"),"Désactiver le chargement de mails"),
                 br(),
                 actionButton(ns("active_mails_send"),"Activer l'envoi de mails"),
                 actionButton(ns("desactive_mails_send"),"Désactiver l'envoi de mails"),

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
             )
      )
    ),
    fluidRow(
      column(6,
        div(class = "card",
            h2("Envoyer un message dans SYNAPSE"),
            fluidRow(
              column(6,textInput(ns("message_text"),label = NULL)),
              column(6,actionButton(ns("send_message_text"),"Envoyer"))
            ),
            h2("Envoyer un message vocal"),
            fluidRow(
              column(6,textInput(ns("message_vocal"),label = NULL)),
              column(6,actionButton(ns("send_message_vocal"),"Envoyer"))
            )
        )
      ),
      column(6,
             div(class = "card",
                 h1("Heure de fin de l'escape room"),
                 fluidRow(
                   column(width = 6,textInput(ns("heure_fin"),label = "Heure")),
                   column(width = 6,textInput(ns("minute_fin"),label = "Minute"))
                 )
             )
      )
    )
  )
}
