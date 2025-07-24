

EcranAdminServer <- function(id,values) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$send_message_vocal,{
        values$text_IA_admin <- input$message_vocal
        # text_IA_admin(input$message_vocal)
      })

      observeEvent(input$reboot, {
        sheet_write(values$id_drive, data = info_enigmes(values),sheet = "db_enigmes")
        sheet_write(values$id_drive, data = info_scans(values),sheet = "db_scans")
        sheet_write(values$id_drive, data = info_IA(values),sheet = "db_IA")
        values$i_etape <- 1
        values$nb_scan <- 0
        values$db_enigmes <- load_db_enigmes(values$id_drive)
        values$db_scans <- load_db_scans(values$id_drive)
        values$db_IA <- load_db_IA(values$id_drive)
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
          filter(Type == "Indice") %>%
          select(ID_bloc,ID_step,ID_enigme,Ecran,FL_Valid) %>%
          mutate(FL_Valid = case_when(
            FL_Valid == 1 ~  gicon("ok"),
            FL_Valid == 0 ~ gicon("remove"),
            TRUE ~ ""
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
                      options = list(lengthChange = FALSE,pageLength = 100, info = FALSE,searching = FALSE, paging=FALSE, fixedHeader=TRUE,ordering=FALSE)
        )})

      # Envois des mails
      output$nb_mails <- renderText({
        paste("Nombre de mails : ",values$nb_mails,"mails")})
      observeEvent(input$active_mails,{
        values$active_mails <- TRUE
      })
      observeEvent(input$desactive_mails,{
        values$active_mails <- FALSE
      })
      observeEvent(input$nvx_mails_button,{
        values$nb_mails <- input$nvx_mails
        updateTextInput(session,"nvx_mails",value = "")
      })
      observe({
        updateProgressBar(
          session = session,
          id = "nb_mails",
          value = values$nb_mails,
          total = 11000000
        )
      })

    }
  )
}

EcranAdminUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(
      ".center {
            display: flex;
            justify-content: center
            }"
    ),
    tags$style(".glyphicon-ok {color:#2b8ee5}
              .glyphicon-remove {color:#e5413b}
              .glyphicon-exclamation-sign {color:#e5413b}
              .glyphicon-flag, .glyphicon-trash {color:#28b728}"
    ),
    h1("Gestion de l'escape room"),
    column(12,
           actionButton(ns("reboot"), "Reboot"),
           actionButton(ns("avancer"), "Avancer d'une étape"),
           actionButton(ns("reculer"), "réculer d'une étape"),
           class = "center",
    ),
    fluidRow(
      h1("Progression de l'escape room"),
      DT::dataTableOutput(ns("synthese_admin")),
      DT::dataTableOutput(ns("db"))
    ),
    fluidRow(
      h1("Envoyer un message vocal"),
      textInput(ns("message_vocal"),label = NULL,placeholder = "Réponse"),
      actionButton(ns("send_message_vocal"),"Envoyer")
    ),
    fluidRow(
      h1("Activer l'envoi de mail"),
      fluidRow(
        column(width = 3,textOutput(ns("nb_mails"))),
        column(width = 6,
               actionButton(ns("active_mails"),"Activer les mails"),
               actionButton(ns("desactive_mails"),"Désactiver les mails")
        )
      ),
      fluidRow(
        column(width = 3,
          textInput(ns("nvx_mails"),label = NULL,
                    placeholder = "Changer le nombre de mails")
        ),
        column(width = 6,actionButton(ns("nvx_mails_button"),"Changer"))
      )
    )
  )
}
