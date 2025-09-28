
#' Serveur Questions/Réponses
#'
#' @param id id
#' @param values Valeurs réactives
#' @param local Valeurs locales
#'
#' @returns shiny server
#' @export
EcranQRServer <- function(id,values,local) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$send, {

      bonne_reponses <- info_enigmes(values) %>%
        filter(ID_enigme == i_question(values,local),
               Type == "Question") %>%
        select(starts_with("Reponse")) %>%
        mutate(Reponse_concat = reduce(across(
          everything(), ~ coalesce(., "")), str_c)) %>%
        pull(Reponse_concat)

      user_reponses <- str_flatten(sort(input$reponse), "")
      bonne_reponses <- str_remove_all(str_to_lower(bonne_reponses),"[ _/-]")
      user_reponses <- str_remove_all(str_to_lower(user_reponses),"[ _/-]")

      if (user_reponses == bonne_reponses){

        new_row <- info_enigmes(values) %>% filter(ID_enigme == i_question(values,local)) %>%
          select(ID_bloc,ID_enigme,ID_step,Ecran,Type) %>% unique() %>%
          mutate(CD_admin = "action", timer = Sys.time(),FL_Valid = 1) %>%
          select(CD_admin,timer,ID_bloc,ID_enigme,ID_step,FL_Valid,Ecran,Type)

        if (nrow(new_row) > 0) {
          sheet_append(values$id_drive, data = new_row,sheet = "db_enigmes")
          values$db_enigmes <- load_db_enigmes(values$id_drive)
        }

        updateTextInput(session = session,inputId = "reponse",placeholder = "Réponse",value = "")
        output$text_erreur <- renderUI("")

      }else{

        # Rebooter la step
        id_step <- info_enigmes(values) %>%
          filter(ID_enigme == i_question(values,local),Type == "Question") %>%
          pull(ID_step) %>% unique()

        label_step <- info_enigmes(values) %>%
          filter(ID_step == id_step,Type == "Question",Ecran == local$userEcran) %>%
          pull(LabelStep) %>% unique()

        label_erreur <- info_enigmes(values) %>%
          filter(ID_enigme == i_question(values,local),Type == "Question",
                 Ecran == local$userEcran) %>%
          pull(LabelErreur) %>% unique()

        if (is.na(label_erreur)){
          if (values$language == "fr")
            texte_erreur <- paste0("Erreur, réinitialisation de l'étape ",label_step)
          else
            texte_erreur <- paste0("Fout, stap ",label_step," resetten")
        }else{
          texte_erreur <- label_erreur
        }

        new_row <- actu_enigmes(values) %>% filter(ID_step == id_step,FL_Valid == 1) %>%
          select(ID_bloc,ID_enigme,ID_step,Ecran,Type) %>%
          unique() %>%
          mutate(CD_admin = "action",timer = Sys.time(),FL_Valid = 0) %>%
          select(CD_admin,timer,ID_bloc,ID_enigme,ID_step,FL_Valid,Ecran,Type)

        if (nrow(new_row) > 0) {
          sheet_append(values$id_drive, data = new_row,sheet = "db_enigmes")
          values$db_enigmes <- load_db_enigmes(values$id_drive)
        }

        output$text_erreur <- renderUI(texte_erreur)
      }

    })

  }
  )
}

#' UI Questions/Réponses
#'
#' @param id id
#' @param values Valeurs réactives
#' @param local Valeurs locales
#'
#' @returns shiny ui
#' @export
EcranQRUI <- function(id,values,local) {
  ns <- NS(id)

  values$db_enigmes

  # Check si bloc terminé
  fl_bloc_ok <- reactive({

    bloc_min <- actu_enigmes(values) %>%
      filter(FL_Valid == 0) %>%
      summarise(ID_bloc = min(ID_bloc)) %>% pull()

    bloc_question <- info_enigmes(values) %>%
      filter(Type == local$userType,ID_enigme == i_question(values,local)) %>%
      pull(ID_bloc) %>% unique()

    if (length(bloc_question) == 0) return(FALSE)
    return(bloc_min == bloc_question)
  })

  db <- reactive({
    info_enigmes(values) %>%
      filter(Type == local$userType,ID_enigme == i_question(values,local))
  })

  LabelServer(ns("label"),db(),local$base_url)

  # if (local$userType == "I"){
  #   if (nrow(db_i()) > 0){
  #
  #   }
  # }
  #
  # if (local$userType == "Q"){
  #   if (nrow(db_q()) > 0){
  #     LabelServer(ns("quest_label"),db_q(),local$base_url)
  #   }
  # }

  choix <- function(){
    # temp <- db_q()
    temp <- db()
    choix <- str_c(temp[,grep("Choix",colnames(temp))])
    choix[!is.na(choix)]
  }

  if (fl_bloc_ok()){
    if (local$userType == "I" & nrow(db()) > 0){
      tagList(
        style_escape_theme(),

        # gl_talk_shinyUI(ns("talk")),

        fluidRow(column(12,div(style = "height:50px"))),
        LabelUI(ns("label"))

      )
    } else if (local$userType == "Q" & nrow(db()) > 0){
      tagList(
        # style_global(),
        style_escape_theme(),

        # gl_talk_shinyUI(ns("talk")),

        div(class = "container-narrow",
            div(class = "card question-card",
                h2(class = "category-title", db()$LabelStep),
                if (db()$Format != "rien"){
                  div(class = "answer-group",
                      div(class = "answer-input",
                          if (db()$Format == "radio") {
                            radioButtons(ns("reponse"),label = "",
                                         choices = choix(),
                                         selected = NULL,inline = FALSE)
                            } else if (db()$Format == "select") {
                              selectInput(ns("reponse"),label = "",
                                          choices = choix(),
                                          selected = NULL,multiple = TRUE)
                            } else if (db()$Format == "checkbox") {
                              checkboxGroupInput(ns("reponse"),label = "",
                                                 choices = choix(),
                                                 selected = NULL)
                            } else if (db()$Format == "texte") {
                              textInput(ns("reponse"),label = NULL,
                                        placeholder = "Réponse")
                            }
                      ),
                      actionButton(ns("send"), "Envoyer", class = "btn-answer"),
                      uiOutput(ns("text_erreur"))
                  )
                  },
                uiOutput("code_src_message"),     # message (erreur/ok/info)
                p(class = "question-label", LabelUI(ns("label")))
            )
        )
      )
    }
  }
}
