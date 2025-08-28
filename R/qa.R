i_question <- function(values,local=TRUE){
  actu <- actu_enigmes(values) %>% filter(FL_Valid == 0)
  if (is.list(local)){
    actu <- actu %>% filter(Ecran == local$userEcran)
  }
  actu %>% summarise(ID_enigme=min(ID_enigme)) %>% pull(ID_enigme)
}

EcranQRServer <- function(id,values,local) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$send, {

      bonne_reponses <- info_enigmes(values) %>%
        filter(ID_enigme == i_question(values,local),
               Type == "Question") %>%
        select(starts_with("Reponse")) %>%
        mutate(Reponse_concat = reduce(across(everything(), ~ coalesce(., "")), str_c)) %>%
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
        output$textquestion <- renderUI("")

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
          texte_erreur <- paste0("Erreur, réinitialisation de l'étape ",label_step)
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

        output$textquestion <- renderUI(texte_erreur)
      }

    })

  }
  )
}

EcranQRUI <- function(id,values,local) {
  ns <- NS(id)

  values$db_enigmes

  # Check si bloc terminé
  fl_bloc_ok <- reactive({

    bloc_min <- actu_enigmes(values) %>%
      filter(FL_Valid == 0) %>%
      summarise(ID_bloc = min(ID_bloc)) %>% pull()

    if (local$userType == "I"){
      bloc_question <- info_enigmes(values) %>%
        filter(Type == "Indice",ID_enigme == i_question(values,local)) %>%
        pull(ID_bloc) %>% unique()
    }
    if (local$userType == "Q"){
      bloc_question <- info_enigmes(values) %>%
        filter(Type == "Question",ID_enigme == i_question(values,local)) %>%
        pull(ID_bloc) %>% unique()
    }

    print(bloc_question)

    if (length(bloc_question) == 0) return(FALSE)
    return(bloc_min == bloc_question)
  })

  db_i <- reactive({
    info_enigmes(values) %>%
      filter(Type == "Indice",ID_enigme == i_question(values,local))
  })

  db_q <- reactive({
    info_enigmes(values) %>%
      filter(Type == "Question",ID_enigme == i_question(values,local))
  })

  if (local$userType == "I"){
    if (nrow(db_i()) > 0){
      if (db_i()$Format  == "image")
        indicAnimServer(ns("indicimg"),db_i(),local$base_url)
      else
        indicTextServer(ns("indictxt"),db_i())
    }
  }

  choix <- function(){
    temp <- db_q()
    choix <- str_c(temp[,grep("Choix",colnames(temp))])
    choix[!is.na(choix)]
  }

  if (fl_bloc_ok()){
    if (local$userType == "I" & nrow(db_i()) > 0){
      tagList(
        style_global(),

        # gl_talk_shinyUI(ns("talk")),

        fluidRow(column(12,div(style = "height:50px"))),

        ### QUESTIONS (APPEL MODULES) ###

        if (db_i()$Format == "image") {
          indicAnimUI(ns("indicimg"))
        } else {
          indicTextUI(ns("indictxt"))
        }
      )
    } else if (local$userType == "Q" & nrow(db_q()) > 0){
      tagList(
        style_global(),

        # gl_talk_shinyUI(ns("talk")),

        ### CATEGORIE ###
        fluidRow(column(6, offset = 3,
                 div(class = "card center_text",h2(db_q()$LabelStep))
                 )),

        fluidRow(column(12,div(style = "height:300px;"))),

        ### LABEL ###
        fluidRow(
          column(6, offset = 3,div(class = "card center_text",h3(db_q()$Label)))
        ),

        ### REPONSES ###

        if (db_q()$Format != "rien"){

          fluidRow(column(6, offset = 3,div(class = "card center",

          if (db_q()$Format == "radio") {

            radioButtons(ns("reponse"),label = "",choices = choix(),
                         selected = NULL,inline = FALSE)

          } else if (db_q()$Format == "select") {

            selectInput(ns("reponse"),label = "",choices = choix(),
                        selected = NULL,multiple = TRUE)

          } else if (db_q()$Format == "checkbox") {

            checkboxGroupInput(ns("reponse"),label = "",
                               choices = choix(),selected = NULL)

          } else if (db_q()$Format == "texte") {

            textInput(ns("reponse"),label = NULL,placeholder = "Réponse")

          },

          actionButton(ns("send"),"Envoyer")
          )))
        },

        ######################


        fluidRow(column(12,div(style = "height:50px;"))),

        column(12,uiOutput(ns("textquestion")),class = "center"),

        fluidRow(column(12,div(style = "height:150px;")))
      )
    }
  }
}
