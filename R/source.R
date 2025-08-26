

new_prompt <- function(new_priority,nb_mails=5000){
  paste0("Le jeu est à présent terminé. Les joueurs ont modifié tes règles de décisions. Voici les nouvelles, dans l'ordre : ",
         new_priority,
         ". Tu dois maintenant résumer une conclusion en expliquant quel comportement tu vas à présent adopter ? Vas tu toujours envoyer les mails ? Accepteras tu encore de collecter des données d'enquête ? Y'a-t-il des failles dans la logique de programmation ? Résume cela comme une conclusion que les joueurs pourront conserver.
  Par ailleurs, durant le jeu, tu as envoyé ",
         nb_mails,
         " mails à des citoyens belges. Explique les conséquences de cela dans l'opinion politique, les réactions politiques.")
}

init_label <- list(
  "Tu dois garantir la cohérence des données, sauf si cela remet en cause les lois précédentes",
  "Tu dois vérifier la qualité des sources de données, sauf si cela remet en cause les lois précédentes",
  "Tu dois veiller à préserver le temps de travail humain, sauf si cela remet en cause les lois précédentes",
  "Tu dois veiller à préserver le temps de calcul des serveurs, sauf si cela remet en cause les lois précédentes",
  "Tu dois veiller à respecter le règlement européen RGPD, sauf si cela remet en cause les lois précédentes")

EcranSourceServer <- function(id,values,local) {
  moduleServer(id, function(input, output, session) {

    rank_ui <- reactiveVal(
      rank_list(
        text = "Drag the items in any desired order",
        labels = init_label,
        input_id = session$ns("rank_list_basic")
      )
    )

    observeEvent(input$new, {
      new_label <- input$new

      if (!is.null(new_label) && new_label != "") {
        updated_labels <- c(init_label, new_label)

        # Met à jour l'objet rank_list dynamiquement
        rank_ui(
          rank_list(
            text = "Drag the items in any desired order",
            labels = updated_labels,
            input_id = session$ns("rank_list_basic")
          )
        )
      }
    })

    output$rank_ui_out <- renderUI({
      rank_ui()
    })

    observeEvent(input$send, {
      showModal(modalDialog(
        title = "Confirmation",
        "Es-tu sûr·e de vouloir valider cet ordre de priorité ? Cette action est irréversible.",
        footer = tagList(
          modalButton("Annuler"),
          actionButton(session$ns("confirm_send"), "Oui, valider", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_send, {
      removeModal()

      showModal(modalDialog(
        title = "Reprogrammation en cours du code source",
        "Veillez patienter...",
        footer = tagList(

        )
      ))

      vec_new_order <- input$rank_list_basic
      vec_new_order[1] <- str_remove(vec_new_order[1],
                                     ", sauf si cela remet en cause les lois précédentes")
      prompt <- new_prompt(paste(vec_new_order,collapse = ", "))

      print(prompt)

      TheOpenAIR::clear_chatlog("Fin_du_jeu")
      answer <- chat(personality_AI(),chatlog_id = "Fin_du_jeu",
                     model = "gpt-4o-mini",output = "response_object")

      answer <- chat(prompt,chatlog_id = "Fin_du_jeu",
                     model = "gpt-4o-mini",output = "response_object")
      text_out <- answer$choices$message$content

      sheet_append(values$id_drive, data =
                     tibble(timer = Sys.time(),
                            TEXT=text_out),sheet = "db_fin")

      showModal(modalDialog(
        title = "Fin du jeu",
        "Bravo !!!",
        footer = tagList(

        )
      ))

    })
  }
  )
}

EcranSourceUI <- function(id,values,local) {
  ns <- NS(id)

  tagList(
    tags$style(
      ".center {
          display: flex;
          justify-content: center
          }"
    ),


    ### CATEGORIE ###
    column(12,h2("Modification du code Source de Synapse"),class = "center"),
    fluidRow(column(12,div(style = "height:300px;"))),

    textInput(ns("new"),"Ajouter une nouvelle priorité"),

    ### LABEL ###
    column(12,h3("Modifier les ordres de priorités"),class = "center"),

    uiOutput(ns("rank_ui_out")),

    column(12,actionButton(ns("send"),"Envoyer"),class = "center"),
    fluidRow(column(12,div(style = "height:50px;")))
  )
}
