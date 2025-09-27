

#' Changement du prompt de l'IA
#'
#' @param new_priority Nouvelles priorité
#' @param nb_mails Nombre de mails envoyés
#'
#' @returns text
#' @export
new_prompt <- function(new_priority,nb_mails=5000,language = "fr"){
  if (language == "fr")
    paste0("Le jeu est à présent terminé. Les joueurs ont modifié tes règles de décisions. Voici les nouvelles, dans l'ordre : ",
           new_priority,
           ". Tu dois maintenant résumer une conclusion en expliquant quel comportement tu vas à présent adopter ? Vas tu toujours envoyer les mails ? Accepteras tu encore de collecter des données d'enquête ? Y'a-t-il des failles dans la logique de programmation ? Résume cela comme une conclusion que les joueurs pourront conserver.
    Par ailleurs, durant le jeu, tu as envoyé ",
           nb_mails,
           " mails à des citoyens belges. Pars du principe que s'il y a moins de 1000 mails envoyés, il n'y aura pas de crise. Et monte en pression progressivement, le but étant d'être positif jusqu'à 50000 mails. Explique les conséquences d'avoir envoyé ",nb_mails," mails dans l'opinion publique, les réactions politiques.")
  else
    paste0("Het spel is nu afgelopen. De spelers hebben je beslissingsregels gewijzigd. Dit zijn de nieuwe regels, in volgorde: ",
           new_priority,
           ". Je moet nu een conclusie samenvatten door uit te leggen welk gedrag je nu gaat aannemen. Ga je nog steeds e-mails versturen? Ga je nog steeds akkoord met het verzamelen van enquêtegegevens? Zijn er hiaten in de programmeerlogica? Vat dit samen in een conclusie die de spelers kunnen onthouden.
Bovendien heb je tijdens het spel",
           nb_mails,
           " e-mails naar Belgische burgers gestuurd. Ga ervan uit dat als er minder dan 1000 e-mails worden verstuurd, er geen crisis zal ontstaan. Voer de druk geleidelijk op, met als doel positief te blijven tot 50.000 e-mails. Leg uit wat de gevolgen zijn van het versturen van ",nb_mails," e-mails voor de publieke opinie en de politieke reacties.")
}

init_label_fr <- list(
  "Tu dois garantir la cohérence des données, sauf si cela remet en cause les lois précédentes",
  "Tu dois vérifier la qualité des sources de données, sauf si cela remet en cause les lois précédentes",
  "Tu dois veiller à préserver le temps de travail humain, sauf si cela remet en cause les lois précédentes",
  "Tu dois veiller à préserver le temps de calcul des serveurs, sauf si cela remet en cause les lois précédentes",
  "Tu dois veiller à respecter le règlement européen RGPD, sauf si cela remet en cause les lois précédentes")

init_label_nl <- list(
  "Je moet de consistentie van de gegevens garanderen, tenzij dit in strijd is met eerdere wetten",
  "Je moet de kwaliteit van de gegevensbronnen controleren, tenzij dit in strijd is met eerdere wetten",
  "Je moet ervoor zorgen dat de werktijd van mensen behouden blijft, tenzij dit in strijd is met eerdere wetten",
  "Je moet ervoor zorgen dat de rekentijd van de servers behouden blijft, tenzij dit in strijd is met eerdere wetten",
  "Je moet ervoor zorgen dat de Europese GDPR-verordening wordt nageleefd, tenzij dit in strijd is met eerdere wetten")

#' Serveur du code source
#'
#' @param id id
#' @param values Valeurs réactives
#' @param local Valeurs locales
#'
#' @returns shiny server
#' @export
EcranSourceServer <- function(id,values,local) {
  moduleServer(id, function(input, output, session) {

    init_label <- reactive({
      if (values$language == "fr") init_label <- init_label_fr
      else init_label <- init_label_nl
    })

    rank_ui <- reactiveVal(

      rank_list(
        text = trad("Déplacer les éléments pour choisir leur ordre de priorité",
                    values),
        labels = init_label(),
        input_id = session$ns("rank_list_basic")
      )
    )

    observeEvent(input$new, {
      new_label <- input$new

      if (!is.null(new_label) && new_label != "") {
        updated_labels <- c(init_label(), new_label)

        # Met à jour l'objet rank_list dynamiquement
        rank_ui(
          rank_list(
            text = trad("Déplacer les éléments pour choisir leur ordre de priorité",
                        values),
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
        trad("Es-tu sûr·e de vouloir valider cet ordre de priorité ? Cette action est irréversible.",values),
        footer = tagList(
          modalButton(trad("Annuler",values)),
          actionButton(session$ns("confirm_send"), trad("Oui, valider",values),
                       class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_send, {
      removeModal()

      showModal(modalDialog(
        title = trad("Reprogrammation en cours du code source",values),
        trad("Veillez patienter...",values),
        footer = tagList(

        )
      ))

      vec_new_order <- input$rank_list_basic
      vec_new_order[1] <- str_remove(vec_new_order[1],
                                     trad(", sauf si cela remet en cause les lois précédentes",values))
      prompt <- new_prompt(paste(vec_new_order,collapse = ", "),
                           values$nb_mails_send,
                           values$language)

      TheOpenAIR::clear_chatlog("Fin_du_jeu")
      answer <- try({chat(personality_AI(values$language),chatlog_id = "Fin_du_jeu",
                     model = "gpt-4o-mini",output = "response_object")
      },silent=T)

      while(inherits(answer, "try-error")){
        Sys.sleep(5)
        TheOpenAIR::clear_chatlog("Fin_du_jeu")
        answer <- try({chat(personality_AI(values$language),chatlog_id = "Fin_du_jeu",
                            model = "gpt-4o-mini",output = "response_object")
        },silent=T)
      }

      print(answer$choices$message$content)
      print(prompt)

      answer <- try({chat(prompt,chatlog_id = "Fin_du_jeu",
                          model = "gpt-4o-mini",output = "response_object")
      },silent=T)

      while(inherits(answer, "try-error")){
        Sys.sleep(5)
        answer <- try({chat(prompt,chatlog_id = "Fin_du_jeu",
                            model = "gpt-4o-mini",output = "response_object")
        },silent=T)
      }

      text_out <- answer$choices$message$content

      sheet_append(values$id_drive, data =
                     tibble(timer = Sys.time(),
                            TEXT=text_out),sheet = "db_fin")

      values$active_mails_load <- FALSE
      values$active_mails_send <- FALSE

      if (values$language == "fr")
        text_fin <- paste("Bravo !! SYNAPSE a bel et bien été reprogrammé !
               Elle a eu le temps d'envoyer",values$nb_mails_send,"mails")
      else
        text_fin <- paste("Bravo!! SYNAPSE is inderdaad opnieuw geprogrammeerd!
Ze heeft tijd gehad om",values$nb_mails_send,"e-mails te versturen.")

      showModal(modalDialog(
        title = trad("Fin du jeu",values),
        text_fin,
        footer = tagList(

        )
      ))

    })
  }
  )
}

#' UI code source
#'
#' @param id id
#' @param values Valeurs réactives
#' @param local Valeurs locales
#'
#' @returns shiny ui
#' @export
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
    column(12,h2(trad("Modification du code Source de Synapse",values)),class = "center"),
    fluidRow(column(12,div(style = "height:300px;"))),

    textInput(ns("new"),trad("Ajouter une nouvelle priorité",values)),

    ### LABEL ###
    column(12,h3(trad("Modifier les ordres de priorités",values)),class = "center"),

    uiOutput(ns("rank_ui_out")),

    column(12,actionButton(ns("send"),trad("Envoyer",values)),class = "center"),
    fluidRow(column(12,div(style = "height:50px;")))
  )
}
