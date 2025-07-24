ask_IA <- function(text,id){
  out <- "Pardon, je n'ai pas compris la question ou le message. Merci de recommancer dans 2 minutes."
  try({
    answer <- chat(text,chatlog_id = id,
                   model = "gpt-4o-mini",output = "response_object")
    out <- answer$choices$message$content
  },silent = TRUE)
  out
}

appendChatMessage <- function(sender, message, ns) {
  message_js <- jsonlite::toJSON(message, auto_unbox = TRUE)
  runjs(sprintf("
          var message_html = '<div class=\"message %s\"><p>' + %s + '</p></div>';
          $('#%s').append(message_html);
          $('#%s').scrollTop($('#%s')[0].scrollHeight);
        ", sender, message_js, ns("chat_window"), ns("chat_window"), ns("chat_window")))
}

EcranIAServer <- function(id,values) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      session$onFlushed(function() {
        observe({
          chat_hist <- actu_IA(values)
          if (nrow(chat_hist) > 0){
            for (id_row in 1:nrow(chat_hist)){
              sender <- chat_hist$user[id_row]
              message <- chat_hist$message[id_row]
              appendChatMessage(sender,message,ns)
            }
          }
        })
      }, once = TRUE)

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

      # observeEvent(input$question_send, {
      observeEvent(input$confirm_send, {
        removeModal()
        req(input$question_text)

        # Ajouter le message de l'utilisateur à la fenêtre de chat
        appendChatMessage("user", input$question_text, ns)

        # Réponse de l'IA
        text_out <- ask_IA(input$question_text, "Question_perso")
        values$text_IA <- text_out

        # Ajouter le message d'Alice avec un délai pour simuler la réflexion
        delay(500, appendChatMessage("alice", text_out, ns))

        # Réinitialiser le champ de saisie
        updateTextInput(session, "question_text", value = "")

        new_rows <- tibble(CD_admin = "action", timer = Sys.time(),
                           user = "user",message = input$question_text) %>%
          add_row(tibble(CD_admin = "action", timer = Sys.time(),
                         user = "alice",message = text_out))

        sheet_append(values$id_drive, data = new_rows,sheet = "db_IA")
      })

      text_IA_admin <- reactiveVal("")

      observeEvent(values$text_IA_admin,{
        text_out <- values$text_IA_admin
        appendChatMessage("alice", text_out, ns)
        delay(500, text_IA_admin(text_out))
        values$text_IA <- text_out
      })

      observe({
        callModule(gl_talk_shiny, "IA_audio", transcript = text_IA_admin, controls = FALSE,
                   languageCode = "fr-fr", gender = "NEUTRAL", pitch = -5)
      })
    }
  )
}


EcranIAUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Activer shinyjs
    useShinyjs(),

    # Inclure le CSS personnalisé
    tags$head(
      tags$style(HTML("
        # body {
        #   background-color: #1A1A1A;
        #   color: #FFFFFF;
        #   font-family: 'Courier New', Courier, monospace;
        # }
        body {
          background-image: url('glados.JPG');
          background-repeat: no-repeat;
          height: 100px;
          background-color: #cccccc;
          # color: #33ff33;
          color: #002300;
          font-family: 'Fira Mono', 'Courier New', Courier, monospace;
        }
        .card {
          # background: #111418;
          background: #F0F8FF;
          # opacity: .8;
          border-radius: 12px;
          box-shadow: 0 2px 16px #00000040;
          padding: 24px;
          margin-bottom: 30px;
          border: 1px solid #222;
        }
        #chat_window {
          background-color: #2E2E2E;
          border-radius: 10px;
          padding: 20px;
          max-height: 400px;
          overflow-y: auto;
        }
        .message {
          margin: 10px 0;
        }
        .user {
          text-align: right;
        }
        .alice {
          text-align: left;
        }
        .message p {
          display: inline-block;
          padding: 10px;
          border-radius: 10px;
          max-width: 70%;
        }
        .user p {
          background-color: #007BFF;
        }
        .alice p {
          background-color: #6C757D;
        }
      "))
    ),

    fluidRow(
      column(12, align = "center",
             div(class = "card",
              h1("Interface de communication avec SYNAPSE")
             )
      )
    ),
    fluidRow(
      column(12,
             div(class = "card",
              div(id = ns("chat_window"))
             )
      )
    ),
    fluidRow(
      column(10,
             textInput(ns("question_text"), label = NULL, placeholder = "Entrez votre message...")
      ),
      column(2,
             actionButton(ns("question_send"), "Envoyer", class = "btn-primary")
      )
    ),
    gl_talk_shinyUI(ns("IA_audio"))
  )
}
