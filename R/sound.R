# UI : grille de boutons auto-générés depuis le dossier
EcranSoundUI <- function(id, title = "Banque de sons") {
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf("
      .%s-grid{
        display: grid; gap: 10px;
        grid-template-columns: repeat(auto-fill, minmax(140px, 1fr));
        align-items: stretch;
      }
      .%s-btn{
        border: 1px solid #e5e7eb; border-radius: 12px;
        padding: 12px; font-weight: 700; text-align: center;
        background: #ffffff; cursor: pointer;
        box-shadow: 0 4px 14px #0000000f;
        transition: transform .06s, box-shadow .2s, background .15s;
      }
      .%s-btn:hover{
        background:#f8fafc; transform: translateY(-1px);
        box-shadow: 0 8px 18px #00000014;
      }
      .%s-search{ margin-bottom: 10px; }
    ", ns("sb"), ns("sb"), ns("sb"), ns("sb")))),
    div(class = "card",
        h2(title),
        # textInput(ns("q"), NULL, placeholder = "Filtrer les sons…", width = "100%",
        #           class = sprintf("%s-search", ns("sb"))),
        textInput(ns("q"), NULL, placeholder = "Filtrer les sons…", width = "100%"),
        uiOutput(ns("btn_grid"))
    )
  )
}

# Server : liste les mp3 et pousse values$play_sound = list(name, url, ts)
EcranSoundServer <- function(id, values,
                             dir = "www/sfx",
                             pattern = "\\.(mp3|wav|ogg)$") {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Manifest des sons (nom & url)
    manifest <- reactive({
      if (!dir.exists(dir)) return(tibble::tibble(name = character(), url = character()))
      files <- list.files(dir, pattern = pattern, full.names = FALSE)
      print(files)
      tibble::tibble(
        name = tools::file_path_sans_ext(files),
        url  = file.path("sfx", files)   # servi depuis www/sfx/…
      ) |> dplyr::arrange(name)
    })

    # 2) Filtre par recherche
    filtered <- reactive({
      q <- trimws(tolower(input$q %||% ""))
      mf <- manifest()
      if (q == "") mf else dplyr::filter(mf, grepl(q, tolower(name), fixed = TRUE))
    })

    # 3) Grille de boutons
    output$btn_grid <- renderUI({
      mf <- filtered()
      if (nrow(mf) == 0) return(div(em("Aucun son…")))
      div(class = sprintf("%s-grid", ns("sb")),
          lapply(seq_len(nrow(mf)), function(i){
            nm <- mf$name[i]
            actionButton(ns(paste0("snd_", nm)), label = nm, class = sprintf("%s-btn", ns("sb")))
          })
      )
    })

    # 4) Observe tous les boutons → pousse un signal réactif global
    observe({
      mf <- manifest()
      lapply(mf$name, function(nm){
        btn_id <- paste0("snd_", nm)
        observeEvent(input[[btn_id]], {
          # Signal “jouer tel son” envoyé aux autres modules
          values$play_sound <- list(
            name = nm,
            url  = file.path("sfx", paste0(nm, ".mp3")),  # par défaut mp3
            ts   = Sys.time()                              # horodatage pour forcer le changement
          )
        }, ignoreInit = TRUE)
      })
    })
  })
}
