#' Check si le scan est déjà fait
#'
#' @param values Valeurs réactives
#' @param scan_id ID du scan
#'
#' @returns boolean
#' @export
check_scan <- function(session,values,scan_id){
  if (is.na(scan_id) || scan_id == "") return(FALSE)

  scan_ligne <- actu_scans(values) %>% filter(ID == scan_id)

  if (nrow(scan_ligne) == 0) {
    updateProgressBar(session = session,id = "scan_progress",
                      value = 0, total = 100,
                      title = trad("Document non valide",values))
    return(FALSE)
  }else if (scan_ligne$FL_Valid == 0){
    updateProgressBar(session = session,id = "scan_progress",
                      value = 0, total = 100,
                      title = trad("Scan en cours",values))
    return(TRUE)
  }else{
    updateProgressBar(session = session,id = "scan_progress",
                      value = 0, total = 100,
                      title = trad("Ce document a déjà été scanné",values))
    return(FALSE)
  }
}

#' Extraite titre du Scan
#'
#' @param session Session shiny
#' @param values Valeurs réactives
#' @param scan_id ID du scan
#'
#' @returns string
#' @export
titre_scan <- function(values,scan_id){
  scan_ligne <- info_scans(values) %>% filter(ID == scan_id)
  return(scan_ligne$Texte)
}

#' Scan en cours
#'
#' @param session Session shiny
#' @param values Valeurs réactives
#' @param scan_id ID du scan
#'
#' @returns TRUE
#' @export
action_scan <- function(session,values,scan_id){

  scan_ligne <- info_scans(values) %>% filter(ID == scan_id)

  messages <- c(
    trad("Initialisation du scan du fichier...",values),
    trad("Analyse des métadonnées...",values),
    trad("Recherche de virus...",values),
    trad("Vérification de l'intégrité des données...",values),
    trad("Analyse des meta-données du fichier...",values),
    trad("Scan du code source...",values),
    trad("Compilation des résultats...",values),
    trad("Test unitaire sur le fichier...",values),
    trad("Scan terminé. Aucun problème détecté.",values)
  )

  timer <- seq(1, 100,by=(100/length(messages)))

  for (i in 1:100) {
    updateProgressBar(session = session,id = "scan_progress",
                      value = i, total = 100,
                      title = messages[max(1,which(timer <= i))])
    Sys.sleep(runif(1)/5)
  }

  return(TRUE)
}

#' Valider un scan
#'
#' @param session session shiny
#' @param values Valeurs réactives
#'
#' @returns NULL
#' @export
valid_scan <- function(session,values,scan_id){
  new_row <- tibble(CD_admin = "action", timer = Sys.time(),
                    ID = scan_id,FL_Valid = 1)

  sheet_append(values$id_drive, data = new_row,sheet = "db_scans")
  values$db_scans <- load_db_scans(values$id_drive)

  # updateProgressBar(session = session, id = "scan_progress",
  #                   value = 0, total = 100,
  #                   title = trad("Scan terminé. Aucun problème détecté.",values))
  # updateTextInput(session = session,inputId = "scan_num",value = "")
  # values$text_scan <- trad("Scan terminé. Aucun problème détecté.",values)
}

#' Serveur de scans
#'
#' @param id id
#' @param values Valeurs réactives
#' @param local Valeurs locales
#'
#' @returns shiny server
#' @export
EcranScanServer <- function(id,values,local) {
  moduleServer(
    id,
    function(input, output, session) {

      scan_id <- reactive(as.numeric(local$userEcran))
      check_valid <- reactive(NULL)

      observe({
        req(scan_id())
        if (local$userType == "S" & str_length(local$userEcran) > 0){
          titre <- titre_scan(values,scan_id())
          output$scan_titre <- renderText(titre)
        }
      })

      # Later pour laisser l'interface se charger avant le check
      later(function() {
        observe({
          # req(scan_id())
          if (local$userType == "S" & str_length(local$userEcran) > 0){
            if (check_scan(session,values,scan_id()) == TRUE){
              valid <- action_scan(session,values,scan_id())
              # check_valid <- reactive(valid)
              valid_scan(session,values,scan_id())
              # scan_id(NULL)
              # valid_scan <- FALSE
              local$userEcran <- ""
            }
          }
        })
      }, delay = 1)

      # Listing des scans
      output$listing_scan <- renderText({

        actu_scans <- info_scans(values) %>% select(ID,Texte) %>%
          left_join(actu_scans(values) %>% select(ID,FL_Valid)) %>%
          mutate(Texte_HTML = if_else(FL_Valid == 1,
                                      paste0("<s>",Texte,"</s>"),Texte))

        ncol <- 2
        total <- length(actu_scans$Texte_HTML)
        nrow <- ceiling(total / ncol)

        # Remplir avec "" pour égaliser la hauteur
        Texte_split <- c(actu_scans$Texte_HTML, rep(NA, ncol * nrow - total))
        mat <- matrix(Texte_split, nrow = nrow, ncol = ncol, byrow = FALSE)

        # Créer HTML
        table_rows <- apply(mat, 1, function(row){
          paste0("<tr>", paste0("<td style='padding: 10px 10px;'>",
                                ifelse(is.na(row), "", row), "</td>",
                                collapse=""), "</tr>")
        })
        html_out <- paste0("<table style='border-collapse:separate; border-spacing: 0 2px;'><tbody>", paste(table_rows, collapse=""), "</tbody></table>")

        return(HTML(html_out))
      })

      # Gestion des mails et des scans
      output$nb_mails_load <- renderText({
        pc_mails_load <- round(100*values$nb_mails_load/values$nb_citoyens_act)
        paste0(
          format(values$nb_mails_load, big.mark = " ", scientific = F), " / ",
          format(values$nb_citoyens_act, big.mark = " ", scientific = F), " (",
          pc_mails_load,"%)")
      })

      output$nb_mails_send <- renderText({
        pc_mails_send <- round(100*values$nb_mails_send/values$nb_mails_load)
        if (values$nb_mails_load == 0) pc_mails_send <- 0
        paste0(
          format(values$nb_mails_send, big.mark = " ", scientific = F), " / ",
          format(values$nb_mails_load, big.mark = " ", scientific = F), " (",
          pc_mails_send,"%)")
      })

      # Nombre de scans
      output$nb_scans <- renderText({

        actu_scans <- info_scans(values) %>%
          select(ID,Texte) %>%
          left_join(actu_scans(values) %>% select(ID,FL_Valid))

        # actu_scans <- actu_scans(values) %>%
        #   select(ID,FL_Valid) %>%
        #   left_join(info_scans(values) %>% select(ID,Texte))

        paste0(
          sum(actu_scans$FL_Valid), " / ",
          nrow(actu_scans), " (",
          round(100*sum(actu_scans$FL_Valid)/nrow(actu_scans)),"%)")
      })

      observe({
        invalidateLater(1000, session)
        isolate({

          output$timing_scan <- renderText({

            timing <- temps_restant(values$heure_fin,values$minute_fin)

            paste0(timing$heures,"H ",timing$minutes,"M ",timing$secondes,"S")
          })
        })
      })

    }
  )
}

#' UI scans
#'
#' @param id id
#' @param values Valeurs réactives
#' @param local Valeurs locales
#'
#' @returns shiny ui
#' @export
EcranScanUI <- function(id,values,local) {
  ns <- NS(id)

  if (local$userType == "S"){

    tagList(

      style_global(),
      style_scan("#004700","#33ff33"),

      tagList(
        fluidRow(
          column(12,h3(textOutput(ns("scan_titre")))),
          column(
            12,
            br(),
            progressBar(
              id = ns("scan_progress"),
              value = 0, total = 100,
              display_pct = TRUE,
              title = trad("En attente de scan",values)
            )
          )
        )
      )
    )
  } else if (local$userType == "L"){


    style_scan <- if (values$active_mails_send) {
      style_list("#470000","#ff3333")
    } else {
      style_list("#004700","#33ff33")
    }

    tagList(
      tags$head(style_global()),
      tags$head(style_scan),
      br(),
      fluidRow(column(12,div(class = "card center_text",h1(
        trad("Interface de contrôle de Synapse",values))))),
      fluidRow(
        column(3,
             div(class = "card",
                 div(class = "panel-title", trad("Temps restant",values)),
                 div(class = "panel-title",textOutput(ns("timing_scan")))
                 # div(id = "digitalClock", class = "digital-clock", "--:--:--")
             ),
             div(class = "card",
                 div(class = "panel-title", trad("Chargement des mails :",values)),
                 div(class = "panel-title",textOutput(ns("nb_mails_load"))),
                 div(class = "panel-title", trad("Envois de mails :",values)),
                 div(class = "panel-title",textOutput(ns("nb_mails_send"))),
                 div(class = "panel-title", trad("Documents à scanner :",values)),
                 div(class = "panel-title",textOutput(ns("nb_scans")))
                 )
          ),
      column(9,
          div(class = "card",
                  div(id = "docList", class = "doc-list",
                      htmlOutput(ns("listing_scan")))
              )
        )
      ),
      tags$script(HTML("
    // HORLOGE DIGITALE
    function updateClock() {
      var now = new Date();
      var h = String(now.getHours()).padStart(2,'0');
      var m = String(now.getMinutes()).padStart(2,'0');
      var s = String(now.getSeconds()).padStart(2,'0');
      document.getElementById('digitalClock').innerHTML = h+':'+m+':'+s;
    }
    setInterval(updateClock, 1000);
    updateClock();
  "))
    )
  }
}
