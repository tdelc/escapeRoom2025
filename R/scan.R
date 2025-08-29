#' Check si le scan est déjà fait
#'
#' @param values Valeurs réactives
#' @param scan_id ID du scan
#'
#' @returns boolean
#' @export
check_scan <- function(values,scan_id){
  if (is.na(scan_id) || scan_id == "") return(FALSE)

  scan_ligne <- actu_scans(values) %>% filter(ID == scan_id)

  if (nrow(scan_ligne) == 0) {
    values$text_scan <- "Document non valide"
    return(FALSE)
  }else if (scan_ligne$FL_Valid == 0){
    values$text_scan <- "Scan en cours"
    return(TRUE)
  }else{
    values$text_scan <- "Ce document a déjà été scanné"
    return(FALSE)
  }
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
  texte <- scan_ligne$Texte

  messages <- c(
    paste("Initialisation du scan du fichier:", texte),
    "Analyse des métadonnées...",
    "Recherche de virus...",
    "Vérification de l'intégrité des données...",
    paste("Analyse des meta-données du fichier:", texte),
    "Scan du code source...",
    "Compilation des résultats...",
    "Test unitaire sur le fichier...",
    "Scan terminé. Aucun problème détecté."
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
valid_scan <- function(session,values){
  new_row <- tibble(CD_admin = "action", timer = Sys.time(),
                    ID = values$scan_id,FL_Valid = 1)

  sheet_append(values$id_drive, data = new_row,sheet = "db_scans")
  values$db_scans <- load_db_scans(values$id_drive)

  updateProgressBar(session = session, id = "scan_progress",
                    value = 0, total = 100,title = paste0("En attente de scan"))
  updateTextInput(session = session,inputId = "scan_num",value = "")
  values$text_scan <- "En attente d'un nouveau scan"
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

      output$textquestion_scan <- renderUI(values$text_scan)

      # Later pour laisser l'interface se charger avant le check
      later(function() {
        observe({
          if (local$userType == "S" & str_length(local$userEcran) > 0){
            values$scan_id <- as.numeric(local$userEcran)
            print(values$scan_id)
            if (check_scan(values,values$scan_id) == TRUE){
              values$valid_scan <- action_scan(session,values,values$scan_id)
            }
          }
        })
      }, delay = 1)

      observeEvent(values$valid_scan,{
        if (values$valid_scan == TRUE){
          valid_scan(session,values)
          values$scan_id <- NA
          values$valid_scan <- FALSE
          local$userEcran <- ""
        }
      })

      observeEvent(input$scan_send, {
        values$scan_id <- as.numeric(input$scan_num)
        if (check_scan(values,values$scan_id) == TRUE){
          values$valid_scan <- action_scan(session,values,values$scan_id)
        }
      })

      # Listing des scans
      output$listing_scan <- renderText({

        actu_scans <- actu_scans(values) %>%
          select(ID,FL_Valid) %>%
          left_join(info_scans(values) %>% select(ID,Texte))

        actu_scans <- actu_scans %>%
          mutate(Texte_HTML = if_else(FL_Valid == 1,
                                      paste0("<s>",Texte,"</s>"),Texte))

        ncol <- 4
        total <- length(actu_scans$Texte_HTML)
        nrow <- ceiling(total / ncol)

        # Remplir avec "" pour égaliser la hauteur
        Texte_split <- c(actu_scans$Texte_HTML, rep(NA, ncol * nrow - total))
        mat <- matrix(Texte_split, nrow = nrow, ncol = ncol, byrow = FALSE)

        # Créer HTML
        table_rows <- apply(mat, 1, function(row){
          paste0("<tr>", paste0("<td style='padding: 4px 20px;'>", ifelse(is.na(row), "", row), "</td>", collapse=""), "</tr>")
        })
        html_out <- paste0("<table style='border-collapse:separate; border-spacing: 0 2px;'><tbody>", paste(table_rows, collapse=""), "</tbody></table>")

        return(HTML(html_out))
      })

      # Gestion des mails et des scans
      output$nb_mails_load <- renderText({
        pc_mails_load <- round(100*values$nb_mails_load/values$nb_mails_tot)
        paste0(
          format(values$nb_mails_load, big.mark = " ", scientific = F), " / ",
          format(values$nb_mails_tot, big.mark = " ", scientific = F), " (",
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

        actu_scans <- actu_scans(values) %>%
          select(ID,FL_Valid) %>%
          left_join(info_scans(values) %>% select(ID,Texte))

        paste0(
          sum(actu_scans$FL_Valid), " / ",
          nrow(actu_scans), " (",
          round(100*sum(actu_scans$FL_Valid)/nrow(actu_scans)),"%)")
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

        # fluidRow(column(12,div(style = "height:100px;"))),
        #
        # fluidRow(
        #   column(12,textInput(
        #     inputId = ns("scan_num"),label = NULL,
        #     placeholder = "Numéro du scan"),class = "center"),
        #   column(12,uiOutput(ns("textquestion_scan")),class = "center"),
        #   column(12,actionButton(ns("scan_send"),"Scannez"),class = "center"),
        #
        #   br(),br(),br(),
        #   column(12,
        #          progressBar(
        #            id = ns("scan_progress"),
        #            value = 0,total = 100,display_pct = TRUE,
        #            title = paste0("En attente de scan")
        #          ))
        # )

        fluidRow(
          column(
            12,
            textInput(ns("scan_num"), NULL,
                      placeholder = "Numéro du scan",
                      width = "100%"),
            uiOutput(ns("textquestion_scan"), class = "text-center h5 mt-2"),
            actionButton(ns("scan_send"), "Scannez",
                         class = "btn btn-success btn-lg w-100 mt-2"),
            br(),
            progressBar(
              id = ns("scan_progress"),
              value = 0, total = 100,
              display_pct = TRUE,
              title = "En attente de scan"
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
      fluidRow(
      column(4,
             fluidRow(
             column(6,div(class = "card",h1("Interface de contrôle de Synapse"))),
             column(6,div(class = "card",
                          div(class = "panel-title", "Horloge système"),
                          div(id = "digitalClock", class = "digital-clock", "--:--:--")
             ))),
          div(class = "card",
              div(class = "panel-title", "Avancement du chargement des mails :"),
              div(class = "panel-title",textOutput(ns("nb_mails_load"))),
              div(class = "panel-title", "Avancement des envois de mails :"),
              div(class = "panel-title",textOutput(ns("nb_mails_send"))),
              div(class = "panel-title", "Documents manquants à scanner :"),
              div(class = "panel-title",textOutput(ns("nb_scans"))),
          )
          ),
      column(8,
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
