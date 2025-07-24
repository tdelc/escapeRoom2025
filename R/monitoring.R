
EcranControleServer <- function(id,base_url) {
  moduleServer(
    id,
    function(input, output, session) {

      create_frame <- function(type,ecran){
        renderUI({
          end_url <- NA
          if (type == "Question") end_url <- paste0("?Q",ecran)
          if (type == "Indice")  end_url <- paste0("?I",ecran)
          if (type == "Scan") end_url <- "?S"
          if (type == "Documents") end_url <- "?L"
          if (type == "IA") end_url <- "?IA"

          if (!is.na(end_url)){
            url <- paste0(base_url,end_url)
          }

          print(url)

          tags$iframe(src=url, height=300, width=500,
                      onload = "this.contentWindow.document.body.style.zoom = '0.5';")
        })
      }

      output$frameQ1 <- create_frame("Question",1)
      output$frameQ2 <- create_frame("Question",2)
      output$frameQ3 <- create_frame("Question",3)
      output$frameQ4 <- create_frame("Question",4)
      output$frameI1 <- create_frame("Indice",1)
      output$frameI2 <- create_frame("Indice",2)
      output$frameI3 <- create_frame("Indice",3)
      output$frameI4 <- create_frame("Indice",4)
      output$frameS <- create_frame("Scan",0)
      output$frameD <- create_frame("Documents",0)
      output$frameIA <- create_frame("IA",0)
    }
  )
}

EcranControleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
    body, .container {margin: 10px; padding: 0; width: 90%;}
    .col-sm-3 {width: 25%;padding: 0;}
    .col-sm-6 {width: 50%;padding: 0;}
    .col-sm-12 {width: 100%;padding: 0;}
    ")),
    h1("Controle des écrans"),
    tags$table(
      tags$tr(
        tags$td(width = "25%", htmlOutput(ns("frameQ1"))),
        tags$td(width = "25%", htmlOutput(ns("frameQ2"))),
        tags$td(width = "25%", htmlOutput(ns("frameQ3"))),
        tags$td(width = "25%", htmlOutput(ns("frameQ4")))
      ),
      tags$tr(
        tags$td(width = "25%", htmlOutput(ns("frameI1"))),
        tags$td(width = "25%", htmlOutput(ns("frameI2"))),
        tags$td(width = "25%", htmlOutput(ns("frameI3"))),
        tags$td(width = "25%", htmlOutput(ns("frameI4")))
      ),
      tags$tr(
        tags$td(width = "25%", htmlOutput(ns("frameS"))),
        tags$td(width = "25%", htmlOutput(ns("frameD"))),
        tags$td(width = "25%", htmlOutput(ns("frameIA"))),
        tags$td(colspan = "1")  # Pour aligner et compléter la rangée
      )
    )
  )
}
