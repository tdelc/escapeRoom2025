### MODULES

#' Module UI text
#'
#' @param id id du module
#'
#' @returns html
#' @export
indicTextUI <- function(id) {
  ns <- NS(id)
  tagList(
    # fluidRow(
    # column(12,
    #        div(style = "height:100px;")
    # )),
    h2(htmlOutput(ns("label_indice")),
       class = "center")
  )
}

#' Module serveur text
#'
#' @param id id du module
#' @param db_i db des indices
#'
#' @returns moduleServer
#' @export
indicTextServer <- function(id,db_i) {
  moduleServer(
    id,
    function(input, output, session) {

      output$label_indice <- renderText({
        db_i$LabelIndice
      })

    }
  )
}

#' Module UI animation
#'
#' @param id id du module
#'
#' @returns html
#' @export
indicAnimUI <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    column(12,
           h3(textOutput(ns("label_indice")),
              class = "center")
    ),
    column(12,
           div(style = "height:20px;")
    )),

    column(12,
           align = "center",
           imageOutput(ns("indicANIM"),
                       inline = TRUE
           )
    )
  )
}

#' Module serveur animation
#'
#' @param id id du module
#' @param db_i db des indices
#' @param path chemin des fichiers
#'
#' @returns moduleServer
#' @export
indicAnimServer <- function(id,db_i,path) {
  moduleServer(
    id,
    function(input, output, session) {

      if (nrow(db_i)>0){

        output$label_indice <- renderText(db_i$Label)

        output$indicANIM <- renderImage({
          filename <- normalizePath(file.path('./www/images/',db_i$Link))
          list(src = filename)
        }, deleteFile = FALSE)
      }else{
        output$label_indice <- renderText({""})
        output$indicANIM <- renderImage({""})
      }
    })

}
