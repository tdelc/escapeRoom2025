### MODULES

#' Module UI label
#'
#' @param id id du module
#'
#' @returns html
#' @export
LabelUI <- function(id) {
  ns <- NS(id)
  tagList(
    # column(6, offset = 3,
      # h2(htmlOutput(ns("label")),class = "card center_text"),
      h2(htmlOutput(ns("label"))),
      imageOutput(ns("anim"),inline = TRUE)
    # )
  )
}

#' Module serveur label
#'
#' @param id id du module
#' @param db db question ou indice
#' @param path chemin des fichiers
#'
#' @returns moduleServer
#' @export
LabelServer <- function(id,db,path) {
  moduleServer(
    id,
    function(input, output, session) {

      output$label <- renderText(db$Label)

      output$anim <- renderImage({
        filename <- normalizePath(file.path('./www/images/',db$Link))
        list(src = filename)
      }, deleteFile = FALSE)
    })

}
