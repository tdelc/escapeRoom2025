### MODULES

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
