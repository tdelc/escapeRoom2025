#' Chargement de la DB enigmes
#'
#' @param id_drive id du google drive
#'
#' @returns data.frame
#' @export
load_db_enigmes <- function(id_drive){
  googlesheets4::read_sheet(id_drive,sheet = "db_enigmes") %>%
    mutate(timer = as.numeric(timer))
}

#' Chargement de la DB scans
#'
#' @param id_drive id du google drive
#'
#' @returns data.frame
#' @export
load_db_scans <- function(id_drive){
  googlesheets4::read_sheet(id_drive,sheet = "db_scans") %>%
    mutate(timer = as.numeric(timer))
}

#' Chargement de la DB AI
#'
#' @param id_drive id du google drive
#'
#' @returns data.frame
#' @export
load_db_AI <- function(id_drive){
  googlesheets4::read_sheet(id_drive,sheet = "db_AI") %>%
    mutate(timer = as.numeric(timer))
}

#' Extraire les infos initiales des enigmes
#'
#' @param values Valeurs réactives
#'
#' @returns data.frame
#' @export
info_enigmes <- function(values){
  values$db_enigmes %>% filter(CD_admin == "init")
}

#' Extraire les infos initiales des scans
#'
#' @param values Valeurs réactives
#'
#' @returns data.frame
#' @export
info_scans <- function(values){
  values$db_scans %>% filter(CD_admin == "init")
}

#' Extraire les infos initiales des IA
#'
#' @param values Valeurs réactives
#'
#' @returns data.frame
#' @export
info_AI <- function(values){
  values$db_AI %>% filter(CD_admin == "init")
}

#' Extraire les infos actuelles des énigmes
#'
#' @param values Valeurs réactives
#'
#' @returns data.frame
#' @export
actu_enigmes <- function(values){
  values$db_enigmes %>% group_by(ID_bloc,ID_enigme,ID_step,Ecran,Type) %>%
    filter(row_number() == n())  %>% ungroup() %>%
    arrange(ID_bloc,ID_enigme,ID_step)
}

#' Extraire les infos actuelles des scan
#'
#' @param values Valeurs réactives
#'
#' @returns data.frame
#' @export
actu_scans <- function(values){
  values$db_scans %>% group_by(ID) %>%
    filter(row_number() == n())  %>% ungroup() %>%
    arrange(ID)
}

#' Extraire les infos actuelles des IA
#'
#' @param values Valeurs réactives
#'
#' @returns data.frame
#' @export
actu_AI <- function(values){
  values$db_AI %>% filter(CD_admin == "action")
}

#' Extraire l'id de la question en cours
#'
#' @param values Valeurs réactives
#' @param local Valeurs locales
#'
#' @returns number
#' @export
i_question <- function(values,local=TRUE){
  actu <- actu_enigmes(values) %>% filter(FL_Valid == 0)
  if (is.list(local)){
    actu <- actu %>% filter(Ecran == local$userEcran)
  }
  actu %>% summarise(ID_enigme=min(ID_enigme)) %>% pull(ID_enigme)
}
