load_db_enigmes <- function(id_drive){
  googlesheets4::read_sheet(id_drive,sheet = "db_enigmes") %>%
    mutate(timer = as.numeric(timer))
}
load_db_scans <- function(id_drive){
  googlesheets4::read_sheet(id_drive,sheet = "db_scans") %>%
    mutate(timer = as.numeric(timer))
}
load_db_AI <- function(id_drive){
  googlesheets4::read_sheet(id_drive,sheet = "db_AI") %>%
    mutate(timer = as.numeric(timer))
}

info_enigmes <- function(values){
  values$db_enigmes %>% filter(CD_admin == "init")
}

info_scans <- function(values){
  values$db_scans %>% filter(CD_admin == "init")
}

info_AI <- function(values){
  values$db_AI %>% filter(CD_admin == "init")
}

actu_enigmes <- function(values){
  values$db_enigmes %>% group_by(ID_bloc,ID_enigme,ID_step,Ecran,Type) %>%
    filter(row_number() == n())  %>% ungroup() %>%
    arrange(ID_bloc,ID_enigme,ID_step)
}

actu_scans <- function(values){
  values$db_scans %>% group_by(ID) %>%
    filter(row_number() == n())  %>% ungroup() %>%
    arrange(ID)
}

actu_AI <- function(values){
  values$db_AI %>% filter(CD_admin == "action")
}
