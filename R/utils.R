temps_restant <- function(heure, minute) {
  # heure actuelle
  maintenant <- Sys.time()

  # date du jour
  aujourd_hui <- as.Date(maintenant)

  # construire l'heure cible
  cible <- as.POSIXct(paste(aujourd_hui, sprintf("%02d:%02d:00", heure, minute)), tz = Sys.timezone())

  # si l'heure est déjà passée, prendre demain
  if (cible <= maintenant) {
    cible <- cible + 24*60*60
  }

  # calcul du temps restant en secondes
  diff_sec <- as.numeric(difftime(cible, maintenant, units = "secs"))

  # le convertir en heures, minutes, secondes
  heures   <- diff_sec %/% 3600
  minutes  <- (diff_sec %% 3600) %/% 60
  secondes <- round(diff_sec %% 60)

  return(list(
    total_secondes = diff_sec,
    heures = heures,
    minutes = minutes,
    secondes = secondes
  ))
}
