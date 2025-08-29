# Dictionnaire Morse
morse_dict <- list(
  A = ".-",    B = "-...",  C = "-.-.",  D = "-..",   E = ".",     F = "..-.",
  G = "--.",   H = "....",  I = "..",    J = ".---",  K = "-.-",   L = ".-..",
  M = "--",    N = "-.",    O = "---",   P = ".--.",  Q = "--.-",  R = ".-.",
  S = "...",   T = "-",     U = "..-",   V = "...-",  W = ".--",   X = "-..-",
  Y = "-.--",  Z = "--..",
  `1` = ".----", `2` = "..---", `3` = "...--", `4` = "....-", `5` = ".....",
  `6` = "-....", `7` = "--...", `8` = "---..", `9` = "----.", `0` = "-----"
)

# Convertir texte en morse (avec espaces entre lettres et mots)
to_morse <- function(text) {
  text <- toupper(gsub("[^A-Z0-9 ]", "", text))
  chars <- strsplit(text, "")[[1]]
  morse <- purrr::map_chr(chars, ~ if (.x == " ") "/" else morse_dict[[.x]])
  paste(morse, collapse = " ")
}

# Génère une image blanche ou noire
get_img <- function(color = "black", size = 1000) {
  magick::image_blank(width = size, height = size, color = color)
}

# Génère la séquence d’images en fonction du code Morse
morse_to_gif <- function(word, filename = "morse.gif") {
  morse <- to_morse(toupper(word))
  cat("Morse:", morse, "\n")

  frames <- list()

  # Durées (en 1/100 secondes = centièmes de seconde)
  unit <- 20  # 2000ms
  dot_delay <- unit
  dash_delay <- unit * 3
  pause_elem <- unit
  pause_letter <- unit * 3
  pause_word <- unit * 6

  for (c in strsplit(morse, "")[[1]]) {
    if (c == ".") {
      frames <- c(frames, list(get_img("white")), list(get_img("black")))
      attr(frames[[length(frames)-1]], "delay") <- dot_delay
      attr(frames[[length(frames)]], "delay") <- pause_elem
    } else if (c == "-") {
      frames <- c(frames, list(get_img("white")), list(get_img("black")))
      attr(frames[[length(frames)-1]], "delay") <- dash_delay
      attr(frames[[length(frames)]], "delay") <- pause_elem
    } else if (c == " ") {
      frames <- c(frames, list(get_img("black")))
      attr(frames[[length(frames)]], "delay") <- pause_letter
    } else if (c == "/") {
      frames <- c(frames, list(get_img("black")))
      attr(frames[[length(frames)]], "delay") <- pause_word
    }
  }

  gif <- magick::image_join(frames)
  magick::image_animate(gif, fps = 1) %>%
    magick::image_write(filename)
  message("GIF sauvegardé sous : ", filename)
}
