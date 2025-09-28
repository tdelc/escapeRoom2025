#### Scan ####

#' Style HTML pour les scans
#'
#' @param color_dark Couleur foncée
#' @param color_light Couleur claire
#'
#' @returns html
#' @export
style_scan <- function(color_dark,color_light){

  # .btn-scan:hover {
  #   background-color: #28cc28;
  # }

  tags$style(HTML(paste0("
    body {
      background-color: #0d0d0d;
      color: ",color_light,";
      font-family: 'Courier New', Courier, monospace;
    }
    .btn-scan {
      background-color: ",color_light,";
      border: none;
      color: #0d0d0d;
      font-size: 18px;
      padding: 10px 20px;
    }
    .btn-scan:hover {
      background-color: ",color_dark,";
    }
    .scan-output {
      border: 1px solid ",color_light,";
      padding: 10px;
      margin-top: 20px;
      height: 200px;
      overflow-y: scroll;
      background-color: #000000;
    }
  ")))
}


#' Style HTML global
#'
#' @returns html
#' @export
style_global <- function(){
  tags$style(HTML(paste0("
    .card {
      background: #F0F8FF;
      opacity: .8;
      border-radius: 12px;
      box-shadow: 0 2px 16px #00000040;
      padding: 24px;
      margin-bottom: 30px;
      border: 1px solid #222;
    }
    .center {
      display: flex;
      justify-content: center;
    }
    .center_text {
      text-align: center;
    }
    .glyphicon-ok {color:#2b8ee5}
    .glyphicon-remove {color:#e5413b}
    .glyphicon-exclamation-sign {color:#e5413b}
    .glyphicon-flag, .glyphicon-trash {color:#28b728}
  ")))
}

#' Style HTML par couleur
#'
#' @param color_dark Couleur foncée
#' @param color_light Couleur claire
#'
#' @returns html
#' @export
style_list <- function(color_dark,color_light){

  tags$style(HTML(paste0("
    body {
        background-image: url('glados.JPG');
        background-repeat: no-repeat;
        height: 100px;
        background-color: #cccccc;
        color: ",color_dark,";
        font-family: 'Fira Mono', 'Courier New', Courier, monospace;
      }
      .digital-clock {
        font-size: 2.8em;
        letter-spacing: 0.12em;
        font-family: 'Orbitron', 'Fira Mono', 'Courier New', Courier, monospace;
        text-align: center;
        color: ",color_dark,";
        text-shadow: 0 0 16px #ff333380;
        margin-bottom: 12px;
      }
      .progress {
        background: #222;
        border-radius: 6px;
        height: 24px;
        box-shadow: 0 1px 8px #111;
        overflow: hidden;
        margin-bottom: 8px;
        color: ",color_light,";
      }
      .progress-bar {
        background: linear-gradient(90deg, ",color_dark," 60%, ",color_light,");
        height: 100%;
        width: 0;
        transition: width 0.7s;
        border-radius: 6px 0 0 6px;
        box-shadow: 0 0 6px ",color_light,"80;
        color: ",color_light,";
      }
      .doc-list {
        font-size: 1.15em;
        font-size: 1em;
        padding-left: 8px;
        margin-top: 6px;
      }
      .doc-item {
        border-bottom: 1px dashed #2f6c2f;
        padding: 8px 0 5px 0;
        display: flex;
        align-items: center;
      }
      .doc-icon {
        font-size: 1.2em;
        margin-right: 12px;
        color: ",color_light,"bb;
      }
      .panel-title {
        color: ",color_dark,";
        letter-spacing: 0.08em;
        font-size: 1.25em;
        font-weight: bold;
        margin-bottom: 5px;
      }
  ")))
}

#' Style général Escape (dark propre)
#' @param primary couleur principale (bouton/accents)
#' @param accent  seconde couleur (hover/glow)
#' @param bg      fond
#' @param text    texte
style_escape_theme <- function(primary = "#2b8ee5",
                               accent  = "#00d084",
                               bg      = "#0d0d0d",
                               text    = "#e6f1ff") {
  htmltools::tags$style(htmltools::HTML(glue::glue("
  :root {{
    --primary: {primary};
    --accent:  {accent};
    --bg:      {bg};
    --text:    {text};
    --muted:   #b9c5d1;
    --error:   #e5413b;
    --ok:      #28b728;
    --card:    #F0F8FF;
  }}

  body {{
    background-color: var(--bg);
    color: var(--text);
    font-family: 'Fira Mono','Courier New',monospace;
    -webkit-font-smoothing: antialiased;
    line-height: 1.35;
  }}

  /* Conteneur max pour éviter les lignes trop longues */
  .container-narrow {{
    max-width: 900px;
    margin: 0 auto;
    padding: 16px;
  }}

  /* Cartes */
  .card {{
    background: var(--card);
    color: #0e141b;
    opacity: .92;
    border-radius: 14px;
    box-shadow: 0 8px 24px #00000050;
    padding: 28px;
    border: 1px solid #1e293b20;
    margin-bottom: 28px;
  }}

  /* Titre de catégorie (gros, all-caps, séparation nette) */
  .category-title {{
    text-transform: uppercase;
    letter-spacing: .08em;
    font-weight: 800;
    font-size: clamp(22px, 3vw, 30px);
    color: #0e141b;
    margin: 0 0 18px 0;
    position: relative;
  }}
  .category-title::after {{
    content: '';
    display: block;
    width: 72px;
    height: 4px;
    margin-top: 10px;
    border-radius: 2px;
    background: linear-gradient(90deg, var(--primary), var(--accent));
    box-shadow: 0 0 10px var(--primary);
  }}

  /* Label de question (plus petit, discret) */
  .question-label {{
    color: #0e141b;
    opacity: .8;
    font-size: 16px;
    margin-top: 18px;
    letter-spacing: .02em;
  }}

  /* Groupe réponse (input + bouton) */
  .answer-group {{
    display: grid;
    grid-template-columns: 1fr auto;
    gap: 10px;
    align-items: center;
    margin: 8px 0 6px 0;
  }}

  /* Input bien visible */
  .answer-input input.form-control {{
    background: #0b1220;
    color: var(--text);
    border: 1px solid #243449;
    border-radius: 10px;
    padding: 12px 14px;
    font-size: 18px;
    letter-spacing: .05em;
    outline: none;
    box-shadow: inset 0 0 0 1px #00000030;
  }}
  .answer-input input.form-control:focus {{
    border-color: var(--primary);
    box-shadow: 0 0 0 3px #2b8ee533;
  }}

  /* Bouton */
  .btn-answer {{
    background: var(--primary);
    color: #fff;
    border: none;
    border-radius: 10px;
    padding: 12px 18px;
    font-weight: 700;
    letter-spacing: .04em;
    cursor: pointer;
    transition: transform .06s ease, box-shadow .2s ease, background .2s ease;
    box-shadow: 0 6px 18px #2b8ee555;
  }}
  .btn-answer:hover {{
    background: color-mix(in srgb, var(--primary) 85%, black);
    transform: translateY(-1px);
    box-shadow: 0 10px 22px #2b8ee560;
  }}
  .btn-answer:active {{
    transform: translateY(0);
    box-shadow: 0 4px 12px #2b8ee540;
  }}

  /* Messages (info/erreur/succès) */
  .alert {{
    margin-top: 10px;
    padding: 10px 12px;
    border-radius: 10px;
    font-size: 14px;
  }}
  .alert-info {{
    background: #0b1220;
    color: var(--text);
    border: 1px solid #2b8ee540;
  }}
  .alert-error {{
    background: #2a0f10;
    color: #ffd8d6;
    border: 1px solid var(--error);
    box-shadow: inset 0 0 0 1px #00000030;
    animation: shake .28s ease-in-out 1;
  }}
  .alert-ok {{
    background: #0f2912;
    color: #dcffe4;
    border: 1px solid var(--ok);
  }}

  @keyframes shake {{
    0% {{ transform: translateX(0); }}
    25% {{ transform: translateX(-4px); }}
    50% {{ transform: translateX(4px); }}
    75% {{ transform: translateX(-2px); }}
    100% {{ transform: translateX(0); }}
  }}

  /* Icônes glyphicons si tu les utilises encore */
  .glyphicon-ok {{ color: var(--ok) }}
  .glyphicon-remove, .glyphicon-exclamation-sign {{ color: var(--error) }}

  /* Utilitaires */
  .center {{ display:flex; justify-content:center; }}
  .center_text {{ text-align:center; }}
  ")))
}

#' Thème sobre pour l'interface d'administration
#' Remplace style_global()
#'
#' @param primary Couleur principale (boutons/accents)
#' @param accent  Couleur secondaire (hover/focus doux)
#' @param bg      Couleur de fond
#' @param card    Fond des cartes
#' @param text    Couleur du texte principal
style_admin_theme <- function(primary = "#2563eb",   # bleu indigo
                              accent  = "#16a34a",   # vert pour hover
                              bg      = "#f5f7fb",
                              card    = "#ffffff",
                              text    = "#0f172a") {

  css <- paste0("
  :root{
    --primary:", primary, ";
    --accent:",  accent,  ";
    --bg:",      bg,      ";
    --card:",    card,    ";
    --text:",    text,    ";
    --muted:#64748b;
    --line:#e5e7eb;
    --shadow:0 6px 20px #0f172a1a;
    --shadow-sm:0 2px 10px #0f172a14;
  }

  /* Base */
  body{
    background: var(--bg);
    color: var(--text);
    font-family: system-ui, -apple-system, Segoe UI, Roboto, 'Helvetica Neue', Arial, 'Noto Sans', sans-serif;
    line-height: 1.4;
  }

  /* Cartes (reprise de .card existant) */
  .card{
    background: var(--card);
    border: 1px solid var(--line);
    border-radius: 14px;
    box-shadow: var(--shadow-sm);
    padding: 24px;
    margin-bottom: 24px;
  }

  /* Titres */
  h1{
    font-weight: 800;
    letter-spacing: .02em;
    margin: 0;
    font-size: clamp(22px, 3vw, 32px);
    color: var(--text);
  }
  h2{
    font-weight: 700;
    margin: 0 0 12px 0;
    font-size: clamp(18px, 2.2vw, 24px);
    color: var(--text);
  }

  /* Bandeau titre principal centré */
  .center_text{ text-align:center; }
  .card.center_text{
    background: linear-gradient(0deg, var(--card), var(--card));
    box-shadow: var(--shadow);
    border: 1px solid var(--line);
  }

  /* Sous-titres légers (tu utilises .panel-title) */
  .panel-title{
    color: var(--muted);
    font-size: 14px;
    margin: 4px 0 2px 0;
  }

  /* Boutons (actionButton) */
  .btn{
    border: none;
    border-radius: 10px !important;
    padding: 10px 14px;
    font-weight: 600;
    letter-spacing: .02em;
    box-shadow: 0 4px 14px #00000012;
    transition: transform .06s ease, box-shadow .2s ease, background .15s ease;
    background: var(--primary);
    color: #fff;
    margin: 4px 6px 4px 0;
  }
  .btn:hover{
    background: color-mix(in srgb, var(--primary) 88%, black);
    transform: translateY(-1px);
    box-shadow: 0 8px 18px #0000001a;
  }
  .btn:active{
    transform: translateY(0);
    box-shadow: 0 3px 10px #00000012;
  }
  .btn:disabled, .btn[disabled]{
    opacity: .5;
    cursor: not-allowed;
  }

  /* Inputs (textInput) */
  .form-control{
    background: #fff;
    color: var(--text);
    border: 1px solid var(--line);
    border-radius: 10px;
    padding: 10px 12px;
    font-size: 14px;
    transition: border-color .15s ease, box-shadow .15s ease;
  }
  .form-control:focus{
    border-color: var(--primary);
    box-shadow: 0 0 0 3px color-mix(in srgb, var(--primary) 25%, transparent);
    outline: none;
  }

  /* Radio (langue) en ligne + plus compacts */
  .radio input[type='radio']{ margin-top: 2px; }
  .radio label{ margin-right: 14px; }
  .shiny-input-radiogroup{ margin-bottom: 8px; }

  /* Datatable : compact + header sticky + zebra + hover doux */
  .dataTables_wrapper .dataTables_filter input{
    border-radius: 8px;
    padding: 6px 10px;
  }
  table.dataTable thead th{
    position: sticky; top: 0; z-index: 2;
    background: #f8fafc !important;
    border-bottom: 1px solid var(--line) !important;
  }
  table.dataTable tbody tr:nth-child(odd) { background: #fafafa; }
  table.dataTable tbody tr:hover { background: #eef2ff; }
  table.dataTable td, table.dataTable th{
    padding: 8px 10px !important;
    vertical-align: middle;
  }

  /* Petites grilles pour les champs + bouton à droite (déjà ok avec fluidRow/column) */
  .input-group, .answer-group{ gap: 8px; }

  /* États (si tu veux afficher des messages plus tard) */
  .alert{ padding: 10px 12px; border-radius: 10px; font-size: 14px; margin-top: 8px; }
  .alert-info{  background:#f1f5f9; color:#0f172a; border:1px solid var(--line); }
  .alert-ok{    background:#ecfdf5; color:#064e3b; border:1px solid #10b98155; }
  .alert-error{ background:#fef2f2; color:#7f1d1d; border:1px solid #ef444455; }

  /* Petites icônes glyphicons si encore utilisées */
  .glyphicon-ok{ color:#16a34a; }
  .glyphicon-remove, .glyphicon-exclamation-sign{ color:#ef4444; }
  ")

  htmltools::tags$style(htmltools::HTML(css))
}


