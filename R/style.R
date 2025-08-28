#### Scan ####

# .btn-scan:hover {
#   background-color: #28cc28;
# }
style_scan <- function(color_dark,color_light){

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

# #470000
# ff3333
# ff6666

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
