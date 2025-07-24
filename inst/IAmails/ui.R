library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(DT)
library(googleLanguageR)

shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  uiOutput("questrep"),
  uiOutput("scan"),
  uiOutput("admin"),
  uiOutput("controle"),
  uiOutput("IA")
))
