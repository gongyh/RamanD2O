library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(DT)
library(shinyalert)
library(shinydisconnect)
library(shinycssloaders)

source("helpers.R")

options(encoding = "UTF-8")
options(spinner.type = 5, spinner.color = "#bf00ff", spinner.size = 1)

dashboardPage(
  skin = "red",

  # BEGIN dashboardHeader
  dashboardHeader(
    title = "RamanD2O",
    tags$li(
      class = "dropdown",
      tags$a(href = "#", style = "font-size: 20px;", "A ShinyApp to Analyze Raman Spectra Data")
    )
  ),
  # END dashboardHeader

  # BEGIN dashboardSidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Load Data", tabName = "settings", icon = icon("cogs")),
      menuItem("Preprocess",
        menuSubItem("Subsample", tabName = "ss", icon = icon("vials")),
        menuSubItem("Trim", tabName = "trim", icon = icon("cut")),
        menuSubItem("Filter", tabName = "fl", icon = icon("filter")),
        menuSubItem("Smooth", tabName = "sm", icon = icon("wave-square")),
        menuSubItem("Baseline", tabName = "bl", icon = icon("chart-line")),
        menuSubItem("Normalization", tabName = "nl", icon = icon("grip-lines")),
        menuSubItem("Export", tabName = "export", icon = icon("file-export")),
        menuSubItem("SNR", tabName = "snr", icon = icon("signal")),
        tabName = "tools", icon = icon("toolbox"), startExpanded = T
      ),

      div(
        class = "hide_when_sidebar_collapsed",
        helpText("Developed by ", a("Yanhai Gong", href = "mailto:gongyh@qibebt.ac.cn"),
          br(), a("Single Cell Center @ Qibebt, CAS",
            href = "http://singlecellcenter.org/en/index.aspx", target = "_blank"
          ),
          style = "padding-left:1em; padding-right:1em;position:absolute; bottom:1em; "
        )
      )
    )
  ),
  # END dashboardSidebar

  # BEGIN dashboardBody
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    tags$head(includeCSS("style.css")),
    tags$script(HTML("$('body').addClass('sidebar-mini');")),

    shinydisconnect::disconnectMessage2(),

    tabItems(
      source(file.path("ui", "dashboard.R"), local = TRUE)$value,
      source(file.path("ui", "dataloader.R"), local = TRUE)$value,
      source(file.path("ui", "subsample.R"), local = TRUE)$value,
      source(file.path("ui", "trim.R"), local = TRUE)$value,
      source(file.path("ui", "filter.R"), local = TRUE)$value,
      source(file.path("ui", "smooth.R"), local = TRUE)$value,
      source(file.path("ui", "baseline.R"), local = TRUE)$value,
      source(file.path("ui", "normalize.R"), local = TRUE)$value,
      source(file.path("ui", "export.R"), local = TRUE)$value,
      source(file.path("ui", "snratio.R"), local = TRUE)$value
    )
  )
  # END dashboardBody
)
