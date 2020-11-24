library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(DT)

options(encoding = "UTF-8")

dashboardPage(
  skin = "red",

  dashboardHeader(
    title = "RamanD2O",
    tags$li(
      class = "dropdown",
      tags$a(href = "#", style = "font-size: 20px;", "A ShinyApp to Analyze Raman Spectra Data")
    )
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Load Data", tabName = "settings", icon = icon("cogs")),
      menuItem("Preprocess",
        menuSubItem("Subsample", tabName = "ss", icon = icon("vials")),
        menuSubItem("Smooth", tabName = "sm", icon = icon("wave-square")),
        menuSubItem("Baseline", tabName = "bl", icon = icon("chart-line")),
        menuSubItem("Normalization", tabName = "nl", icon = icon("grip-lines")),
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

  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(includeCSS("style.css")),
    tags$script(HTML("$('body').addClass('sidebar-mini');")),

    tabItems(
      # Introduction tab
      tabItem(
        tabName = "dashboard",
        h3("Introduction", style = "text-align:center;")
      ),

      # Loading data tab
      tabItem(
        tabName = "settings",
        h2("Load data"),
        br(),
        fluidRow(
          column(3, fileInput("scrs_file", "1. Upload SCRS Zip file", accept = ".zip", placeholder = "SCRS.zip")),
          column(3, actionButton("unzip", "Load SCRS")),
          column(3, fileInput("meta_file", "2. Upload Metadata", accept = ".tsv", placeholder = "meta.tsv")),
          column(3, actionButton("load_meta", "Load metadata"))
        ),
        br(),
        fluidRow(
          column(6, DTOutput("spectra_files")),
          column(6, DTOutput("meta_table")),
        )
      ),

      # Subsample tab
      tabItem(
        tabName = "ss",
        h2("Subsample"),
        br(),
        h4("Notice: Please make sure to only subsample once!"),
        br(),
        fluidRow(
          column(4, sliderInput("percentage", "Percentage to keep:",
                                min = 1, max = 100, value = 50),
                 fluidRow(
                   column(2, checkboxInput("shuffle", "Shuffle")),
                   column(1, actionButton("subsample", "Subsample"))
                 )),
          column(8, DTOutput("sampled_table"))
        )
      )

    )
  )
)
