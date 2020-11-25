library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(DT)
library(shinyalert)
library(shinydisconnect)

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

  dashboardBody(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    tags$head(includeCSS("style.css")),
    tags$script(HTML("$('body').addClass('sidebar-mini');")),

    shinydisconnect::disconnectMessage2(),

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
          column(3, actionButton("unzip", "Load SCRS", class = "top25 btn-success")),
          column(3, fileInput("meta_file", "2. Upload Metadata", accept = ".tsv", placeholder = "meta.tsv")),
          column(3, actionButton("load_meta", "Load metadata", class = "top25 btn-success"))
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
        fluidRow(
          column(
            6, sliderInput("percentage", "Percentage to keep:",
              min = 1, max = 100, value = 50, width = "100%"
            ),
            fluidRow(
              column(2, uiOutput("hs_select_for_subsample")),
              column(2, checkboxInput("shuffle", "Shuffle"), class = "top25"),
              column(2, actionButton("subsample", "Subsample", class = "top25 btn-success"))
            ),
            DTOutput("sampled_table")
          ),
          column(6, plotOutput("after_subsample_plot"))
        )
      ),

      # Trim tab
      tabItem(
        tabName = "trim",
        h2("Trim"),
        br(),
        fluidRow(
          column(
            6, sliderInput("trim_range", " Selecting Wavelength Ranges:",
              min = 0, max = 4000, value = c(400, 3400),
              step = 1, dragRange = F, width = "100%"
            ),
            fluidRow(
              column(2, uiOutput("hs_select_for_trim")),
              column(1, actionButton("trim", "Trim", class = "top25 btn-success"))
            ),
            DTOutput("after_trim")
          ),
          column(6, plotOutput("after_trim_plot"))
        )
      ),

      # Filter tab
      tabItem(
        tabName = "fl",
        h2("Filter")
      ),

      # Smooth tab
      tabItem(
        tabName = "sm",
        h2("Smooth & Interpolation")
      ),


      # Baseline tab
      tabItem(
        tabName = "bl",
        h2("Baseline")
      ),


      # Normalization tab
      tabItem(
        tabName = "nl",
        h2("Normalization")
      ),


      # Export tab
      tabItem(
        tabName = "export",
        h2("Export")
      ),


      # SNR tab
      tabItem(
        tabName = "snr",
        h2("SNR")
      )
    )
  )
)
