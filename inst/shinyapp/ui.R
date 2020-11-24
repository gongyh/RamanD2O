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
      menuItem("Tools", tabName = "tools", icon = icon("toolbox")),
      sidebarMenuOutput("subs0"), # Basic workflow information

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
      # First tab content
      tabItem(
        tabName = "dashboard",
        h3("Introduction", style = "text-align:center;")
      ),

      # Second tab content
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
        verbatimTextOutput("runtimeInfo", placeholder = TRUE),
        tableOutput("zipped")
      )
    )
  )
)
