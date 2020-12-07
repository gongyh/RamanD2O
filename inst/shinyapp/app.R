source("globals.R")
source("helpers.R")

ui <- dashboardPage(
  md = FALSE,
  skin = "blue",

  # BEGIN dashboardHeader
  dashboardHeader(
    title = "RamanD2O",
    tags$li(
      class = "dropdown",
      tags$a(href = "#", style = "font-size: 20px;", "A ShinyApp to Analyze Raman Spectra Data  ")
    ),
    userOutput("user")
  ),
  # END dashboardHeader

  controlbar = dashboardControlbar(collapsed = T, skinSelector()),

  # BEGIN dashboardSidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Load Data", tabName = "settings", icon = icon("cogs")),
      menuItem("Pipeline",
        menuSubItem("Sample", tabName = "ss", icon = icon("crosshairs")),
        menuSubItem("Trim", tabName = "trim", icon = icon("cut")),
        menuSubItem("Filter", tabName = "fl", icon = icon("filter")),
        menuSubItem("Smooth", tabName = "sm", icon = icon("wave-square")),
        menuSubItem("Baseline", tabName = "bl", icon = icon("chart-line")),
        menuSubItem("Normalize", tabName = "nl", icon = icon("compress-arrows-alt")),
        menuSubItem("SNR", tabName = "snr", icon = icon("signal")),
        menuSubItem("CDR", tabName = "cdr", icon = icon("battery-half")),
        tabName = "pipeline", icon = icon("project-diagram"), startExpanded = T
      ),
      menuItem("Tools",
        menuItem("Visualize", tabName = "visualize", icon = icon("poll")),
        menuItem("Database", tabName = "database", icon = icon("coins")),
        tabName = "tools", icon = icon("toolbox"), startExpanded = T
      ),
      menuItem("Machine learning",
        menuItem("Prepare", tabName = "prepare", icon = icon("hourglass-start")),
        menuItem("Explore", tabName = "explore", icon = icon("eye")),
        menuItem("Random forest", tabName = "rf", icon = icon("tree")),
        tabName = "ml", icon = icon("robot"), startExpanded = T
      ),

      div(
        class = "hide_when_sidebar_collapsed",
        helpText("Developed by ", a("Yanhai Gong", href = "mailto:gongyh@qibebt.ac.cn"),
          br(), a("SCC, QIBEBT, CAS",
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
    useToastr(),

    tabItems(
      source(file.path("ui", "dashboard.R"), local = TRUE)$value,

      source(file.path("ui", "dataloader.R"), local = TRUE)$value,

      source(file.path("ui", "subsample.R"), local = TRUE)$value,
      source(file.path("ui", "trim.R"), local = TRUE)$value,
      source(file.path("ui", "filter.R"), local = TRUE)$value,
      source(file.path("ui", "smooth.R"), local = TRUE)$value,
      source(file.path("ui", "baseline.R"), local = TRUE)$value,
      source(file.path("ui", "normalize.R"), local = TRUE)$value,
      source(file.path("ui", "snratio.R"), local = TRUE)$value,
      source(file.path("ui", "cdr.R"), local = TRUE)$value,

      source(file.path("ui", "visualize.R"), local = TRUE)$value,
      source(file.path("ui", "database.R"), local = TRUE)$value,

      source(file.path("ui", "ml_prepare.R"), local = TRUE)$value
    )
  ),
  # END dashboardBody

  footer = dashboardFooter(left = "By Yanhai Gong", right = "SCC, QIBEBT, CAS, 2020")
)

server <- function(input, output, session) {
  # set user info
  if (user_name == "Anonymous" && !is.null(session$user)) {
    user_name <- session$user
  }
  output$user <- renderUser({
    dashboardUser(
      name = str_trunc(user_name, 20),
      image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg"
    )
  })

  # print one dot every minute to prevent gray-out
  autoInvalidate <- reactiveTimer(60000)
  observe({
    autoInvalidate()
    cat(".")
  })

  # load demo data
  observeEvent(input$load_demo, {
    withBusyIndicatorServer("load_demo", {
      scrs$spc <- readRDS("scrs_spc.RDS")
      meta$tbl <- readRDS("meta_tbl.RDS")
      hs$val[["raw"]] <- readRDS("hs_raw.RDS")
    })
  })

  source(file.path("server", "unzip_scrs.R"), local = TRUE)$value
  source(file.path("server", "load_meta.R"), local = TRUE)$value
  source(file.path("server", "subsample.R"), local = TRUE)$value
  source(file.path("server", "trim.R"), local = TRUE)$value
  source(file.path("server", "filter.R"), local = TRUE)$value
  source(file.path("server", "smooth.R"), local = TRUE)$value
  source(file.path("server", "baseline.R"), local = TRUE)$value
  source(file.path("server", "normalize.R"), local = TRUE)$value
  source(file.path("server", "visualize.R"), local = TRUE)$value
  source(file.path("server", "database.R"), local = TRUE)$value
  source(file.path("server", "snr.R"), local = TRUE)$value
  source(file.path("server", "cdr.R"), local = TRUE)$value
  source(file.path("server", "ml_prepare.R"), local = TRUE)$value
}

# Create Shiny object
shinyApp(ui = ui, server = server)
