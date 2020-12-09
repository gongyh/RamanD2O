# Cartenoid tab
tabItem(
  tabName = "ct",
  h2("Cartenoid containing cells identification"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(4, uiOutput("hs_select_for_cartenoid")),
          column(2, checkboxInput("filter_cartenoid", "Remove"), class = "top25"),
          column(4, withBusyIndicatorUI(actionButton("cartenoid", "Identification", class = "btn-success")), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("1. Detect and/or remove cartenoid containing cells.")
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("cartenoid_table", height = "400px") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, plotlyOutput("after_cartenoid_plot") %>% withSpinner())
    )
  )
)
