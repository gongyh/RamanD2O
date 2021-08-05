# Carotenoid tab
tabItem(
  tabName = "ct",
  h2("Carotenoid containing cells identification"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(4, uiOutput("hs_select_for_carotenoid")),
          column(2, checkboxInput("filter_carotenoid", "Remove", value = T), class = "top25"),
          column(4, withBusyIndicatorUI(actionButton("carotenoid", "Identification", class = "btn-success")), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("1. Detect and/or remove carotenoid containing cells.")
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("carotenoid_table", height = "400px") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, plotlyOutput("after_carotenoid_plot") %>% withSpinner())
    )
  )
)
