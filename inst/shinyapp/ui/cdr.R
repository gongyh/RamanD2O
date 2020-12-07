# CDR tab
tabItem(
  tabName = "cdr",
  h2("C/D Ratio"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(4, uiOutput("hs_select_for_cdr")),
          column(4, withBusyIndicatorUI(actionButton("cdr", "Calc CDR", class = "btn-success")), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("1. CD intensity region: 2050 ~ 2300"),
        p("2. CH intensity region: 2800 ~ 3050"),
        p("3. CDR = area(CD) / (area(CD) + area(CH))")
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("cdr_table", height = "400px") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, plotlyOutput("after_cdr_plot") %>% withSpinner())
    )
  )
)
