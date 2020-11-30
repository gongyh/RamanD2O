# Subsample tab
tabItem(
  tabName = "ss",
  h2("Subsample"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        sliderInput("percentage", "Percentage to keep:",
          min = 1, max = 100, value = 50, width = "100%"
        ),
        fluidRow(
          column(4, uiOutput("hs_select_for_subsample")),
          column(4, checkboxInput("shuffle", "Shuffle"), class = "top25"),
          column(4, withBusyIndicatorUI(actionButton("subsample", "Subsample", class = "btn-success")), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("1. If you only want to shuffle those spectram, set the slider to 100."),
        p("2. Choose the target spectra collection from the dropdown selector."),
        p("3. Check shuffle checkbox if you want to shuffle your spectra collection."),
        p("4. Row of the spectra table (bottom left) can be selected to visual the spectrum (bottom right).")
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("sampled_table", height = "400px") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, plotlyOutput("after_subsample_plot") %>% withSpinner())
    )
  )
)
