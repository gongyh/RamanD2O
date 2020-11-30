# Normalization tab
tabItem(
  tabName = "nl",
  h2("Normalization"),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(6, uiOutput("hs_select_for_normalize")),
          column(6, withBusyIndicatorUI(actionButton("normalize", "Normalize", class = "btn-success")), class = "top25")
        ),
        fluidRow(
          column(6, selectInput("select_normalize", "Normalize method",
            choices = c("area"), selected = "area"
          )),
          column(6, checkboxInput("fingerprint", "Fingerprint", value = TRUE), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("1. Choose the target spectra collection from the dropdown selector."),
        p("2. Choose normalize method, currently support area normalization."),
        p("3. Check fingerprint checkbox to normalize according to wavelength range of 500 ~ 2000."),
        p("4. Row of the spectra table (bottom left) can be selected to visual the spectrum (bottom right).")
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("normalized_table", height = "400px") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, plotlyOutput("after_normalize_plot") %>% withSpinner())
    )
  )
)
