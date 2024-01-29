# Filter tab
tabItem(
  tabName = "fl",
  h2("Filter"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(4, uiOutput("hs_select_for_filter")),
          column(4, withBusyIndicatorUI(actionButton("filter", "Filter", class = "btn-success")), class = "top25"),
          column(4, p("All selected filters will be performed sequentially."), class = "top25")
        ),
        fluidRow(
          column(2, numericInput("filter_min", "Min", min = 0, max = 4000, step = 1, value = 0, width = "100%")),
          column(
            8,
            sliderInput("filter_range", " Selecting Wavelength Ranges for Filtering:",
              min = 0, max = 4000, value = c(0, 4000), step = 1, dragRange = FALSE, width = "100%"
            )
          ),
          column(2, numericInput("filter_max", "Max", min = 0, max = 4000, step = 1, value = 4000, width = "100%"))
        ),
        fluidRow(
          column(4, checkboxInput("filter_low", "By lowest intensity", value = TRUE), class = "top25"),
          column(4, numericInput("lowest", "Value", 0)),
          column(4, p("Filter spectra with too low signal."), class = "top25")
        ),
        fluidRow(
          column(4, checkboxInput("filter_high", "By highest intensity"), class = "top25"),
          column(4, numericInput("highest", "Value", 100000)),
          column(4, p("Filter spectra with too high signal."), class = "top25")
        ),
        fluidRow(
          column(4, checkboxInput("filter_sd", "By standard deviations"), class = "top25"),
          column(4, numericInput("n_sd", "n", 5, min = 1)),
          column(4, p("Filter outliers which are outside n*sd."), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("1. Select each checkbox to enable corresponding filter criteria."),
        p("2. Outlier filter: outside mean +- n*sd."),
        p("3. Choose the target spectra collection from the dropdown selector."),
        p("4. Row of the spectra table (bottom left) can be selected to visual the spectrum (bottom right).")
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("after_filter", height = "400px") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      fluidRow(
        column(1, withBusyIndicatorUI(actionButton("prev_rm", label = icon("arrow-up"), class = "btn-success")), class = "top25", style = "text-align: center;"),
        column(1, withBusyIndicatorUI(actionButton("next_rm", label = icon("arrow-down"), class = "btn-success")), class = "top25", style = "text-align: center;"),
        column(2, withBusyIndicatorUI(actionButton("remove", "Remove", class = "btn-success")), class = "top25", style = "text-align: center;"),
        column(8, p("Caution: you use this button to manually remove the selected spectrum, need double check!"), class = "top25")
      ),
      hr(),
      column(12, plotlyOutput("after_filter_plot") %>% withSpinner())
    )
  )
)
