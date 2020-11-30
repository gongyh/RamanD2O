# SNR tab
tabItem(
  tabName = "snr",
  h2("SNR"),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(4, uiOutput("hs_select_for_snr")),
          column(4, selectInput("select_snr", "Select method",
            choices = c("new", "old"), selected = "new"
          )),
          column(4, withBusyIndicatorUI(actionButton("snr", "Calculate", class = "btn-success")), class = "top25")
        ),
        fluidRow(
          column(4, checkboxInput("filter_by_snr", "Remove Low-SNR Spectra"), class = "top25"),
          column(4, numericInput("snr_cutoff", "SNR Cutoff: (0~30)",
            min = 0, max = 30, step = 0.1, value = 2.5, width = "100%"
          ))
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("1. Choose the target spectra collection from the dropdown selector."),
        p("2. Choose Signal/Noise ratio calculation method."),
        p("3. Spectra can be filtered by SNR (default >= 2.5; optional)."),
        p("4. Row of the spectra table (bottom left) can be selected to visual the spectrum (bottom right).")
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("snr_table") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, plotlyOutput("after_snr_plot") %>% withSpinner())
    )
  )
)
