# Prepare tab
tabItem(
  tabName = "prepare",
  h2("Prepare training & validation datasets"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(1, checkboxInput("prepare_trim", "Trim", value = TRUE), class = "top25"),
          column(2, numericInput("ptrim_min", "Min", min = 0, max = 4000, step = 1, value = 2000, width = "100%")),
          column(7, sliderInput("ptrim_range", " Selecting Wavelength Ranges:",
            min = 0, max = 4000, value = c(2000, 2350),
            step = 1, dragRange = F, width = "100%"
          )),
          column(2, numericInput("ptrim_max", "Max", min = 0, max = 4000, step = 1, value = 2350, width = "100%"))
        ),
        fluidRow(
          column(3, uiOutput("hs_select_for_ml_prepare")),
          column(3, numericInput("train_pct", "Percent for training", 80, min = 50, max = 95, step = 1)),
          column(4, withBusyIndicatorUI(actionButton("prepare", "Prepare datasets", class = "btn-success")), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("1. Select the wavelength region to keep, only one region is supported."),
        p("2. The wavelength region can be broader than your spectra, but should not be too narrow."),
        p("3. Choose the target spectra collection from the dropdown selector."),
        p("4. Row of the spectra table (bottom left) can be selected to visual the spectrum (bottom right).")
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("after_prepare", height = "400px") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, plotlyOutput("after_prepare_plot") %>% withSpinner())
    )
  )
)
