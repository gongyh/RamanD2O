# Trim tab
tabItem(
  tabName = "trim",
  h2("Trim"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      fluidRow(
        column(2, numericInput("trim_min", "Min", min = 0, max = 4000, step = 1, value = 400, width = "100%")),
        column(
          8,
          sliderInput("trim_range", " Selecting Wavelength Ranges:",
            min = 0, max = 4000, value = c(400, 3400), step = 1, dragRange = F, width = "100%"
          )
        ),
        column(2, numericInput("trim_max", "Max", min = 0, max = 4000, step = 1, value = 3400, width = "100%"))
      ),
      fluidRow(
        column(6, uiOutput("hs_select_for_trim")),
        column(6, withBusyIndicatorUI(actionButton("trim", "Trim", class = "btn-success")), class = "top25")
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
      column(12, DTOutput("after_trim", height = "400px") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, plotlyOutput("after_trim_plot") %>% withSpinner())
    )
  )
)
