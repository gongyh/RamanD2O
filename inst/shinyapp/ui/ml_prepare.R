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
        uiOutput("ml_trim_multi"),
        fluidRow(
          column(3, uiOutput("hs_select_for_ml_prepare")),
          column(3, numericInput("train_pct", "Percent for training", 80, min = 50, max = 95, step = 1)),
          column(1, checkboxInput("ml_trim", "Trim", value = TRUE), class = "top25"),
          column(1, br(), actionButton("ml_plusButton", "", icon = icon("plus"))),
          column(1, br(), actionButton("ml_minusButton", "", icon = icon("minus"))),
          column(2, withBusyIndicatorUI(actionButton("prepare", "Prepare datasets", class = "btn-success")), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("1. Select the wavelength region to keep, use \"+\" and \"-\" buttons to select multiple ranges."),
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
