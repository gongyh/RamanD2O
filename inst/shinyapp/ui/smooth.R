# Smooth tab
tabItem(
  tabName = "sm",
  h2("Smooth interpolation"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(4, uiOutput("hs_select_for_smooth")),
          column(4, checkboxInput("interp", "Interpolation"), class = "top25"),
          column(4, withBusyIndicatorUI(actionButton("smooth", "Smooth", class = "btn-success")), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("1. Choose the target spectra collection from the dropdown selector."),
        p("2. Check the interpolation checkbox to also enable interpolation."),
        p("3. The interpolation will be performed at integer frequencies."),
        p("4. Row of the spectra table (bottom left) can be selected to visual the spectrum (bottom right).")
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("smoothed_table", height = "400px") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, plotlyOutput("after_smooth_plot") %>% withSpinner())
    )
  )
)
