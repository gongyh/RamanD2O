# Baseline tab
tabItem(
  tabName = "bl",
  h2("Baseline"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(6, uiOutput("hs_select_for_baseline")),
          column(6, withBusyIndicatorUI(actionButton("baseline", "Baseline", class = "btn-success")), class = "top25")
        ),
        fluidRow(
          column(6, selectInput("select_baseline", "Baseline method",
            choices = c("polyfit", "als"), selected = "als"
          )),
          column(6, uiOutput("baseline_config"))
        ),
        radioButtons("select_negative", "How to handle negative values",
          choices = c(
            "Set to zero" = "zero",
            "Pull up" = "up",
            "Keep intact" = "keep"
          ),
          selected = "keep", inline = T
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("1. Choose the target spectra collection from the dropdown selector."),
        p("2. Choose baseline method, currently support polyfit and ALS."),
        p("3. Negative values can be set to zero, or pull the whole spectrum up, or leave as it is."),
        p("4. Row of the spectra table (bottom left) can be selected to visual the spectrum (bottom right).")
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("baselined_table") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, plotlyOutput("after_baseline_plot") %>% withSpinner())
    )
  )
)
