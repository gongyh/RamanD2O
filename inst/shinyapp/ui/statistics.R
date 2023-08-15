# Statistics tab
tabItem(
  tabName = "statistics",
  h2("Statistics"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE, width = 12,
      column(
        12,
        fluidRow(
          column(2, uiOutput("hs_select_for_statistics"))
        )
      )
    )
  ),
  fluidRow(
    box(
      title = "Multivariable Statistics", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = 12,
      column(
        12,
        tabsetPanel(
          tabPanel(
            "PCA",
            br(),
            fluidRow(
              column(2, numericInput("num_clusters", "How many clusters", 1, min = 1, max = 10, step = 1)),
              column(2, uiOutput("visualize_pcaColBy")),
              column(2, withBusyIndicatorUI(actionButton("plot_pca", "Analysis & Draw", class = "btn-success")), class = "top25")
            ),
            plotlyOutput("pca_plot", height = "800px") %>% withSpinner()
          )
        )
      )
    )
  )
)
