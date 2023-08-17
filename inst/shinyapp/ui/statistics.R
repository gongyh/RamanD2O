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
              column(2, uiOutput("visualize_pcaColBy")),
              column(2, numericInput("num_clusters", "How many clusters", 1, min = 1, max = 10, step = 1)),
              column(1, checkboxInput("pca_scale", "Scale", value = T), class = "top25"),
              column(1, checkboxInput("pca_center", "Center", value = T), class = "top25"),
              column(2, withBusyIndicatorUI(actionButton("plot_pca", "Analysis & Draw", class = "btn-success")), class = "top25")
            ),
            plotlyOutput("pca_plot", width = "600px", height = "600px") %>% withSpinner()
          ),
          tabPanel(
            "LDA",
            br(),
            fluidRow(
              column(1, checkboxInput("use_pca", "PCA-LDA", value = T), class = "top25"),
              column(2, numericInput("num_pcs", "PCA components to keep", 2, min = 1, max = 10, step = 1)),
              column(2, uiOutput("statistics_ldaBy")),
              column(2, numericInput("lda_eval_pct", "Percent of evaluation set", 30, min = 5, max = 50, step = 1)),
              column(4, withBusyIndicatorUI(actionButton("perform_lda", "(PCA-)LDA analysis", class = "btn-success")), class = "top25")
            ),
            plotlyOutput("lda_plot", width = "600px", height = "600px") %>% withSpinner()
          )
        )
      )
    )
  )
)
