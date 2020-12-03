# Visualize tab
tabItem(
  tabName = "visualize",
  h2("Visualize"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(4, uiOutput("hs_select_for_export")),
          column(4, selectInput("select_type", "Choose format",
            choices = c("csv", "zip"), selected = "csv"
          )),
          column(4, downloadButton("download", "Download", class = "btn-success"), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("Download zip file will produce an archive with multiple spectrum files in txt format and a metadata table file.")
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("visualize_table", height = "400px") %>% withSpinner())
    ),
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(
        12,
        tabsetPanel(
          tabPanel("Selected Spectrum", plotlyOutput("after_visualize_plot") %>% withSpinner()),
          tabPanel(
            "All spectra",
            fluidRow(
              column(4, selectInput("select_ptype", "Choose type",
                choices = c("spc", "spcmeansd", "spcprctile", "spcprctl5"), selected = "spcprctile"
              )),
              column(4, withBusyIndicatorUI(actionButton("plot_all", "Plot", class = "btn-success")), class = "top25")
            ),
            plotOutput("simple_plot") %>% withSpinner()
          ),
          tabPanel(
            "PCA & HCA",
            fluidRow(
              column(4, numericInput("num_clusters", "How many clusters", 3, min = 1, max = 10, step = 1)),
              column(4, uiOutput("visualize_pcaColBy")),
              column(4, withBusyIndicatorUI(actionButton("plot_pca", "Analysis & Draw", class = "btn-success")), class = "top25")
            ),
            plotlyOutput("pca_plot") %>% withSpinner()
          ),
          tabPanel(
            "Group-wise aggregation",
            fluidRow(
              column(4, uiOutput("visualize_aggBy")),
              column(4, withBusyIndicatorUI(actionButton("plot_agg", "Analysis & Draw", class = "btn-success")), class = "top25")
            ),
            plotOutput("agg_plot") %>% withSpinner()
          ),
          tabPanel("Grouped spectra", plotlyOutput("group_plot") %>% withSpinner())
        )
      )
    )
  )
)
