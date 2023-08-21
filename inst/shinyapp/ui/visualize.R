# Visualize tab
tabItem(
  tabName = "visualize",
  h2("Visualize"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE, width = 12,
      column(
        12,
        fluidRow(
          column(2, uiOutput("hs_select_for_export")),
          column(2, selectInput("select_type", "Choose format",
            choices = c("csv", "zip"), selected = "csv"
          )),
          column(2, withBusyIndicatorUI(actionButton("prepare_file", "Prepare file", class = "btn-success")), class = "top25"),
          column(2, downloadButton("download", "Download", class = "btn-success disabled"), class = "top25"),
          column(4, p("Download zip file will produce an archive with multiple spectrum files in txt format and a metadata table file."), class = "top25")
        )
      )
    )
  ),
  fluidRow(
    box(
      title = "Spectra Figure", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = 12,
      column(
        12,
        tabsetPanel(
          tabPanel(
            "All",
            br(),
            fluidRow(
              column(2, selectInput("select_ptype", "Choose type",
                choices = c("spc", "spcmeansd", "spcprctile", "spcprctl5"), selected = "spcprctile"
              )),
              column(2, withBusyIndicatorUI(actionButton("plot_all", "Plot", class = "btn-success")), class = "top25")
            ),
            plotOutput("simple_plot") %>% withSpinner()
          ),
          tabPanel(
            "Aggregation",
            br(),
            fluidRow(
              column(2, uiOutput("visualize_aggBy")),
              column(2, withBusyIndicatorUI(actionButton("plot_agg", "Analysis & Draw", class = "btn-success")), class = "top25")
            ),
            plotOutput("agg_plot") %>% withSpinner()
          ),
          tabPanel(
            "Compare",
            br(),
            fluidRow(
              column(2, uiOutput("visualize_x")),
              column(2, uiOutput("visualize_y")),
              column(2, uiOutput("visualize_sgroup")),
              column(2, uiOutput("visualize_scolor"))
            ),
            fluidRow(
              column(2, uiOutput("visualize_facet")),
              column(2, selectInput("stype", "Plot type", choices = c("Lineplot", "Boxplot"), selected = "Lineplot")),
              column(2, withBusyIndicatorUI(actionButton("plot_compare", "Plot", class = "btn-success")), class = "top25")
            ),
            plotlyOutput("groupCmp_plot") %>% withSpinner()
          ),
          tabPanel(
            "Selected",
            br(),
            fluidRow(
              column(6, DTOutput("visualize_table", height = "400px") %>% withSpinner()),
              column(6, plotlyOutput("after_visualize_plot") %>% withSpinner())
            )
          )
        )
      )
    )
  )
)
