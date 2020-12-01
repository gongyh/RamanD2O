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
      column(12, plotlyOutput("after_visualize_plot") %>% withSpinner())
    )
  )
)
