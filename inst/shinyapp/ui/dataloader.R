# Loading data tab
tabItem(
  tabName = "settings",
  h2("Load data"),
  br(),
  p("If have imported your data from external database, then no need to reload here.", style = "color:red;"),
  withBusyIndicatorUI(actionLink("load_demo", "Quick start, load demo data", class = "primary")),
  br(),
  fluidRow(
    box(
      title = "Spectra Uploader", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(6, fileInput("scrs_file", "1. Upload SCRS Zip file", accept = ".zip", placeholder = "SCRS.zip")),
      column(6, withBusyIndicatorUI(actionButton("unzip", "Load SCRS", class = "btn-success")), class = "top25")
    ),
    box(
      title = "Metadata Uploader", status = "warning", solidHeader = TRUE, collapsible = FALSE,
      column(6, fileInput("meta_file", "2. Upload Metadata", accept = ".tsv", placeholder = "meta.tsv")),
      column(6, withBusyIndicatorUI(actionButton("load_meta", "Load metadata", class = "btn-success")), class = "top25")
    )
  ),
  br(),
  fluidRow(
    box(
      title = "Spectra Table", status = "info", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("spectra_files", height = "400px") %>% withSpinner())
    ),
    box(
      title = "Metadata Table", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("meta_table", height = "400px") %>% withSpinner())
    )
  )
)
