# Loading data tab
tabItem(
  tabName = "settings",
  h2("Load data"),
  br(),
  fluidRow(
    box(
      title = "Uploader", status = "info",
      solidHeader = TRUE, collapsible = FALSE,
      br(),
      withBusyIndicatorUI(actionLink("load_demo",
                                     "Quick start, click here to load
                                     a small demo dataset.",
                                     class = "primary")),
      p("You can also import your data from external MongoDB database,
        please refer to Tools -> Database.", style = "color:red;"),
      br(),
      fluidRow(
        column(5, fileInput("scrs_file", "1. Upload SCRS Zip file",
                            accept = ".zip", placeholder = "SCRS.zip")),
        column(2, checkboxInput("align", "Interpolation"), class = "top25"),
        column(5,
               withBusyIndicatorUI(actionButton("unzip",
                                                "Load SCRS",
                                                class = "btn-success")),
               class = "top25")
      ),
      fluidRow(
        column(5, fileInput("meta_file", "2. Upload Metadata",
                            accept = ".tsv", placeholder = "meta.tsv")),
        column(7,
               withBusyIndicatorUI(actionButton("load_meta",
                                                "Load metadata",
                                                class = "btn-success")),
               class = "top25")
      ),
      h5("Notes:"),
      p("The first column of the metadata table should be ID_Cell, which
        contains spectra file names without suffix. If you choose Interpolation,
        Raman spectra will be interpolated at integer frequencies."),
      h5("Tips:"),
      fluidRow(
        column(3, actionLink("tips1", "About Filenames")),
        column(3, actionLink("tips2", "About TSV File"))
      ),
      hr(),
      fluidRow(
        column(5, fileInput("ramex_file", "Upload RamEx dataset",
                            accept = ".rds", placeholder = "ramex.rds")),
        column(7,
               withBusyIndicatorUI(actionButton("load_ramex",
                                                "Load RamExData",
                                                class = "btn-success")),
               class = "top25")
      )
    ),
    box(
      title = "Metadata Table", status = "warning",
      solidHeader = TRUE, collapsible = TRUE,
      column(12, DTOutput("meta_table", height = "400px") %>% withSpinner())
    )
  )
)
