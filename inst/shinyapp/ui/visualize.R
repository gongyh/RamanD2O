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
                                choices = c("csv","zip"), selected = "csv"
          )),
          column(4, downloadButton("download", "Download", class = "btn-success"), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("Download zip file will produce a zip file with multiple txt files, one file for each spectrum.")
      )
    )
  )
)
