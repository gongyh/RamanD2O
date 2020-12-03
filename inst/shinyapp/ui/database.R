# Database tab
tabItem(
  tabName = "database",
  h2("Database"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        h4("Database connection"),
        fluidRow(
          column(2, textInput("collection", "Collection", value = "test")),
          column(2, textInput("db", "db", value = "test")),
          column(5, textInput("url", "url", value = "mongodb://localhost")),
          column(3, actionButton("connectdb", "Connect", class = "btn-success"), class = "top25")
        ),
        verbatimTextOutput("db_message", placeholder = T),
        hr(),
        h4("Export spectra data"),
        fluidRow(
          column(4, uiOutput("hs_select_for_database")),
          column(4, textInput("project", "Project accession", value = "CRP00000000")),
          column(4, withBusyIndicatorUI(actionButton("savedb", "Load to database", class = "btn-success")), class = "top25")
        ),
        verbatimTextOutput("db_message2", placeholder = T),
        HTML("<p><b>Note:</b> Please contact your database manager to add proper indexes, e.g. unqiue compound index on <b>ID_Cell</b> and <b>dtype</b>, regular index on <b>project</b>.</p>"),
        hr(),
        h4("Load spectra data"),
        fluidRow(
          column(4, uiOutput("project_select_for_database")),
          column(4, withBusyIndicatorUI(actionButton("load_from_db", "Load raw data", class = "btn-success")),
            class = "top25", style = "text-align: center;"
          ),
          column(4, p("You may need to refresh the database connection to let your project selectable."))
        ),
        verbatimTextOutput("db_message3", placeholder = T)
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("The URL is in Connection String URI Format, the standard URI connection scheme has the form:"),
        p("mongodb://[username:password@]host1[:port1][,...hostN[:portN]][/[defaultauthdb][?options]]")
      )
    )
  )
)
