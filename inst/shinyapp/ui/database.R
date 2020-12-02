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
        fluidRow(
          column(2, textInput("collection", "Collection", value = "test")),
          column(2, textInput("db", "db", value = "test")),
          column(5, textInput("url", "url", value = "mongodb://localhost")),
          column(3, actionButton("connectdb", "Connect", class = "btn-success"), class = "top25")
        ),
        verbatimTextOutput("db_message", placeholder = T),
        hr(),
        fluidRow(
          column(6, uiOutput("hs_select_for_database")),
          column(6, withBusyIndicatorUI(actionButton("savedb", "Load to database", class = "btn-success")), class = "top25")
        )
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
