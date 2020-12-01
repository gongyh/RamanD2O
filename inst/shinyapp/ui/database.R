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
          column(3, withBusyIndicatorUI(actionButton("connectdb", "Connect", class = "btn-success")), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12, p("The URL is in Connection String URI Format, the standard URI connection scheme has the form:",
            p("mongodb://[username:password@]host1[:port1][,...hostN[:portN]][/[defaultauthdb][?options]]"))
      )
    )
  )
)
