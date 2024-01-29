# Information tab
tabItem(
  tabName = "info",
  h3("SessionInfo"),
  br(),
  fluidRow(
    column(12, verbatimTextOutput("sessionInfo"))
  )
)
