# Introduction tab
tabItem(
  tabName = "dashboard",
  h3("RamanD2O: A ShinyApp to Analyze Raman Spectra Data",
     style = "text-align:center;"),
  fluidRow(
    column(12, includeMarkdown(file.path("ui", "dashboard.md")))
  )
)
