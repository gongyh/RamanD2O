# Explore tab
tabItem(
  tabName = "explore",
  h2("Dataset exploration"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info",
      solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(3, uiOutput("hs_select_for_ml_explore")),
          column(2, numericInput("perplexity", "Perplexity",
                                 30, min = 1, step = 1)),
          column(2, numericInput("max_iter", "Iterations",
                                 1000, min = 1, step = 1)),
          column(2, uiOutput("tsneColBy")),
          column(3,
                 withBusyIndicatorUI(actionButton("tsne",
                                                  "Plot tSNE",
                                                  class = "btn-success")),
                 class = "top25")
        ),
        hr(),
        h4("Notes:"),
        p("1. t-SNE converts affinities of data points to probabilities.
          The affinities in the original space are represented by Gaussian
          joint probabilities and the affinities in the embedded space are
          represented by Student's t-distributions."),
        p("2. t-SNE algorithm is stochastic and multiple restarts with
          different seeds can yield different embeddings."),
        p("3. Perplexity parameter should not be bigger than
          3 * perplexity < nrow(X) - 1")
      )
    ),
    box(
      title = "BH-tSNE", status = "warning",
      solidHeader = TRUE, collapsible = FALSE,
      column(12, plotlyOutput("after_tsne_plot") %>% withSpinner())
    )
  )
)
