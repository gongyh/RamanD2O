# Integration analysis tab
tabItem(
  tabName = "integrate",
  h2("Integration analysis"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(12, radioButtons("ig_choice", label = "Choose X dataset source:", choices = c("Use existing dataset (a)", "Upload new dataset (b)"),
            selected = "Use existing dataset (a)", inline = TRUE))
        ),
        fluidRow(
          column(3, uiOutput("hs_select_for_integrate")),
          column(3, uiOutput("hs_select_for_ig_label")),
          column(4, fileInput("upload_X_file", "(b) X dataset", accept = ".csv", placeholder = "X.csv")),
          column(2, withBusyIndicatorUI(actionButton("upload_X", "Upload", class = "btn-success")), class = "top25")
        ),
        fluidRow(
          column(4, fileInput("upload_Y_file", "Y dataset", accept = ".csv", placeholder = "Y.csv")),
          column(2, withBusyIndicatorUI(actionButton("upload_Y", "Upload", class = "btn-success")), class = "top25")
        ),
        fluidRow(
          column(2, numericInput("pars_N_max", "N_max", 10, min = 1, step = 1)),
          column(2, numericInput("pars_Nx_max", "Nx_max", 5, min = 1, step = 1)),
          column(2, numericInput("pars_Ny_max", "Ny_max", 5, min = 1, step = 1)),
          column(2, numericInput("pars_4", "p4", 10, min = 1, step = 1)),
          column(3, withBusyIndicatorUI(actionButton("crossval", "Crossval", class = "btn-success")), class = "top25")
        ),
        h4("Parameters for Integration analysis:"),
        fluidRow(
          column(2, numericInput("pars_N", "N", 10, min = 1, step = 1)),
          column(2, numericInput("pars_Nx", "Nx", 10, min = 1, step = 1)),
          column(2, numericInput("pars_Ny", "Ny", 10, min = 1, step = 1)),
          column(2, numericInput("pars_top", "top", 10, min = 1, step = 1)),
          column(3, withBusyIndicatorUI(actionButton("integrate", "Integrate", class = "btn-success")), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE,
      p("randomForest implements Breiman's random forest algorithm (based on Breiman and Cutler's original Fortran code) for classification and regression.
        The RandomForestClassifier is trained using bootstrap aggregation, where each new tree is fit from a bootstrap sample of the training observations.
        The out-of-bag (OOB) error is the average error for each observation calculated using predictions from the trees that do not contain this observation in their respective bootstrap sample.
        This allows the RandomForestClassifier to be fit and validated whilst being trained."),
      p("1. If you also want to evaluate your model, please change Evaluation dataset to anything except _ ."),
      p("2. Change Label to your categorical variable."),
      p("3. Number of trees should not be set to too small to ensure that every input row gets predicted at least a few times."),
      p("4. Sampling of cases (replicate) can be done with or without replacement.")
    )
  ),
  fluidRow(
    box(
      title = "Results", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = 6,
      column(
        12,
        tabsetPanel(
          tabPanel(
            "01",
            br(),
            plotOutput("a01") %>% withSpinner(),
            tags$div(style = "text-align: right; margin-right: 20px;",
              downloadButton("adownload_result1", "Download", class = "btn-primary"))
          ),
          tabPanel(
            "02",
            br(),
            plotOutput("a02") %>% withSpinner(),
            tags$div(style = "text-align: right; margin-right: 20px;",
              downloadButton("adownload_result2", "Download", class = "btn-primary"))
          ),
          tabPanel(
            "03",
            br(),
            DTOutput("a03") %>% withSpinner(),
            tags$div(style = "text-align: right; margin-right: 20px; margin-top: 20px;",
              downloadButton("adownload_result3", "Download", class = "btn-primary"))
          ),
          tabPanel(
            "04",
            br(),
            DTOutput("a04") %>% withSpinner(),
            tags$div(style = "text-align: right; margin-right: 20px; margin-top: 20px;",
              downloadButton("adownload_result4", "Download", class = "btn-primary"))
          ),
          tabPanel(
            "05",
            br(),
            DTOutput("a05") %>% withSpinner(),
            tags$div(style = "text-align: right; margin-right: 20px; margin-top: 20px;",
              downloadButton("adownload_result5", "Download", class = "btn-primary"))
          )
        )
      )
    ),
    box(
      title = "Raman spectra", status = "warning", solidHeader = TRUE, collapsible = FALSE,
      plotlyOutput("aselected_predicted_plot") %>% withSpinner()
    )
  )
)

