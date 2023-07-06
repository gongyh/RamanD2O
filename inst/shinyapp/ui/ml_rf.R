# Random forest tab
tabItem(
  tabName = "rf",
  h2("Training, evaluation and testing"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE,
      column(
        12,
        fluidRow(
          column(3, uiOutput("hs_select_for_ml_train")),
          column(3, uiOutput("hs_select_for_ml_eval")),
          column(3, uiOutput("hs_select_for_ml_label")),
          column(3, uiOutput("hs_select_for_ml_test"))
        ),
        h4("Parameters for Random forest classifier:"),
        fluidRow(
          column(3, numericInput("rf_ntree", "Number of trees", 100, min = 1, step = 1)),
          column(3, checkboxInput("rf_replace", "Replacement", value = TRUE), class = "top25")
        ),
        fluidRow(
          column(3, withBusyIndicatorUI(actionButton("train", "Train & Eval", class = "btn-success")), class = "top25"),
          column(3, withBusyIndicatorUI(actionButton("eval", "Eval", class = "btn-success")), class = "top25"),
          column(3, withBusyIndicatorUI(actionButton("test", "Test", class = "btn-success")), class = "top25")
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
            "Error rates",
            br(),
            plotOutput("rf_mse_plot") %>% withSpinner()
          ),
          tabPanel(
            "Importance",
            br(),
            plotOutput("rf_importance_plot") %>% withSpinner()
          ),
          tabPanel(
            "Confusion (OOB)",
            br(),
            DTOutput("rf_confusion_oob_plot") %>% withSpinner()
          ),
          tabPanel(
            "Confusion (Evaluation)",
            br(),
            DTOutput("rf_confusion_eval_plot") %>% withSpinner()
          ),
          tabPanel(
            "Evaluation results",
            br(),
            DTOutput("rf_test_predicted_plot") %>% withSpinner()
          )
        )
      )
    ),
    box(
      title = "Raman spectra", status = "warning", solidHeader = TRUE, collapsible = FALSE,
      plotlyOutput("selected_predicted_plot") %>% withSpinner()
    )
  )
)
