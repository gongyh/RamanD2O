# Integration analysis tab
tabItem(
  tabName = "integrate",
  h2("Integration analysis based on O2PLS"),
  br(),
  fluidRow(
    box(
      title = "Settings", status = "info", solidHeader = TRUE, collapsible = FALSE, width = 8,
      column(
        12,
        fluidRow(
          column(12, radioButtons("ig_choice",
            label = "Choose Ramanome dataset source:",
            choices = c("Use existing dataset", "Upload new dataset"),
            selected = "Use existing dataset", inline = TRUE
          ))
        ),
        fluidRow(
          conditionalPanel(
            condition = "input.ig_choice == 'Use existing dataset'",
            column(3, uiOutput("hs_select_for_integrate")),
            column(3, uiOutput("hs_select_for_ig_label")),
            column(2, withBusyIndicatorUI(actionButton("confirm_X", "Confirm ", class = "btn-success")), class = "top25")
          ),
          conditionalPanel(
            condition = "input.ig_choice == 'Upload new dataset'",
            column(4, fileInput("upload_X_file", "Ramanome dataset", accept = ".csv", placeholder = "X.csv")),
            column(2, withBusyIndicatorUI(actionButton("upload_X", "Upload", class = "btn-success")), class = "top25")
          )
        ),
        fluidRow(
          column(4, fileInput("upload_Y_file", "Transcriptome dataset", accept = ".csv", placeholder = "Y.csv")),
          column(2, withBusyIndicatorUI(actionButton("upload_Y", "Upload", class = "btn-success")), class = "top25")
        ),
        hr(),
        fluidRow(
          column(3, numericInput("pars_N_min1", "N_min", 1, min = 1, step = 1)),
          column(3, numericInput("pars_N_max1", "N_max", 5, min = 1, step = 1)),
          column(3, numericInput("pars_Nx_min1", "Nx_min", 0, min = 0, step = 1)),
          column(3, numericInput("pars_Nx_max1", "Nx_max", 5, min = 1, step = 1))
        ),
        fluidRow(
          column(3, numericInput("pars_Ny_min1", "Ny_min", 0, min = 0, step = 1)),
          column(3, numericInput("pars_Ny_max1", "Ny_max", 5, min = 1, step = 1)),
          column(3, ),
          column(3, withBusyIndicatorUI(actionButton("cvadjr", "CV_adjR2", class = "btn-success")), class = "top25")
        ),
        hr(),
        fluidRow(
          column(3, numericInput("pars_N_min2", "N_min", 1, min = 1, step = 1)),
          column(3, numericInput("pars_N_max2", "N_max", 5, min = 1, step = 1)),
          column(3, numericInput("pars_Nx_min2", "Nx_min", 0, min = 0, step = 1)),
          column(3, numericInput("pars_Nx_max2", "Nx_max", 5, min = 1, step = 1))
        ),
        fluidRow(
          column(3, numericInput("pars_Ny_min2", "Ny_min", 0, min = 0, step = 1)),
          column(3, numericInput("pars_Ny_max2", "Ny_max", 5, min = 1, step = 1)),
          column(3, numericInput("pars_fold", "nr_folds", 6, min = 1, step = 1)),
          column(3, withBusyIndicatorUI(actionButton("crossval", "CrossVal", class = "btn-success")), class = "top25")
        ),
        hr(),
        fluidRow(
          column(2, numericInput("pars_N", "N", 5, min = 1, step = 1)),
          column(2, numericInput("pars_Nx", "Nx", 0, min = 0, step = 1)),
          column(2, numericInput("pars_Ny", "Ny", 0, min = 0, step = 1)),
          column(2, numericInput("cpu_cores", "CPU cores", 1, min = 1, max = 30, step = 1)),
          column(2, withBusyIndicatorUI(actionButton("integrate", "O2PLS", class = "btn-success")), class = "top25"),
          column(2, withBusyIndicatorUI(actionButton("integrate2d", "O2PLS-2D", class = "btn-success")), class = "top25")
        )
      )
    ),
    box(
      title = "Notes", status = "info", solidHeader = TRUE, collapsible = FALSE, width = 4,
      p("The integration analysis using the OmicsPLS package.
        The O2PLS (Two-way Orthogonal Partial Least Squares) model is a statistical modeling
        approach applied to two data matrices representing different omics datasets.
        It aims to predict sets of data variables (such as sets of correlated genes and metabolites)
        that have potential associations within the two matrices."),
      p("1. Select X and Y datasets, there are two options for the X dataset, choose one of them."),
      p("2. The parameters N_max/Nx_max/Ny_max are used to search for the optimal value of N/Nx/Ny within ranges."),
      p("3. CV_adjR2: Preliminary calculation of analysis parameters, can be attempted multiple times."),
      p("4. CrossVal: Further calculate the analysis parameters based on the results of CV_adjR2."),
      p("5. Integrate: Integration analysis using the determined parameters N/Nx/Ny.")
    )
  ),
  fluidRow(
    box(
      title = "Results", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = 12,
      column(
        12,
        tabsetPanel(
          tabPanel(
            "CV_adjR2",
            br(),
            DTOutput("cvadjr_result") %>% withSpinner(),
            tags$div(
              style = "text-align: right; margin-right: 20px;",
              downloadButton("ig_result1", "Download", class = "btn-primary")
            )
          ),
          tabPanel(
            "CrossVal",
            br(),
            DTOutput("crossval_result") %>% withSpinner(),
            tags$div(
              style = "text-align: right; margin-right: 20px;",
              downloadButton("ig_result2", "Download", class = "btn-primary")
            )
          ),
          tabPanel(
            "Xjoint",
            br(),
            plotOutput("Xjoint", height = "1000px") %>% withSpinner(),
            tags$div(
              style = "text-align: right; margin-right: 20px;",
              downloadButton("ig_result3", "Download", class = "btn-primary")
            )
          ),
          tabPanel(
            "Yjoint",
            br(),
            plotOutput("Yjoint", height = "1000px") %>% withSpinner(),
            tags$div(
              style = "text-align: right; margin-right: 20px;",
              downloadButton("ig_result4", "Download", class = "btn-primary")
            )
          ),
          tabPanel(
            "X_VIP",
            br(),
            DTOutput("x_vip") %>% withSpinner(),
            tags$div(
              style = "text-align: right; margin-right: 20px;",
              downloadButton("ig_result5", "Download", class = "btn-primary")
            )
          ),
          tabPanel(
            "Y_VIP",
            br(),
            DTOutput("y_vip") %>% withSpinner(),
            tags$div(
              style = "text-align: right; margin-right: 20px;",
              downloadButton("ig_result6", "Download", class = "btn-primary")
            )
          )
        )
      )
    )
  )
)
