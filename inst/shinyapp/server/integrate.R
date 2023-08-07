output$hs_select_for_integrate <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("train" %in% hs_all) {
    selected <- "train"
  } else if ("cdr" %in% hs_all) {
    selected <- "cdr"
  } else if ("snr" %in% hs_all) {
    selected <- "snr"
  }
  selectInput("hs_selector_for_integrate", "(a) X dataset", choices = hs_all, selected = selected)
})

observeEvent(input$hs_selector_for_integrate,
  {
    hs_cur <- NULL
    if (!is.null(input$hs_selector_for_integrate)) {
      hs_cur <- hs$val[[input$hs_selector_for_integrate]]
    }
    output$hs_select_for_ig_label <- renderUI({
      metacols <- c("")
      if (!is.null(hs_cur)) {
        metacols <- colnames(hs_cur)
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("ig_select_label", "Group label", choices = metacols, selected = F)
    })
  },
  ignoreNULL = FALSE
)

# upload X/Y dataset
observeEvent(input$upload_X, {
  withBusyIndicatorServer("upload_X", {
    if (!is.null(isolate(input$upload_X_file$datapath))) {
      ig$upload_X <- read.table(isolate(input$upload_X_file$datapath), header = T, sep = ",", row.names = 1, stringsAsFactors = T)
      ig$upload_X <- scale(ig$upload_X, scale=F)
    } else {
      toastr_error("No file selected!", position = "top-center")
    }
  })
})

observeEvent(input$upload_Y, {
  withBusyIndicatorServer("upload_Y", {
    if (!is.null(isolate(input$upload_Y_file$datapath))) {
      ig$upload_Y <- read.table(isolate(input$upload_Y_file$datapath), header = T, sep = ",", row.names = 1, stringsAsFactors = T)
      ig$upload_Y <- scale(ig$upload_Y, scale=F)
    } else {
      toastr_error("No file selected!", position = "top-center")
    }
  })
})

observeEvent(input$integrate,
  {
  }
)


# prepare training datasets for scrs on click of button
observeEvent(input$integrate,
  {
  },
  ignoreNULL = T
)


