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

observeEvent(input$cvadjr, {
  withBusyIndicatorServer("cvadjr", {
    ig$cvadjr <- crossval_o2m_adjR2(ig$upload_X, ig$upload_Y, 1:isolate(input$pars_N_max1),
      0:isolate(input$pars_Nx_max1), 0:isolate(input$pars_Ny_max1), nr_folds=2, nr_cores=1)
  })
})

observeEvent(input$crossval,{
  withBusyIndicatorServer("crossval",{
    ig$crossval <- crossval_o2m(ig$upload_X, ig$upload_Y, 1:isolate(input$pars_N_max2),
      0:isolate(input$pars_Nx_max2), 0:isolate(input$pars_Ny_max2), nr_folds=isolate(input$pars_fold), nr_cores=5)
    index <- which(ig$crossval$Original == min(ig$crossval$Original, na.rm = T), arr.ind = TRUE)
    Nx <- dimnames(ig$crossval$Original)[[1]][index[1]]
    Ny <- dimnames(ig$crossval$Original)[[2]][index[2]]
    N <- dimnames(ig$crossval$Original)[[3]][index[3]]
    Nx <- as.numeric(str_extract(Nx, "(?<=ax=)\\d+"))
    Ny <- as.numeric(str_extract(Ny, "(?<=ay=)\\d+"))
    N <- as.numeric(str_extract(N, "(?<=a=)\\d+"))
    ig$number <- c(N, Nx, Ny)
    updateNumericInput(session, "pars_N", value = ig$number[1])
    updateNumericInput(session, "pars_Nx", value = ig$number[2])
    updateNumericInput(session, "pars_Ny", value = ig$number[3])
  })
})

observeEvent(input$integrate, {
  withBusyIndicatorServer("integrate", {
    ig$result <- o2m(ig$upload_X, ig$upload_Y, ig$number[1], ig$number[2], ig$number[3])
  })
})

observeEvent(ig$cvadjr, {
  output$cvadjr_result <- renderDataTable({
    validate(need(ig$cvadjr, ""))
    DT::datatable(ig$cvadjr,
      escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
      options = list(deferRender = T, searchHighlight = T, scrollX = T)
    )
  })
},
ignoreNULL = FALSE
)

observeEvent(ig$crossval, {
  output$crossval_result <- renderDataTable({
    validate(need(ig$crossval, ""))
    new_row <- gsub("a", "N", rownames(as.data.frame(res$Sorted)))
    new_col <- gsub("a", "N", colnames(as.data.frame(res$Sorted)))
    DT::datatable(as.data.frame(ig$crossval$Sorted),
      escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
      options = list(deferRender = T, searchHighlight = T, scrollX = T),
      rownames = new_row, colnames = new_col
    )
  })
},
ignoreNULL = FALSE
)
