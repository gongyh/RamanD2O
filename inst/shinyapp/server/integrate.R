output$hs_select_for_integrate <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("average" %in% hs_all) {
    selected <- "average"
  } else if ("cdr" %in% hs_all) {
    selected <- "cdr"
  } else if ("snr" %in% hs_all) {
    selected <- "snr"
  }
  selectInput("hs_selector_for_integrate", "Ramanome dataset", choices = hs_all, selected = selected)
})

observeEvent(input$hs_selector_for_integrate, {
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
}, ignoreNULL = FALSE)

# upload X/Y dataset
observeEvent(input$confirm_X, {
  hs_cur <- NULL
  if (!is.null(input$hs_selector_for_integrate)) {
    hs_cur <- hs$val[[input$hs_selector_for_integrate]]
    ig$upload_X <- hs_cur$spc
    ig$upload_X <- scale(ig$upload_X, scale=F)
  } else {
    toastr_error("No data selected!", position = "top-center")
  }
})

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
    ig$cvadjr <- crossval_o2m_adjR2(ig$upload_X, ig$upload_Y, isolate(input$pars_N_min1):isolate(input$pars_N_max1),
      isolate(input$pars_Nx_min1):isolate(input$pars_Nx_max1), isolate(input$pars_Ny_min1):isolate(input$pars_Ny_max1), nr_folds=2, nr_cores=1)
  })
})

observeEvent(input$crossval,{
  withBusyIndicatorServer("crossval",{
    ig$crossval <- crossval_o2m(ig$upload_X, ig$upload_Y, isolate(input$pars_N_min2):isolate(input$pars_N_max2),
      isolate(input$pars_Nx_min2):isolate(input$pars_Nx_max2), isolate(input$pars_Ny_min2):isolate(input$pars_Ny_max2), nr_folds=isolate(input$pars_fold), nr_cores=5)
    ig$crossval_df <- data.frame()
    df <- lapply(1:(isolate(input$pars_N_max2) - isolate(input$pars_N_min2) + 1), function(i) {
      index <- which(ig$crossval$Original == min(ig$crossval$Original[,,i], na.rm = T), arr.ind = TRUE)
      Nx <- dimnames(ig$crossval$Original)[[1]][index[1]]
      Ny <- dimnames(ig$crossval$Original)[[2]][index[2]]
      N <- dimnames(ig$crossval$Original)[[3]][index[3]]
      Nx <- as.numeric(str_extract(Nx, "(?<=ax=)\\d+"))
      Ny <- as.numeric(str_extract(Ny, "(?<=ay=)\\d+"))
      N <- as.numeric(str_extract(N, "(?<=a=)\\d+"))
      df_tmp <- data.frame(MSE = ifelse(is.null(rownames(index)), "NA", ig$crossval$Original[index]), N = N, Nx = Nx, Ny = Ny)
      ig$crossval_df <- rbind(ig$crossval_df, df_tmp)
    })
  })
})

observeEvent(input$integrate, {
  withBusyIndicatorServer("integrate", {
    ig$result <- o2m(ig$upload_X, ig$upload_Y, isolate(input$pars_N), isolate(input$pars_Nx), isolate(input$pars_Ny))
  })
})

observeEvent(ig$cvadjr, {
  output$cvadjr_result <- renderDataTable({
    validate(need(ig$cvadjr, ""))
    DT::datatable(ig$cvadjr,
      escape = FALSE, selection = "single", extensions = c("Buttons", "Responsive"),
      options = list(
        dom = 'Bfrtip',
        deferRender = T, searchHighlight = T, scrollX = T,
        buttons = list(
          list(extend = "csv", filename = "CVadjr_result", text = "Download CSV")
        )
      )
    )
  })
},
ignoreNULL = FALSE)

observeEvent(ig$crossval, {
  output$crossval_result <- renderDataTable({
    validate(need(ig$crossval, ""))
    DT::datatable(as.data.frame(ig$crossval_df),
      escape = FALSE, selection = "single", extensions = c("Buttons", "Responsive"),
      options = list(
        dom = 'Bfrtip',
        deferRender = T, searchHighlight = T, scrollX = T,
        buttons = list(
          list(extend = "csv", filename = "Crossval_result", text = "Download CSV")
        )
      )
    )
  })
},
ignoreNULL = FALSE)

observeEvent(ig$result, {
  output$Xjoint <- renderPlot({
    validate(need(ig$result, ""))
    px <- list()
    px <- lapply(1:isolate(input$pars_N), function(i) {
      plot(ig$result, loading_name='Xjoint', i=i, j=NULL, col='black')
    })
    ggarrange(plotlist = px, ncol=1)
  })
  output$Yjoint <- renderPlot({
    validate(need(ig$result, ""))
    py <- list()
    py <- lapply(1:isolate(input$pars_N), function(i) {
      plot(ig$result, loading_name='Yjoint', i=i, j=NULL, col='black')
    })
    ggarrange(plotlist = py, ncol=1)
  })
  vip <- O2PLSvip(ig$upload_X, ig$upload_Y, ig$result)
  vip_x <- which(vip$x$predVIPxy>1)
  vip_y <- which(vip$y$predVIPyx>1)
},
ignoreNULL = FALSE)

# Download result
output$ig_result3 <- downloadHandler(
  filename = paste0("ig-result3-", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"),
  content = function(file) {
    if (is.null(ig$result)) {
      shinyalert("Oops!", "No result yet.", type = "error")
      return()
    } else {
      pdf(file)
      px <- list()
      px <- lapply(1:isolate(input$pars_N), function(i) {
        plot(ig$result, loading_name='Xjoint', i=i, j=NULL, col='black')
      })
      print(px)
      pxs <- ggarrange(plotlist = px, ncol=1)
      print(pxs)
      dev.off()
    }
  }
)

output$ig_result4 <- downloadHandler(
  filename = paste0("ig-result4-", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"),
  content = function(file) {
    if (is.null(ig$result)) {
      shinyalert("Oops!", "No result yet.", type = "error")
      return()
    } else {
      pdf(file)
      py <- list()
      py <- lapply(1:isolate(input$pars_N), function(i) {
        plot(ig$result, loading_name='Yjoint', i=i, j=NULL, col='black')
      })
      print(py)
      pys <- ggarrange(plotlist = py, ncol=1)
      print(pys)
      dev.off()
    }
  }
)
