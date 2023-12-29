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
    ig$vip <- O2PLSvip(ig$upload_X, ig$upload_Y, ig$result)
  })
})

observeEvent(input$integrate2d, {
  withBusyIndicatorServer("integrate2d", {
    ig$upload_X2 <- matrix(nrow=nrow(ig$upload_X), ncol=ncol(ig$upload_X)*(ncol(ig$upload_X)+1))
    ig$upload_X2[,1:ncol(ig$upload_X)] <- as.matrix(ig$upload_X)
    rownames(ig$upload_X2) <- rownames(ig$upload_X)
    # library para
    cores_to_use <- isolate(input$cpu_cores)
    if (cores_to_use > (detectCores(logical = FALSE) - 1)) {cores_to_use <- detectCores(logical = FALSE) - 1}
    cl <- makeCluster(cores_to_use)
    registerDoParallel(cl)
    upload_X <- isolate(ig$upload_X)
    X2 <- foreach(i = 1:ncol(upload_X), .combine = 'cbind', .export = "upload_X") %dopar% {
      Ri <- (upload_X+1)/(upload_X[,i]+1)
      Ri <- as.matrix(Ri)
      colnames(Ri) <- paste0(colnames(upload_X), "_", colnames(upload_X)[i])
      Ri
    }
    stopCluster(cl)
    ig$upload_X2[, (ncol(ig$upload_X) + 1) : (ncol(ig$upload_X) + ncol(X2))] <- X2
    colnames(ig$upload_X2) <- c(colnames(ig$upload_X), colnames(X2))
    # O2PLS
    ig$result2 <- o2m(ig$upload_X2, ig$upload_Y, isolate(input$pars_N), isolate(input$pars_Nx), isolate(input$pars_Ny))
    ig$vip2 <- O2PLSvip(ig$upload_X2, ig$upload_Y, ig$result2)
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
          list(extend = "csv", filename = "CVadjr_result", text = "Download CSV",
            exportOptions = list(modifier = list(page = 'all', search = 'none')))
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
          list(extend = "csv", filename = "Crossval_result", text = "Download CSV",
            exportOptions = list(modifier = list(page = 'all', search = 'none')))
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
  # datatable x_vip and y_vip
  output$x_vip <- renderDataTable({
    validate(need(ig$vip, ""))
    x_vip_index <- which(ig$vip$x$predVIPxy>1)
    x_vip_data <- ig$vip$x[x_vip_index,]
    DT::datatable(as.data.frame(x_vip_data),
      escape = FALSE, selection = "single", extensions = c("Buttons", "Responsive"),
      options = list(
        dom = 'Bfrtip',
        deferRender = T, searchHighlight = T, scrollX = T,
        buttons = list(
          list(extend = "csv", filename = "x_vip_data", text = "Download CSV",
            exportOptions = list(modifier = list(page = 'all', search = 'none')))
        )
      )
    )
  })
  output$y_vip <- renderDataTable({
    validate(need(ig$vip, ""))
    y_vip_index <- which(ig$vip$y$predVIPyx>1)
    y_vip_data <- ig$vip$y[y_vip_index,]
    DT::datatable(as.data.frame(y_vip_data),
      escape = FALSE, selection = "single", extensions = c("Buttons", "Responsive"),
      options = list(
        dom = 'Bfrtip',
        deferRender = T, searchHighlight = T, scrollX = T,
        buttons = list(
          list(extend = "csv", filename = "y_vip_data", text = "Download CSV",
            exportOptions = list(modifier = list(page = 'all', search = 'none'))
          )
        )
      )
    )
  })
  ig$cur_result <- "result"
},
ignoreNULL = FALSE)

observeEvent(ig$result2, {
  output$Xjoint <- renderPlot({
    validate(need(ig$result2, ""))
    px <- list()
    px <- lapply(1:isolate(input$pars_N), function(i) {
      plot(ig$result2, loading_name='Xjoint', i=i, j=NULL, col='black')
    })
    ggarrange(plotlist = px, ncol=1)
  })
  output$Yjoint <- renderPlot({
    validate(need(ig$result2, ""))
    py <- list()
    py <- lapply(1:isolate(input$pars_N), function(i) {
      plot(ig$result2, loading_name='Yjoint', i=i, j=NULL, col='black')
    })
    ggarrange(plotlist = py, ncol=1)
  })
  # datatable x_vip and y_vip
  output$x_vip <- renderDataTable({
    validate(need(ig$vip2, ""))
    x_vip_index <- which(ig$vip2$x$predVIPxy>55)
    x_vip_data <- ig$vip2$x[x_vip_index,]
    DT::datatable(as.data.frame(x_vip_data),
      escape = FALSE, selection = "single", extensions = c("Buttons", "Responsive"),
      options = list(
        dom = 'Bfrtip',
        deferRender = T, searchHighlight = T, scrollX = T,
         buttons = list(
           list(extend = "csv", filename = "x_vip_data", text = "Download CSV",
             exportOptions = list(modifier = list(page = 'all', search = 'none')))
         )
      )
    )
  })
  output$y_vip <- renderDataTable({
    validate(need(ig$vip2, ""))
    y_vip_index <- which(ig$vip2$y$predVIPyx>1)
    y_vip_data <- ig$vip2$y[y_vip_index,]
    DT::datatable(as.data.frame(y_vip_data),
      escape = FALSE, selection = "single", extensions = c("Buttons", "Responsive"),
      options = list(
        dom = 'Bfrtip',
        deferRender = T, searchHighlight = T, scrollX = T,
        buttons = list(
          list(extend = "csv", filename = "y_vip_data", text = "Download CSV",
            exportOptions = list(modifier = list(page = 'all', search = 'none')))
        )
      )
    )
  })
  ig$cur_result <- "result2"
},
ignoreNULL = FALSE)

# Download result
output$ig_result3 <- downloadHandler(
  filename = paste0("ig-result3-", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"),
  content = function(file) {
    if (is.null(ig[[ig$cur_result]])) {
      shinyalert("Oops!", "No result yet.", type = "error")
      return()
    } else {
      pdf(file)
      px <- list()
      px <- lapply(1:isolate(input$pars_N), function(i) {
        plot(ig[[ig$cur_result]], loading_name='Xjoint', i=i, j=NULL, col='black')
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
    if (is.null(ig[[ig$cur_result]])) {
      shinyalert("Oops!", "No result yet.", type = "error")
      return()
    } else {
      pdf(file)
      py <- list()
      py <- lapply(1:isolate(input$pars_N), function(i) {
        plot(ig[[ig$cur_result]], loading_name='Yjoint', i=i, j=NULL, col='black')
      })
      print(py)
      pys <- ggarrange(plotlist = py, ncol=1)
      print(pys)
      dev.off()
    }
  }
)
