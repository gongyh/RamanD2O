output$hs_select_for_ml_train <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("train" %in% hs_all) {
    selected <- "train"
  } else if ("cdr" %in% hs_all) {
    selected <- "cdr"
  } else if ("snr" %in% hs_all) {
    selected <- "snr"
  }
  selectInput("hs_selector_for_ml_train", "Training dataset", choices = hs_all, selected = selected)
})

output$hs_select_for_ml_eval <- renderUI({
  hs_all <- c("_", names(hs$val))
  selected <- NULL
  if ("eval" %in% hs_all) {
    selected <- "eval"
  } else {
    selected <- "_"
  }
  selectInput("hs_selector_for_ml_eval", "Evaluation dataset", choices = hs_all, selected = selected)
})

output$hs_select_for_ml_test <- renderUI({
  hs_all <- c("_", names(hs$val))
  selected <- NULL
  if ("test" %in% hs_all) {
    selected <- "test"
  } else {
    selected <- "_"
  }
  selectInput("hs_selector_for_ml_test", "Test dataset", choices = hs_all, selected = selected)
})

observeEvent(input$hs_selector_for_ml_train,
  {
    hs_cur <- NULL
    if (!is.null(input$hs_selector_for_ml_train)) {
      hs_cur <- hs$val[[input$hs_selector_for_ml_train]]
    }
    output$hs_select_for_ml_label <- renderUI({
      metacols <- c("")
      if (!is.null(hs_cur)) {
        metacols <- colnames(hs_cur)
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("ml_select_label", "Label", choices = metacols, selected = F)
    })
  },
  ignoreNULL = FALSE
)


# prepare training datasets for scrs on click of button
observeEvent(input$train,
  {
    withBusyIndicatorServer("train", {
      validate(need(isolate(input$hs_selector_for_ml_train), ""))
      validate(need(isolate(input$ml_select_label), ""))
      hs_train <- hs$val[[isolate(input$hs_selector_for_ml_train)]]
      xtest <- NULL
      ytest <- NULL
      ytest_factor <- NULL
      eval <- isolate(input$hs_selector_for_ml_eval)
      if (!is.null(eval) && (eval != "_")) {
        xtest <- hs$val[[isolate(input$hs_selector_for_ml_eval)]]
        ytest <- xtest@data[isolate(input$ml_select_label)]
        ytrain <- hs_train@data[isolate(input$ml_select_label)]
        ytrain_factor <- factor(ytrain[, 1])
        ytest_factor <- factor(as.character(ytest[, 1]),levels=levels(ytrain_factor))
      }
      ytrain <- hs_train@data[isolate(input$ml_select_label)]
      ytrain_factor <- factor(ytrain[, 1])
      rf <- randomForest(
        x = hs_train$spc, y = ytrain_factor, xtest = xtest$spc, ytest = ytest_factor,
        ntree = isolate(input$rf_ntree), replace = isolate(input$rf_replace), norm.votes = TRUE, keep.forest = TRUE
      )
      ml$results <- rf
      output$rf_test_predicted_plot <- renderDataTable({
        validate(need(rf, ""))
        validate(need(rf$test, ""))
        df <- data.frame(real = ytest[, 1], predicted = rf$test$predicted)
        rownames(df) <- names(rf$test$predicted)
        df <- cbind(df, rf$test$votes)
        result$predict <- df
        DT::datatable(df,
          escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
          options = list(deferRender = T, searchHighlight = T, scrollX = T)
        )
      })
    })
  },
  ignoreNULL = T
)

observeEvent(input$eval,
  {
    withBusyIndicatorServer("eval", {
      validate(need(isolate(input$hs_selector_for_ml_eval), ""))
      if (is.null(ml$results)) {
        toastr_error("Please train the model first!", position = "top-center")
        return()
      }
      eval <- isolate(input$hs_selector_for_ml_eval)
      if (!is.null(eval) && (eval != "_")) {
        hs_eval <- hs$val[[isolate(input$hs_selector_for_ml_eval)]]
        if (identical(names(ml$results$importance[,]), colnames(hs_eval$spc))) {
          result_predict <- predict(ml$results, hs_eval$spc)
          cur_levels <- levels(hs_eval@data[isolate(input$ml_select_label)][,1])
          factor_predict <- factor(result_predict, levels = cur_levels)
          result_confusion <- table(factor_predict, hs_eval@data[isolate(input$ml_select_label)][,1])
          result_confusion <- as.data.frame.matrix(result_confusion)
        } else {
          toastr_error("Inconsistent spectra of the data set and model.", position = "top-center")
          return()
        }
      } else {
        toastr_error("Eval set not selected!", position = "top-center")
        return()
      }
      output$rf_confusion_eval_plot <- renderDataTable({
        DT::datatable(result_confusion,
          escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
          options = list(deferRender = T, searchHighlight = T, scrollX = T)
        )
      })
      output$rf_test_predicted_plot <- renderDataTable({
        df <- data.frame(real = hs_eval@data[isolate(input$ml_select_label)][, 1], predicted = result_predict)
        rownames(df) <- names(result_predict)
        result$predict <- df
        DT::datatable(df,
          escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
          options = list(deferRender = T, searchHighlight = T, scrollX = T)
        )
      })
    })
  },
  ignoreNULL = T
)

observeEvent(input$test,
  {
    withBusyIndicatorServer("test", {
      validate(need(isolate(input$hs_selector_for_ml_test), ""))
      if (is.null(ml$results)) {
        toastr_error("Please train the model first!", position = "top-center")
        return()
      }
      test <- isolate(input$hs_selector_for_ml_test)
      if (!is.null(test) && (test != "_")) {
        hs_test <- hs$val[[isolate(input$hs_selector_for_ml_test)]]
        if (identical(names(ml$results$importance[,]), colnames(hs_test$spc))) {
          result_predict <- predict(ml$results, hs_test$spc)
          cur_levels <- levels(hs_test@data[isolate(input$ml_select_label)][,1])
          factor_predict <- factor(result_predict, levels = cur_levels)
          result_confusion <- table(factor_predict, hs_test@data[isolate(input$ml_select_label)][,1])
          result_confusion <- as.data.frame.matrix(result_confusion)
        } else {
          toastr_error("Inconsistent spectra of the data set and model.", position = "top-center")
          return()
        }
      } else {
        toastr_error("Test set not selected!", position = "top-center")
        return()
      }
      output$rf_confusion_eval_plot <- renderDataTable({
        DT::datatable(result_confusion,
          escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
          options = list(deferRender = T, searchHighlight = T, scrollX = T)
        )
      })
      output$rf_test_predicted_plot <- renderDataTable({
        df <- data.frame(predicted = result_predict)
        rownames(df) <- names(result_predict)
        result$predict <- df
        DT::datatable(df,
          escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
          options = list(deferRender = T, searchHighlight = T, scrollX = T)
        )
      })
    })
  },
  ignoreNULL = T
)

# training model plot
observeEvent(ml$results,
  {
    rf <- ml$results
    output$rf_mse_plot <- renderPlot({
      validate(need(rf, ""))
      plot(rf)
    })
    output$rf_importance_plot <- renderPlot({
      validate(need(rf, ""))
      plot(
        x = rownames(rf$importance), y = rf$importance, type = "h",
        xlab = "Wavelength", ylab = "Importance"
      )
    })
    output$rf_confusion_oob_plot <- renderDataTable({
      validate(need(rf, ""))
      DT::datatable(rf$confusion,
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(deferRender = T, searchHighlight = T, scrollX = T)
      ) %>% formatPercentage(c("class.error"), 4)
    })
    output$rf_confusion_eval_plot <- renderDataTable({
      validate(need(rf, ""))
      validate(need(rf$test, ""))
      DT::datatable(rf$test$confusion,
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(deferRender = T, searchHighlight = T, scrollX = T)
      ) %>% formatPercentage(c("class.error"), 4)
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$rf_test_predicted_plot_rows_selected,
  {
    output$selected_predicted_plot <- renderPlotly({
      validate(need(input$rf_test_predicted_plot_rows_selected, ""))
      index <- input$rf_test_predicted_plot_rows_selected
      item <- hs$val[[isolate(input$hs_selector_for_ml_eval)]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)

# upload and download training model
observeEvent(input$upload_model, {
  withBusyIndicatorServer("upload_model", {
    if (!is.null(isolate(input$upload_model_file$datapath))) {
      ml$results <- readRDS(isolate(input$upload_model_file$datapath))
      if (is.null(ml$results$call) | is.null(ml$results$predicted) | is.null(ml$results$importance)) {
        toastr_error("Please load valid model!", position = "top-center")
        return()
      }
    } else {
      toastr_error("No file selected!", position = "top-center")
    }
  })
})

output$download_model <- downloadHandler(
  filename = paste0("model-", format(Sys.time(), "%Y%m%d%H%M%S"), ".rds"), 
  content = function(file) {
    if (is.null(ml$results)) {
      shinyalert("Oops!", "Please train the model first.", type = "error")
      return()
    } else {saveRDS(ml$results, file)}
  }
)

output$download_result <- downloadHandler(
  filename = paste0("predict-", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv"), 
  content = function(file) {
    if (is.null(result$predict)) {
      shinyalert("Oops!", "No results yet.", type = "error")
      return()
    } else {write.csv(result$predict, file)}
  }
)