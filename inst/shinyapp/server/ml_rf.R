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
      eval <- isolate(input$hs_selector_for_ml_eval)
      if (!is.null(eval) && (eval != "_")) {
        xtest <- hs$val[[isolate(input$hs_selector_for_ml_eval)]]
        ytest <- xtest@data[isolate(input$ml_select_label)]
        ytrain <- hs_train@data[isolate(input$ml_select_label)]
      }
      rf <- randomForest(
        x = hs_train$spc, y = factor(ytrain[, 1]), xtest = xtest$spc, ytest = as.factor(ytest[, 1]),
        ntree = isolate(input$rf_ntree), replace = isolate(input$rf_replace), norm.votes = TRUE
      )
      ml$results <- rf
      output$rf_test_predicted_plot <- renderDataTable({
        validate(need(rf, ""))
        validate(need(rf$test, ""))
        df <- data.frame(real = ytest[, 1], predicted = rf$test$predicted)
        rownames(df) <- names(rf$test$predicted)
        df <- cbind(df, rf$test$votes)
        DT::datatable(df,
          escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
          options = list(deferRender = T, searchHighlight = T, scrollX = T)
        )
      })
    })
  },
  ignoreNULL = T
)

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
