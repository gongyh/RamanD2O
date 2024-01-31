output$hs_select_for_ml_explore <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("train" %in% hs_all) {
    selected <- "train"
  } else if ("cdr" %in% hs_all) {
    selected <- "cdr"
  } else if ("snr" %in% hs_all) {
    selected <- "snr"
  } else if ("smoothed" %in% hs_all) {
    selected <- "smoothed"
  }
  selectInput("hs_selector_for_ml_explore","Choose target",
    choices = hs_all, selected = selected)
})

observeEvent(input$hs_selector_for_ml_explore,
  {
    hs_cur <- NULL
    if (!is.null(input$hs_selector_for_ml_explore)) {
      hs_cur <- hs$val[[input$hs_selector_for_ml_explore]]
    }
    output$tsneColBy <- renderUI({
      metacols <- c("")
      if (!is.null(hs_cur)) {
        metacols <- colnames(hs_cur)
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("select_tsneColBy", "Color by",
        choices = metacols, selected = FALSE)
    })
  },
  ignoreNULL = FALSE
)

# prepare training datasets for scrs on click of button
observeEvent(input$tsne,
  {
    withBusyIndicatorServer("tsne", {
      output$after_tsne_plot <- renderPlotly({
        validate(need(isolate(input$hs_selector_for_ml_explore), ""))
        validate(need(isolate(input$select_tsneColBy), ""))
        hs_cur <- hs$val[[isolate(input$hs_selector_for_ml_explore)]]
        hs_cur_meta <- hs_cur@data
        hs_cur_meta$spc <- NULL
        tsne_out <- Rtsne(
          hs_cur$spc, perplexity = isolate(input$perplexity),
          max_iter = isolate(input$max_iter)
        )
        df <- data.frame(dim1 = tsne_out$Y[, 1], dim2 = tsne_out$Y[, 2])
        df <- cbind(df, hs_cur_meta)
        df$text <- rownames(hs_cur$spc)
        p <- ggplot(df, aes_string(x = "dim1", y = "dim2"))
        suppressWarnings(
          p <- p +
            geom_point(
              aes_string(color = isolate(input$select_tsneColBy), text = "text")
            )
        )
        ggplotly(p)
      })
    })
  },
  ignoreNULL = FALSE
)
