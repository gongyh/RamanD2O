output$hs_select_for_average <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("normalized" %in% hs_all) {
    selected <- "normalized"
  } else if ("baselined" %in% hs_all) {
    selected <- "baselined"
  }
  selectInput("hs_selector_for_average", "Choose target", choices = hs_all, selected = selected)
})

observeEvent(input$hs_selector_for_average,
  {
    hs_cur <- NULL
    if (!is.null(input$hs_selector_for_average)) {
      hs_cur <- hs$val[[input$hs_selector_for_average]]
    }
    output$hs_select_for_average_label <- renderUI({
    metacols <- c("")
    if (!is.null(hs_cur)) {
      metacols <- colnames(hs_cur)
      metacols <- metacols[metacols != "spc"]
    }
    selectInput("average_select_label", "Label", choices = metacols, selected = F)
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$average, {
  withBusyIndicatorServer("average", {
    hs_cur <- hs$val[[isolate(input$hs_selector_for_average)]]
    average_label <- isolate(input$average_select_label)
    hs_avr <- aggregate(hs_cur, hs_cur@data[average_label][,1], FUN = mean)
    groupBy <- hs_avr$.aggregate
    row.names(hs_avr@data) <- groupBy
    hs_avr@data$ID_Cell <- groupBy
    row.names(hs_avr$spc) <- groupBy
    hs$val[["average"]] <- hs_avr
  })
})

observeEvent(hs$val[["average"]],
  {
    hs_average <- hs$val[["average"]]
    output$average_table <- renderDataTable({
      DT::datatable(if (is.null(hs_average)) NULL else hs_average@data %>% dplyr::select(!matches("spc")),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$average_table_rows_selected,
  {
    output$after_average_plot <- renderPlotly({
      validate(need(input$average_table_rows_selected, ""))
      index <- input$average_table_rows_selected
      item <- hs$val[["average"]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
