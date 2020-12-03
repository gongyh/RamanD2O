output$hs_select_for_normalize <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("baselined" %in% hs_all) {
    selected <- "baselined"
  }
  selectInput("hs_selector_for_normalize", "Choose target", choices = hs_all, selected = selected)
})

# smooth scrs on click of button
observeEvent(input$normalize, {
  withBusyIndicatorServer("normalize", {
    if (input$hs_selector_for_normalize == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[input$hs_selector_for_smooth]]
      wavelength <- wl(hs_cur)
      if (input$fingerprint) {
        hs_nl <- hs_cur / rowMeans(hs_cur[, , c(500 ~ 2000)])
      } else {
        hs_nl <- hs_cur / rowMeans(hs_cur)
      }
      hs$val[["normalized"]] <- hs_nl
    }
  })
})


observeEvent(hs$val[["normalized"]],
  {
    hs_nl <- hs$val[["normalized"]]
    output$normalized_table <- renderDataTable({
      DT::datatable(if (is.null(hs_nl)) NULL else round(hs_nl$spc, 2),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)


observeEvent(input$normalized_table_rows_selected,
  {
    output$after_normalize_plot <- renderPlotly({
      validate(need(input$normalized_table_rows_selected, ""))
      index <- input$normalized_table_rows_selected
      item <- hs$val[["normalized"]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
