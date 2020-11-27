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
      output$normalized_table <- renderDataTable({
        df <- as.data.frame(hs_nl$spc[, 1:6]) %>% mutate_if(is.numeric, round2)
        colnames(df) <- hs_nl@wavelength[1:6]
        rownames(df) <- rownames(hs_nl$spc)
        DT::datatable(df,
          escape = FALSE, selection = "single",
          options = list(searchHighlight = TRUE, scrollX = TRUE)
        )
      })
    }
  })
})

observeEvent(input$normalized_table_rows_selected, {
  index <- input$normalized_table_rows_selected
  item <- hs$val[["normalized"]][index]
  output$after_normalize_plot <- renderPlotly({
    p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
    ggplotly(p) %>% config(mathjax = 'cdn')
  })
})
