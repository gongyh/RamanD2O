output$hs_select_for_snr <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("normalized" %in% hs_all) {
    selected <- "normalized"
  } else if ("baselined" %in% hs_all) {
    selected <- "baselined"
  }
  selectInput("hs_selector_for_snr", "Choose target", choices = hs_all, selected = selected)
})

# calc SNR for scrs on click of button
observeEvent(input$snr, {
  withBusyIndicatorServer("snr", {
    if (input$hs_selector_for_snr == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[input$hs_selector_for_snr]]
      wavelength <- wl(hs_cur)
      # snr
      if (input$select_baseline == "new") {
        b_als <- baseline(hs_cur$spc, method = "als")
        data <- hs_cur@data
        data$spc <- NULL
        hs_bl <- new("hyperSpec",
                     data = data,
                     spc = getCorrected(b_als), wavelength = wavelength
        )
      } else if (input$select_baseline == "old") {
        order <- input$polyfit_order
        hs_bl <- hs_cur - spc.fit.poly.below(hs_cur, poly.order = 7)
        hs_bl$spc <- unAsIs(hs_bl$spc)
        dimnames(hs_bl$spc) <- dimnames(hs_cur$spc)
      } else {
        shinyalert("Oops!", "SNR method not implemented yet.", type = "error")
        return()
      }
      # handle filter
      if (input$filter_by_snr) {
        hs_bl_spc <- hs_bl$spc
        hs_bl_spc[hs_bl_spc < 0] <- 0
        hs_bl$spc <- hs_bl_spc
      } else {
        # treat as keep
      }
      hs$val[["snr"]] <- hs_bl
      output$snr_table <- renderDataTable({
        df <- as.data.frame(hs_bl$spc[, 1:6]) %>% mutate_if(is.numeric, round2)
        rownames(df) <- rownames(hs_bl$spc)
        DT::datatable(df,
                      escape = FALSE, selection = "single",
                      options = list(searchHighlight = TRUE, scrollX = TRUE)
        )
      })
    }
  })
})

observeEvent(input$snr_table_rows_selected, {
  index <- input$snr_table_rows_selected
  item <- hs$val[["snr"]][index]
  output$after_snr_plot <- renderPlotly({
    p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
    ggplotly(p) %>% config(mathjax = 'cdn')
  })
})
