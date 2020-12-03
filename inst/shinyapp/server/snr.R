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
      SNR_All <- NULL
      hs_snr <- hs_cur
      spc <- hs_cur$spc
      # snr
      if (input$select_snr == "new") {
        for (i in 1:nrow(spc)) {
          Baseline_start <- which.min(abs(wavelength - 1730)) # 1760
          Baseline_end <- which.min(abs(wavelength - 1800)) # 1960
          Baseline <- spc[i, Baseline_start:Baseline_end]
          marker <- max(spc[i, which.min(abs(wavelength - 3050)):which.min(abs(wavelength - 2800))]) # C-H peak
          SNR <- (marker - sum(Baseline) / length(Baseline)) / sqrt(marker)
          SNR_All <- rbind(SNR_All, SNR)
        }
        hs_snr$SNR <- round2(SNR_All)
      } else if (input$select_snr == "old") {
        for (i in 1:nrow(spc)) {
          Baseline_start <- which.min(abs(wavelength - 1760))
          Baseline_end <- which.min(abs(wavelength - 1960))
          Baseline <- spc[i, Baseline_start:Baseline_end]
          marker <- max(spc[1, which.min(abs(wavelength - 1400)):which.min(abs(wavelength - 1460))])
          SNR <- (marker - sum(Baseline) / length(Baseline)) / sd(Baseline)
          SNR_All <- rbind(SNR_All, SNR)
        }
        hs_snr$SNR <- round2(SNR_All)
      } else {
        shinyalert("Oops!", "SNR method not implemented yet.", type = "error")
        return()
      }
      # handle filter
      if (input$filter_by_snr) {
        snr_cutoff <- input$snr_cutoff
        hs_snr <- hs_snr[hs_snr$SNR >= snr_cutoff]
      }
      hs$val[["snr"]] <- hs_snr
    }
  })
})

observeEvent(hs$val[["snr"]],
  {
    hs_snr <- hs$val[["snr"]]
    output$snr_table <- renderDataTable({
      DT::datatable(if (is.null(hs_snr)) NULL else hs_snr@data %>% select(!matches("spc")),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$snr_table_rows_selected,
  {
    output$after_snr_plot <- renderPlotly({
      validate(need(input$snr_table_rows_selected, ""))
      index <- input$snr_table_rows_selected
      item <- hs$val[["snr"]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
