output$hs_select_for_cdr <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("snr" %in% hs_all) {
    selected <- "snr"
  } else if ("normalized" %in% hs_all) {
    selected <- "normalized"
  }
  selectInput("hs_selector_for_cdr", "Choose target",
              choices = hs_all, selected = selected)
})

# calc CDR for scrs on click of button
observeEvent(input$cdr, {
  withBusyIndicatorServer("cdr", {
    if (isolate(input$hs_selector_for_cdr) == "") {
      shinyalert("Oops!",
                 "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[isolate(input$hs_selector_for_cdr)]]
      wavelength <- wl(hs_cur)
      CDR_All <- NULL
      hs_cdr <- hs_cur
      spc <- hs_cur$spc
      # cdr
      for (i in seq_len(nrow(spc))) {
        Baseline_start <- which.min(abs(wavelength - 1760))
        Baseline_end <- which.min(abs(wavelength - 1960))
        Baseline <- spc[i, Baseline_start:Baseline_end]
        CD_start <- which.min(abs(wavelength - 2050))
        CD_end <- which.min(abs(wavelength - 2300))
        CH_start <- which.min(abs(wavelength - 3050))
        CH_end <- which.min(abs(wavelength - 2800))
        CD <- spc[i, CD_start:CD_end]
        CH <- spc[i, CH_start:CH_end]
        CDR <- (sum(CD) - sum(Baseline) * 1.25) /
          (sum(CD) + sum(CH) - sum(Baseline) * 2.5)
        if (CDR < 0) CDR <- 0
        CDR_All <- rbind(CDR_All, CDR)
      }
      hs_cdr$CDR <- round4(CDR_All)
      hs_cdr$D2O <- ifelse(CDR_All >= isolate(input$cdr_cutoff), 1, 0)
      hs$val[["cdr"]] <- hs_cdr
    }
  })
})

observeEvent(hs$val[["cdr"]],
  {
    hs_cdr <- hs$val[["cdr"]]
    output$cdr_table <- renderDataTable({
      DT::datatable(
        if (is.null(hs_cdr)) NULL else hs_cdr@data %>%
          dplyr::select(!matches("spc")),
        escape = FALSE, selection = "single",
        extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$cdr_table_rows_selected,
  {
    output$after_cdr_plot <- renderPlotly({
      validate(need(input$cdr_table_rows_selected, ""))
      index <- input$cdr_table_rows_selected
      item <- hs$val[["cdr"]][index][, , c(2050 - 250 ~ 3050 + 250)]
      p <- qplotspc(item) +
        xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.") +
        geom_vline(xintercept = c(2050, 2300, 2800, 3050), color = "gray")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
