output$hs_select_for_cdr <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("normalized" %in% hs_all) {
    selected <- "normalized"
  } else if ("snr" %in% hs_all) {
    selected <- "snr"
  }
  selectInput("hs_selector_for_cdr", "Choose target", choices = hs_all, selected = selected)
})

# calc CDR for scrs on click of button
observeEvent(input$cdr, {
  withBusyIndicatorServer("cdr", {
    if (input$hs_selector_for_cdr == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[input$hs_selector_for_cdr]]
      wave_nums <- wl(hs_cur)
      CDR_All <- NULL
      hs_cdr <- hs_cur
      spc <- hs_cur$spc
      # cdr
      for (i in 1:nrow(spc)) {
        CD_start <- which.min(abs(wave_nums - 2050))
        CD_end <- which.min(abs(wave_nums - 2300))
        CH_start <- which.min(abs(wave_nums - 3050))
        CH_end <- which.min(abs(wave_nums - 2800))
        CD <- spc[i, CD_start:CD_end]
        CH <- spc[i, CH_start:CH_end]
        CDR <- sum(CD) / (sum(CD) + sum(CH))
        CDR_All <- rbind(CDR_All, CDR)
      }
      hs_cdr$CDR <- round2(CDR_All)
      hs$val[["cdr"]] <- hs_cdr
    }
  })
})

observeEvent(hs$val[["cdr"]],
  {
    hs_cdr <- hs$val[["cdr"]]
    output$cdr_table <- renderDataTable({
      DT::datatable(if (is.null(hs_cdr)) NULL else hs_cdr@data %>% select(!matches("spc")),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
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
      item <- hs$val[["cdr"]][index][, , c(2050 ~ 3050)]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
