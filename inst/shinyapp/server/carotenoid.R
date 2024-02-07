output$hs_select_for_carotenoid <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("normalized" %in% hs_all) {
    selected <- "normalized"
  } else if ("baselined" %in% hs_all) {
    selected <- "baselined"
  }
  selectInput("hs_selector_for_carotenoid", "Choose target",
              choices = hs_all, selected = selected)
})

# detect carotenoid scrs on click of button
observeEvent(input$carotenoid, {
  withBusyIndicatorServer("carotenoid", {
    if (isolate(input$hs_selector_for_carotenoid) == "") {
      shinyalert("Oops!",
                 "Please first load your spectra data.", type = "error")
      return()
    } else {
      show_modal_spinner(spin = "flower", color = "red",
                         text = "Processing ....")
      hs_cur <- hs$val[[isolate(input$hs_selector_for_carotenoid)]]
      wavelength <- wl(hs_cur)
      keep <- c()
      for (i in seq_len(nrow(hs_cur))) {
        peaks <- wavelength[findPeaks(hs_cur[i]$spc)]
        if ((length(peaks[(peaks > 992) & (peaks < 1010)]) >= 1) &&
              (length(peaks[(peaks > 1145) & (peaks < 1160)]) >= 1) &&
              # carotenoid peaks
              (length(peaks[(peaks > 1480) & (peaks < 1525)]) >= 1)) {
          keep <- c(keep, FALSE)
        } else {
          keep <- c(keep, TRUE)
        }
      }
      hs_bl <- hs_cur
      if (isolate(input$filter_carotenoid) == TRUE) {
        hs_bl <- hs_cur[keep]
      } else {
        hs_bl$Carotenoid <- !keep
      }
      hs$val[["carotenoid"]] <- hs_bl
      remove_modal_spinner()
    }
  })
})

observeEvent(hs$val[["carotenoid"]],
  {
    hs_bl <- hs$val[["carotenoid"]]
    output$carotenoid_table <- renderDataTable({
      DT::datatable(
        if (is.null(hs_bl)) NULL else hs_bl@data %>%
          dplyr::select(!matches("spc")),
        escape = FALSE, selection = "single",
        extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$carotenoid_table_rows_selected,
  {
    output$after_carotenoid_plot <- renderPlotly({
      validate(need(input$carotenoid_table_rows_selected, ""))
      index <- input$carotenoid_table_rows_selected
      item <- hs$val[["carotenoid"]][index]
      p <- qplotspc(item) +
        xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
