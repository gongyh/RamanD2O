output$hs_select_for_smooth <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("filtered" %in% hs_all) {
    selected <- "filtered"
  } else if ("sampled" %in% hs_all) {
    selected <- "sampled"
  }
  selectInput("hs_selector_for_smooth", "Choose target", choices = hs_all, selected = selected)
})

# smooth scrs on click of button
observeEvent(input$smooth, {
  withBusyIndicatorServer("smooth", {
    if (input$hs_selector_for_smooth == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[input$hs_selector_for_smooth]]
      wavelength <- wl(hs_cur)
      if (input$interp) {
        minWl <- round(min(wavelength)) + 1
        maxWl <- round(max(wavelength)) - 1
        wavelength <- minWl:maxWl
      }
      hs_sm <- spc.loess(hs_cur, wavelength, normalize = F)
      colnames(hs_sm$spc) <- hs_sm@wavelength
      hs$val[["smoothed"]] <- hs_sm
    }
  })
})

observeEvent(hs$val[["smoothed"]],
  {
    hs_sm <- hs$val[["smoothed"]]
    output$smoothed_table <- renderDataTable({
      DT::datatable(if (is.null(hs_sm)) NULL else hs_sm@data %>% select(!matches("spc")),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$smoothed_table_rows_selected,
  {
    output$after_smooth_plot <- renderPlotly({
      validate(need(input$smoothed_table_rows_selected, ""))
      index <- input$smoothed_table_rows_selected
      item <- hs$val[["smoothed"]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
