output$hs_select_for_trim <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("sampled" %in% hs_all) {
    selected <- "sampled"
  }
  selectInput("hs_selector_for_trim", "Choose target",
              choices = hs_all, selected = selected)
})

# convert trim_range and trim_min/trim_max
observeEvent(c(input$trim_min, input$trim_max), {
  updateSliderInput(session, "trim_range",
                    value = c(input$trim_min, input$trim_max))
})
observeEvent(c(input$trim_range[1], input$trim_range[2]), {
  updateNumericInput(session, "trim_min", value = input$trim_range[1])
  updateNumericInput(session, "trim_max", value = input$trim_range[2])
})

# trim scrs on click of button
observeEvent(input$trim, {
  withBusyIndicatorServer("trim", {
    if (isolate(input$hs_selector_for_trim) == "") {
      shinyalert("Oops!",
                 "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[isolate(input$hs_selector_for_trim)]]
      minR <- isolate(input$trim_range)[1]
      maxR <- isolate(input$trim_range)[2]
      hs_tm <- hs_cur[, , minR ~ maxR]
      hs$val[["trimmed"]] <- hs_tm
    }
  })
})

observeEvent(hs$val[["trimmed"]],
  {
    hs_tm <- hs$val[["trimmed"]]
    output$after_trim <- renderDataTable({
      DT::datatable(
        if (is.null(hs_tm)) NULL else hs_tm@data %>%
          dplyr::select(!matches("spc")),
        escape = FALSE, selection = "single",
        extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$after_trim_rows_selected,
  {
    output$after_trim_plot <- renderPlotly({
      validate(need(input$after_trim_rows_selected, ""))
      index <- input$after_trim_rows_selected
      item <- hs$val[["trimmed"]][index]
      p <- qplotspc(item) +
        xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
