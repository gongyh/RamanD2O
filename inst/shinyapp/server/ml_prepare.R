output$hs_select_for_ml_prepare <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("cdr" %in% hs_all) {
    selected <- "cdr"
  } else if ("snr" %in% hs_all) {
    selected <- "snr"
  } else if ("smoothed" %in% hs_all) {
    selected <- "smoothed"
  }
  selectInput("hs_selector_for_ml_prepare", "Choose target", choices = hs_all, selected = selected)
})

# convert prepare_trim_range and prepare_trim_min/max
observeEvent(c(input$ptrim_min, input$ptrim_max), {
  updateSliderInput(session, "ptrim_range", value = c(input$ptrim_min, input$ptrim_max))
})
observeEvent(c(input$ptrim_range[1], input$ptrim_range[2]), {
  updateNumericInput(session, "ptrim_min", value = input$ptrim_range[1])
  updateNumericInput(session, "ptrim_max", value = input$ptrim_range[2])
})

# prepare training datasets for scrs on click of button
observeEvent(input$prepare, {
  withBusyIndicatorServer("prepare", {
    if (isolate(input$hs_selector_for_ml_prepare) == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[isolate(input$hs_selector_for_ml_prepare)]]
      if (isolate(input$prepare_trim)) {
        minR <- isolate(input$ptrim_range)[1]
        maxR <- isolate(input$ptrim_range)[2]
        hs_cur <- hs_cur[, , minR ~ maxR]
      }
      # randomly split
      total <- nrow(hs_cur)
      size <- floor(isolate(input$train_pct) / 100.0 * total)
      tindex <- isample(hs_cur)
      index <- tindex[1:max(size, 2)]
      hs$val[["train"]] <- hs_cur[index]
      hs$val[["eval"]] <- hs_cur[-index]
    }
  })
})

observeEvent(hs$val[["train"]],
  {
    hs_train <- hs$val[["train"]]
    output$after_prepare <- renderDataTable({
      DT::datatable(if (is.null(hs_train)) NULL else hs_train@data %>% select(!matches("spc")),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$after_prepare_rows_selected,
  {
    output$after_prepare_plot <- renderPlotly({
      validate(need(input$after_prepare_rows_selected, ""))
      index <- input$after_prepare_rows_selected
      item <- hs$val[["train"]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
