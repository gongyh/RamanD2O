output$hs_select_for_filter <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("trimmed" %in% hs_all) {
    selected <- "trimmed"
  } else if ("sampled" %in% hs_all) {
    selected <- "sampled"
  }
  selectInput("hs_selector_for_filter", "Choose target", choices = hs_all, selected = selected)
})

# filter scrs on click of button
observeEvent(input$filter, {
  withBusyIndicatorServer("filter", {
    if (isolate(input$hs_selector_for_filter) == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[isolate(input$hs_selector_for_filter)]]
      if (isolate(input$filter_low)) {
        low.int <- apply(hs_cur, 1, max) < isolate(input$lowest)
        hs_cur <- hs_cur[!low.int]
      }
      if (isolate(input$filter_high)) {
        high.int <- apply(hs_cur > isolate(input$highest), 1, any)
        hs_cur <- hs_cur[!high.int]
      }
      if (isolate(input$filter_sd)) {
        OK <- apply(hs_cur[[]], 2, mean_sd_filter, n = isolate(input$n_sd))
        hs_cur <- hs_cur[apply(OK, 1, all)]
      }
      hs$val[["filtered"]] <- hs_cur
    }
  })
})

observeEvent(hs$val[["filtered"]],
  {
    hs_fl <- hs$val[["filtered"]]
    output$after_filter <- renderDataTable({
      DT::datatable(if (is.null(hs_fl)) NULL else hs_fl@data %>% select(!matches("spc")),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$after_filter_rows_selected,
  {
    output$after_filter_plot <- renderPlotly({
      validate(need(input$after_filter_rows_selected, ""))
      index <- input$after_filter_rows_selected
      item <- hs$val[["filtered"]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)

# remove scrs on click of button
observeEvent(input$remove, {
  withBusyIndicatorServer("remove", {
    if (is.null(isolate(input$after_filter_rows_selected))) {
      shinyalert("Oops!", "Please first select one spectrum to visualize.", type = "error")
      return()
    } else {
      shinyalert("Caution", "Confirm to delete this spectrum?",
        type = "info", closeOnClickOutside = T, showCancelButton = T,
        callbackR = function(x) {
          if (x) hs$val[["filtered"]] <- hs$val[["filtered"]][-isolate(input$after_filter_rows_selected)]
        }
      )
    }
  })
})
