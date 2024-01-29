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

# convert filter_range and filter_min/filter_max
observeEvent(c(input$filter_min, input$filter_max), {
  updateSliderInput(session, "filter_range", value = c(input$filter_min, input$filter_max))
})
observeEvent(c(input$filter_range[1], input$filter_range[2]), {
  updateNumericInput(session, "filter_min", value = input$filter_range[1])
  updateNumericInput(session, "filter_max", value = input$filter_range[2])
})

# filter scrs on click of button
output$after_filter <- NULL
observeEvent(input$filter, {
  withBusyIndicatorServer("filter", {
    if (isolate(input$hs_selector_for_filter) == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[isolate(input$hs_selector_for_filter)]]
      hs_cur_range <- hs_cur[, , isolate(input$filter_min) ~ isolate(input$filter_max)]
      if (isolate(input$filter_low)) {
        low.int <- apply(hs_cur_range, 1, max) < isolate(input$lowest)
        hs_cur <- hs_cur[!low.int]
      }
      if (isolate(input$filter_high)) {
        high.int <- apply(hs_cur_range > isolate(input$highest), 1, any)
        hs_cur <- hs_cur[!high.int]
      }
      if (isolate(input$filter_sd)) {
        OK <- apply(hs_cur_range[[]], 2, mean_sd_filter, n = isolate(input$n_sd))
        hs_cur <- hs_cur[apply(OK, 1, all)]
      }
      output$after_filter <- renderDataTable({
        DT::datatable(if (is.null(hs_cur)) NULL else hs_cur@data %>% dplyr::select(!matches("spc")),
          escape = FALSE, selection = "multiple", extensions = list("Responsive", "Scroller"),
          options = list(searchHighlight = TRUE, scrollX = TRUE)
        )
      })
      hs$val[["filtered"]] <- hs_cur
    }
  })
})

observeEvent(input$after_filter_rows_selected,
  {
    output$after_filter_plot <- renderPlotly({
      validate(need(isolate(input$after_filter_rows_selected), ""))
      index <- isolate(input$after_filter_rows_selected)
      item <- hs$val[["filtered"]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)

# remove scrs on click of button
proxy <- dataTableProxy("after_filter")

observeEvent(input$remove, {
  withBusyIndicatorServer("remove", {
    if (is.null(isolate(input$after_filter_rows_selected))) {
      shinyalert("Oops!", "Please first select one spectrum to visualize.", type = "error")
      return()
    } else {
      shinyalert("Caution", "Confirm to delete this spectrum?",
        type = "info", closeOnClickOutside = TRUE, showCancelButton = TRUE,
        callbackR = function(x) {
          if (x) hs$val[["filtered"]] <- hs$val[["filtered"]][-isolate(input$after_filter_rows_selected)]
          replaceData(proxy, data = hs$val[["filtered"]]@data %>% dplyr::select(!matches("spc")))
          if (length(isolate(input$after_filter_rows_selected)) == 1) {
            selectRows(proxy, as.numeric(isolate(input$after_filter_rows_selected)))
          }
        }
      )
    }
  })
})

observeEvent(input$prev_rm, {
  proxy %>% selectRows(if (length(isolate(input$after_filter_rows_selected)) == 1 &&
                             isolate(input$after_filter_rows_selected) > 1) {
    as.numeric(isolate(input$after_filter_rows_selected) - 1)
  } else {
    as.numeric(isolate(input$after_filter_rows_selected))
  })
})

observeEvent(input$next_rm, {
  proxy %>% selectRows(if (length(isolate(input$after_filter_rows_selected)) == 1 &&
                             isolate(input$after_filter_rows_selected) < length(input$after_filter_rows_all)) {
    as.numeric(isolate(input$after_filter_rows_selected) + 1)
  } else {
    as.numeric(isolate(input$after_filter_rows_selected))
  })
})
