output$hs_select_for_baseline <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("smoothed" %in% hs_all) {
    selected <- "smoothed"
  } else if ("filtered" %in% hs_all) {
    selected <- "filtered"
  }
  selectInput("hs_selector_for_baseline", "Choose target", choices = hs_all, selected = selected)
})

# after select baseline method, show corresponding parameters
observeEvent(input$select_baseline, {
  output$baseline_config <- renderUI({
    if (input$select_baseline == "polyfit") {
      numericInput("polyfit_order", "Order", 7, min = 3, max = 10, step = 1)
    } else if (input$select_baseline == "als") {
      ""
    } else {
      "Error: not implemented yet!"
    }
  })
})


# baseline scrs on click of button
observeEvent(input$baseline, {
  withBusyIndicatorServer("baseline", {
    # shinyjs::disable("baseline")
    if (input$hs_selector_for_baseline == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      # shinyjs::enable("baseline")
      return()
    } else {
      hs_cur <- hs$val[[input$hs_selector_for_baseline]]
      wavelength <- wl(hs_cur)
      # baseline
      if (input$select_baseline == "als") {
        b_als <- baseline(hs_cur$spc, method = "als")
        data <- hs_cur@data
        data$spc <- NULL
        hs_bl <- new("hyperSpec",
          data = data,
          spc = getCorrected(b_als), wavelength = wavelength
        )
      } else if (input$select_baseline == "polyfit") {
        order <- input$polyfit_order
        hs_bl <- hs_cur - spc.fit.poly.below(hs_cur, poly.order = 7)
        hs_bl$spc <- unAsIs(hs_bl$spc)
        dimnames(hs_bl$spc) <- dimnames(hs_cur$spc)
      } else {
        shinyalert("Oops!", "Baseline method not implemented yet.", type = "error")
        # shinyjs::enable("baseline")
        return()
      }
      # handle negative
      if (input$select_negative == "zero") {
        hs_bl_spc <- hs_bl$spc
        hs_bl_spc[hs_bl_spc < 0] <- 0
        hs_bl$spc <- hs_bl_spc
      } else if (input$select_negative == "up") {
        yminset <- apply(hs_bl$spc, 1, min)
        hs_bl_spc <- hs_bl$spc + abs(yminset)
        hs_bl$spc <- hs_bl_spc
      } else if (input$select_negative == "keep") {
        # need to do nothing
      } else {
        # treat as keep
      }
      hs$val[["baselined"]] <- hs_bl
      output$baselined_table <- renderDataTable({
        df <- as.data.frame(hs_bl$spc[, 1:6]) %>% mutate_if(is.numeric, round2)
        rownames(df) <- rownames(hs_bl$spc)
        DT::datatable(df,
          escape = FALSE, selection = "single",
          options = list(searchHighlight = TRUE, scrollX = TRUE)
        )
      })
    }
    # shinyjs::enable("baseline")
  })
})

observeEvent(input$baselined_table_rows_selected, {
  index <- input$baselined_table_rows_selected
  item <- hs$val[["baselined"]][index]
  output$after_baseline_plot <- renderPlotly({
    p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
    ggplotly(p) %>% config(mathjax = 'cdn')
  })
})
