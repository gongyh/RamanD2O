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
    # shinyjs::disable("smooth")
    if (input$hs_selector_for_smooth == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      # shinyjs::enable("smooth")
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
      output$smoothed_table <- renderDataTable({
        df <- as.data.frame(hs_sm$spc[, 1:6]) %>% mutate_if(is.numeric, round2)
        colnames(df) <- hs_sm@wavelength[1:6]
        rownames(df) <- rownames(hs_sm$spc)
        DT::datatable(df,
          escape = FALSE, selection = "single",
          options = list(searchHighlight = TRUE, scrollX = TRUE)
        )
      })
    }
    # shinyjs::enable("smooth")
  })
})

observeEvent(input$smoothed_table_rows_selected, {
  index <- input$smoothed_table_rows_selected
  item <- hs$val[["smoothed"]][index]
  output$after_smooth_plot <- renderPlot({
    plot(item)
  })
})
