output$hs_select_for_subsample <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("raw" %in% hs_all) {
    selected <- "raw"
  }
  selectInput("hs_selector_for_subsample", "Choose target", choices = hs_all, selected = selected)
})

# sabsample scrs on click of button
observeEvent(input$subsample, {
  withBusyIndicatorServer("subsample", {
    # shinyjs::disable("subsample")
    if (isolate(input$hs_selector_for_subsample) == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      # shinyjs::enable("subsample")
      return()
    } else {
      hs_cur <- hs$val[[isolate(input$hs_selector_for_subsample)]]
      total <- nrow(hs_cur)
      size <- floor(isolate(input$percentage) / 100.0 * total)
      tindex <- isample(hs_cur)
      index <- tindex[1:max(size, 2)]
      sampled <- hs_cur[index]
      hs$val[["sampled"]] <- sampled
      # showNotification(paste0("Subsampled ", nrow(sampled), " spectra."), type = "message", duration = 10)
      # showModal(modalDialog(paste0("Subsampled ", nrow(sampled), " spectra."),
      #  title = "Message", easyClose = TRUE
      # ))
      toastr_success(paste0("Subsampled ", nrow(sampled), " spectra."), position = "top-center")
    }
    # shinyjs::enable("subsample")
  })
})

observeEvent(hs$val[["sampled"]],
  {
    # req(hs$val[["sampled"]])
    sampled <- hs$val[["sampled"]]
    output$sampled_table <- renderDataTable({
      DT::datatable(if (is.null(sampled)) NULL else sampled@data %>% select(!matches("spc")),
        escape = FALSE, selection = "single",
        extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$sampled_table_rows_selected,
  {
    output$after_subsample_plot <- renderPlotly({
      validate(need(input$sampled_table_rows_selected, ""))
      index <- input$sampled_table_rows_selected
      item <- hs$val[["sampled"]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
