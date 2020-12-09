output$hs_select_for_cartenoid <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("normalized" %in% hs_all) {
    selected <- "normalized"
  } else if ("baselined" %in% hs_all) {
    selected <- "baselined"
  }
  selectInput("hs_selector_for_cartenoid", "Choose target", choices = hs_all, selected = selected)
})

# detect cartenoid scrs on click of button
observeEvent(input$cartenoid, {
  withBusyIndicatorServer("cartenoid", {
    if (input$hs_selector_for_cartenoid == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[input$hs_selector_for_cartenoid]]
      wavelength <- wl(hs_cur)
      keep <- c()
      for (i in (1:nrow(hs_cur))) {
        peaks <- wavelength[findPeaks(hs_cur[i]$spc)]
        if ((length(peaks[(peaks > 992) & (peaks < 1010)]) >= 1) && (length(peaks[(peaks > 1145) & (peaks < 1160)]) >= 1) &&
          (length(peaks[(peaks > 1480) & (peaks < 1525)]) >= 1)) { # cartenoid peaks
          keep <- c(keep, FALSE)
        } else {
          keep <- c(keep, TRUE)
        }
      }
      hs_bl <- hs_cur
      if (input$filter_cartenoid == TRUE) {
        hs_bl <- hs_cur[keep]
      } else {
        hs_bl$Cartenoid <- !keep
      }
      hs$val[["cartenoid"]] <- hs_bl
    }
  })
})

observeEvent(hs$val[["cartenoid"]],
  {
    hs_bl <- hs$val[["cartenoid"]]
    output$cartenoid_table <- renderDataTable({
      DT::datatable(if (is.null(hs_bl)) NULL else hs_bl@data %>% select(!matches("spc")),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$cartenoid_table_rows_selected,
  {
    output$after_cartenoid_plot <- renderPlotly({
      validate(need(input$cartenoid_table_rows_selected, ""))
      index <- input$cartenoid_table_rows_selected
      item <- hs$val[["cartenoid"]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
