output$hs_select_for_export <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("normalized" %in% hs_all) {
    selected <- "normalized"
  } else if ("baselined" %in% hs_all) {
    selected <- "baselined"
  }
  selectInput("hs_selector_for_export", "Choose target", choices = hs_all, selected = selected)
})


output$download <- downloadHandler(
  filename = function() {
    name <- paste0("spectra-", input$hs_selector_for_export, "-", format(Sys.time(), "%Y%m%d%H%M%S"))
    suffix <- ".csv"
    if (input$select_type == "csv") {
      suffix <- ".csv"
    } else if (input$select_type == "zip") {
      suffix <- ".zip"
    }
    paste0(name, suffix)
  },
  content = function(file) {
    data <- hs$val[[input$hs_selector_for_export]]
    if (input$select_type == "csv") {
      write.csv(data, file)
    } else if (input$select_type == "zip") {
      setwd(tempdir())
      zip_dir <- input$hs_selector_for_export
      meta <- data@data
      meta$spc <- NULL
      write.table(meta, "meta.txt", row.names = F, col.names = T, quote = F, sep = "\t")
      if (!dir.exists(zip_dir)) dir.create(zip_dir)
      files <- c()
      for (i in (1:nrow(data))) {
        cell <- data[i]
        txtdf <- data.frame(shift = cell@wavelength, intensity = t(cell$spc))
        txtname <- file.path(zip_dir, paste0(cell$ID_Cell, ".txt"))
        write.table(txtdf, txtname, row.names = F, col.names = F, quote = F, sep = "\t")
      }
      zip::zip(zipfile = file, c(zip_dir, "meta.txt"))
    }
  }
)

observeEvent(input$hs_selector_for_export,
  {
    df <- NULL
    if (!is.null(input$hs_selector_for_export)) {
      hs_cur <- hs$val[[input$hs_selector_for_export]]
      df <- as.data.frame(hs_cur$spc[, 1:6]) %>% mutate_if(is.numeric, round2)
      rownames(df) <- rownames(hs_cur$spc)
    }
    output$visualize_table <- renderDataTable({
      DT::datatable(df, escape = FALSE, selection = "single", options = list(searchHighlight = TRUE, scrollX = TRUE))
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$visualize_table_rows_selected,
  {
    output$after_visualize_plot <- renderPlotly({
      validate(need(input$hs_selector_for_export, ""))
      validate(need(input$visualize_table_rows_selected, ""))
      index <- input$visualize_table_rows_selected
      item <- hs$val[[input$hs_selector_for_export]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
