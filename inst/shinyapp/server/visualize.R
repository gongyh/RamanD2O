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
      zip_dir <- file.path(tempdir(), input$hs_selector_for_export)
      dir.create(zip_dir)
      files <- c()
      for (i in (1:nrow(data))){
        cell <- data[i]
        txtdf <-data.frame(shift=cell@wavelength,intensity=t(cell$spc))
        txtname <- file.path(tempdir(), input$hs_selector_for_export, paste0(cell$ID_Cell, ".txt"))
        write.table(txtdf, txtname, row.names=F, col.names=F, quote=F, sep = "\t")
      }
      zip(file.path(tempdir(), "tmp.zip"), zip_dir)
    }
  }
)
