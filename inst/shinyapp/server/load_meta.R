# load metadata table
observeEvent(input$load_meta, {
  withBusyIndicatorServer("load_meta", {
    if (!is.null(input$meta_file$datapath)) {
      df <- read.table(input$meta_file$datapath, header = T, sep = "\t")
      # check whether all spectra file have metadata lines
      if (is.null(scrs$spc)) {
        # showNotification("Please load spectrum files first!", type = "error", duration = 10)
        # showModal(modalDialog("Please load spectrum files first!",
        #  title = "Error", easyClose = TRUE
        # ))
        toastr_error("Please load spectrum files first!", position = "top-center")
        return()
      }
      file_ids <- scrs$spc$ID_Cell
      diffs <- setdiff(file_ids, df$ID_Cell)
      if (length(diffs) > 0) {
        # showNotification("Metadata does not include all spectrum files!", type = "error", duration = 10)
        toastr_error("Metadata does not include all spectrum files!", position = "top-center")
        return()
      }
      meta$tbl <- df[df$ID_Cell %in% file_ids, ]
      rawdata <- merge(meta$tbl, scrs$spc, by = "ID_Cell")
      ncol_meta <- ncol(meta$tbl)
      spc <- rawdata[, (ncol_meta + 1):ncol(rawdata)] %>% mutate_if(is.factor, as.character)
      rownames(spc) <- rawdata$ID_Cell
      hs_raw <- new("hyperSpec",
        data = rawdata[, 1:ncol_meta], spc = data.matrix(spc),
        wavelength = as.numeric(colnames(scrs$spc)[2:ncol(scrs$spc)])
      )
      hs$val[["raw"]] <- hs_raw
      # showNotification(paste0("Successfully load metadata for ", nrow(meta$tbl), " spectra."), type = "message", duration = 10)
      # showModal(modalDialog(paste0("Successfully load metadata for ", nrow(meta$tbl), " spectra."),
      #  title = "Message", easyClose = TRUE
      # ))
      toastr_success(paste0("Successfully load metadata for ", nrow(meta$tbl), " spectra."), position = "top-center")
    } else {
      # showModal(modalDialog("No file selected!", title = "Error", easyClose = TRUE))
      toastr_error("No file selected!", position = "top-center")
    }
  })
})

observeEvent(meta$tbl,
  {
    # req(meta$tbl)
    output$meta_table <- renderDataTable({
      DT::datatable(meta$tbl,
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)
