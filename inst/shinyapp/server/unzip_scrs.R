# Unzipping files on click of button
observeEvent(input$unzip, {
  withBusyIndicatorServer("unzip", {
    if (!is.null(input$scrs_file$datapath)) {
      # show_modal_spinner(spin="fulfilling-square",color="#ff1d5e",text="Please wait ...")
      show_modal_progress_line(value = 0, text = "Decompressing ...")
      files <- unzip(input$scrs_file$datapath, list = FALSE, exdir = tempdir())
      txtfiles <- str_subset(files, ".*..txt")
      i <- 1
      total <- length(txtfiles)
      if (total == 0) {
        remove_modal_progress()
        # showNotification("No spectrum files found!", type = "error", duration = 10)
        showModal(modalDialog("No spectrum files found!",
          title = "Error", easyClose = TRUE
        ))

        return()
      }
      shift <- read.table(txtfiles[1], header = F, sep = "\t")$V1
      scrs_colnames <- c("ID_Cell", shift)
      scrs_df <- c()
      for (filename in txtfiles)
      {
        ID_Cell <- sub(".txt", "", basename(filename))
        dt <- read.table(filename, header = F, sep = "\t")$V2
        # remove Cosmic Rays
        dt2 <- removeCosmic(dt)
        if (dt2$cosmic) {
          cat(filename, "contains cosmic ray signal.\n")
        }
        data <- c(ID_Cell, dt2$spc)
        scrs_df <- cbind(scrs_df, data)
        update_modal_progress(i / total, paste0("Reading ", i, " spectrum (", floor(100 * i / total), "%)"))
        i <- i + 1
      }
      sc <- data.frame(t(scrs_df))
      colnames(sc) <- scrs_colnames
      rownames(sc) <- NULL
      scrs$spc <- sc
      output$spectra_files <- renderDataTable({
        DT::datatable(scrs$spc[, 1:5], escape = FALSE, selection = "single", options = list(searchHighlight = TRUE, scrollX = TRUE))
      })
      # remove_modal_spinner()
      remove_modal_progress()
      # showNotification(paste0("Load ", length(txtfiles), " spectrum files."), type = "message", duration = 10)
      showModal(modalDialog(paste0("Load ", length(txtfiles), " spectrum files."),
        title = "Message", easyClose = TRUE
      ))
    } else {
      showModal(modalDialog("No spectra zip file selected!", title = "Error", easyClose = TRUE))
    }
  })
})
