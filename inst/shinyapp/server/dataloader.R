# Unzipping files on click of button
observeEvent(input$unzip, {
  withBusyIndicatorServer("unzip", {
    if (!is.null(input$scrs_file$datapath)) {
      # show_modal_spinner(spin="fulfilling-square",color="#ff1d5e",text="Please wait ...")
      show_modal_progress_line(value = 0, text = "Decompressing ...")
      files <- utils::unzip(input$scrs_file$datapath, list = FALSE, exdir = tempdir())
      txtfiles <- str_subset(files, ".*..txt")
      i <- 1
      total <- length(txtfiles)
      if (total == 0) {
        remove_modal_progress()
        # showNotification("No spectrum files found!", type = "error", duration = 10)
        # showModal(modalDialog("No spectrum files found!",
        #  title = "Error", easyClose = TRUE
        # ))
        toastr_error("No spectrum files found!", position = "top-center")
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
      # remove_modal_spinner()
      remove_modal_progress()
      # showNotification(paste0("Load ", length(txtfiles), " spectrum files."), type = "message", duration = 10)
      # showModal(modalDialog(paste0("Load ", length(txtfiles), " spectrum files."),
      #  title = "Message", easyClose = TRUE
      # ))
      toastr_success(paste0("Load ", length(txtfiles), " spectrum files."), position = "top-center")
    } else {
      # showModal(modalDialog("No spectra zip file selected!", title = "Error", easyClose = TRUE))
      toastr_error("No spectra zip file selected!", position = "top-center")
    }
  })
})

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
        options = list(deferRender = T, searchHighlight = T, scrollX = T)
      )
    })
  },
  ignoreNULL = FALSE
)
