# Unzipping files on click of button
observeEvent(input$unzip, {
  withBusyIndicatorServer("unzip", {
    if (!is.null(isolate(input$scrs_file$datapath))) {
      # show_modal_spinner(spin="fulfilling-square",color="#ff1d5e",text="Please wait ...")
      show_modal_progress_line(value = 0, text = "Decompressing ...")
      files <- utils::unzip(isolate(input$scrs_file$datapath), list = FALSE, exdir = tempdir())
      txtfiles <- str_subset(files, ".*..txt")
      i <- 1
      total <- length(txtfiles)
      if (total == 0) {
        remove_modal_progress()
        toastr_error("No spectrum files found!", position = "top-center")
        return()
      }
      if (isolate(input$align)) {
	    # different shift
        max_min_values <- lapply(files, function(f) {
        data <- read.table(f, header = FALSE)$V1
        max_value <- max(data, na.rm = TRUE)
        min_value <- min(data, na.rm = TRUE)
        list(max = max_value, min = min_value)
        })
        scrs_min <- max(sapply(max_min_values, function(x) x$min))
        scrs_max <- min(sapply(max_min_values, function(x) x$max))
        shift <- read.table(txtfiles[1], header = F, sep = "\t")$V1
        shift <- shift[shift >= scrs_min & shift <= scrs_max]
        scrs_colnames <- c("ID_Cell", shift)
        scrs_df <- matrix(nrow = length(scrs_colnames), ncol = total)
        for (filename in txtfiles) {
          ID_Cell <- sub(".txt", "", basename(filename))
          content <- read.table(filename, header = F, sep = "\t")
          shift_cur <- content$V1
          dt <- removeCosmic(content$V2)
          hs_data <- new("hyperSpec", wavelength = shift_cur, spc = dt$spc)
          hs_align <- spc.loess(hs_data, shift, normalize = F)
          data <- c(ID_Cell, hs_align$spc)
          scrs_df[, i] <- data
          update_modal_progress(i / total, paste0("Reading ", i, " spectrum (", floor(100 * i / total), "%)"))
          i <- i + 1
        }
      } else {
	    # same shift
        shift <- read.table(txtfiles[1], header = F, sep = "\t")$V1
        scrs_colnames <- c("ID_Cell", shift)
        scrs_df <- matrix(nrow = length(scrs_colnames), ncol = total)
        for (filename in txtfiles)
        {
          ID_Cell <- sub(".txt", "", basename(filename))
          content <- read.table(filename, header = F, sep = "\t")
          cur_shift <- content$V1
          ## TBD: compare shift with cur_shift, should be same, error if not same
          dt <- content$V2
          # remove Cosmic Rays
          dt2 <- removeCosmic(dt)
          # if (dt2$cosmic) {
          #   cat(filename, "contains cosmic ray signal.\n")
          # }
          data <- c(ID_Cell, dt2$spc)
          scrs_df[, i] <- data
          update_modal_progress(i / total, paste0("Reading ", i, " spectrum (", floor(100 * i / total), "%)"))
          i <- i + 1
        }
      }

      sc <- data.frame(t(scrs_df), stringsAsFactors = T)
      colnames(sc) <- scrs_colnames
      rownames(sc) <- NULL
      scrs$spc <- sc
      # remove_modal_spinner()
      remove_modal_progress()
      toastr_success(paste0("Load ", length(txtfiles), " spectrum files."), position = "top-center")
    } else {
      toastr_error("No spectra zip file selected!", position = "top-center")
    }
  })
})

# load metadata table
observeEvent(input$load_meta, {
  withBusyIndicatorServer("load_meta", {
    if (!is.null(isolate(input$meta_file$datapath))) {
      df <- read.table(isolate(input$meta_file$datapath), header = T, sep = "\t", stringsAsFactors = T)
      # check whether all spectra file have metadata lines
      if (is.null(scrs$spc)) {
        toastr_error("Please load spectrum files first!", position = "top-center")
        return()
      }
      file_ids <- scrs$spc$ID_Cell
      diffs <- setdiff(file_ids, df$ID_Cell)
      if (length(diffs) > 0) {
        toastr_error("Metadata does not include all spectrum files!", position = "top-center")
        return()
      }
      meta$tbl <- df[df$ID_Cell %in% file_ids, ]
      rawdata <- merge(meta$tbl, scrs$spc, by = "ID_Cell")
      ncol_meta <- ncol(meta$tbl)
      spc <- rawdata[, (ncol_meta + 1):ncol(rawdata)] %>%
        mutate_if(is.factor, as.character) %>%
        mutate_if(is.character, as.numeric)
      rownames(spc) <- rawdata$ID_Cell
      hs_raw <- new("hyperSpec",
        data = rawdata[, 1:ncol_meta], spc = as.matrix(spc),
        wavelength = as.numeric(colnames(scrs$spc)[2:ncol(scrs$spc)])
      )
      hs$val[["raw"]] <- hs_raw
      toastr_success(paste0("Successfully load metadata for ", nrow(meta$tbl), " spectra."), position = "top-center")
    } else {
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
