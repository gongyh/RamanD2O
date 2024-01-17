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
        shift <- round(scrs_min):round(scrs_max)
        scrs_colnames <- c("ID_Cell", shift)
        scrs_df <- matrix(nrow = length(scrs_colnames), ncol = total)
        for (filename in txtfiles) {
          ID_Cell <- sub(".txt", "", basename(filename))
          content <- read.table(filename, header = F, sep = "\t")
          shift_cur <- content$V1
          dt <- removeCosmic(content$V2)
          hs_data <- new("hyperSpec", wavelength = shift_cur, spc = dt$spc)
          hs_align <- spc_loess(hs_data, shift, normalize = F)
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
          # compare shift with cur_shift, should be same, error if not same
          if (!identical(shift, cur_shift)) {
            remove_modal_progress()
            toastr_error("Wavelength differences found, please double check!", position = "top-center")
            return()
          }
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

observeEvent(input$tips1, {
  showModal(
    modalDialog(
      includeMarkdown('#### **How to save the filenames in a folder to a text file?**
                      \n(1)
                      \nOpen the Command Prompt (CMD): Press Win+R, then type "cmd", and press Enter.
                      \n(2)
                      \nSwitch to the desired folder: Use the “cd” command to navigate to the folder
                      that contains the desired files.
                      \n`cd C:\\Users\\YourUsername\\Documents`
                      \nReplace path with your actual path.
                      \n(3)
                      \nRun the “dir” command with additional parameters: In the command prompt,
                      enter “dir /b” and press the space key. This will list the file names of all files
                      in the folder in a compact format.
                      \n(4)
                      \nAdd ">filenames.txt": After the previous command, add ">filenames.txt" where "filenames"
                      is the name of the text file you want to save. For example, if you want to save it as
                      "filenames.txt", the complete command would be:
                      \n`dir /b > filenames.txt`
                      \nExecuting this command will save the file names of all files in the folder to a text file
                      named "filenames.txt".
                      \n(5)
                      \nLocate the text file: Check if a text file named “filenames.txt” (or the name you specified)
                      has been created in the folder. You can look in the corresponding folder and make sure that
                      the file name and saved location match what you specified.
                      '),
      footer = modalButton("Close")
    )
  )
})

observeEvent(input$tips2, {
  showModal(
    modalDialog(
      includeMarkdown('#### **About meta.tsv file!**
        \n(1)
        \nTSV (Tab-Separated Values) files are a type of plain text file where the data
        fields are separated by tab characters.
        \n(2)
        \nYou can obtain a TSV file using Excel. Enter your data into spreadsheet,
        save the file in the TSV format or other Tab-Separated format and keep the suffix of filename to tsv.'),
      renderTable(data.frame(
        ID_Cell = c("file1", "file2", "file3"),
        Group1 = c("Control", "Treat", "Treat"),
        Group2 = c("Hot", "Hot", "Cold")
      ), bordered = TRUE, align = "c"),
      includeMarkdown('\n(3)
        \nThe first column should be `ID_Cell`, feel free to add at least one columns.'),
      footer = modalButton("Close")
    )
  )
})
