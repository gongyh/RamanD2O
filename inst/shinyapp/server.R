library(shiny)
library(shinyFiles)
library(shinyjs)
library(fs)
library(DT)
library(ggpubr)
library(stringr)
library(RColorBrewer)
library(shinybusy)

library(baseline)
library(permute)
library(shinyalert)

library(dplyr)

options(encoding = "UTF-8")

options(shiny.maxRequestSize = 6000 * 1024^2)

source("globals.R")
source("helpers.R")

# prepare colors
cols1 <- brewer.pal(12, "Paired")
cols1 <- cols1[-11]
cols2 <- brewer.pal(8, "Dark2")
cols <- c(cols1, cols2)


function(input, output, session) {
  # print one dot every minute to prevent gray-out
  autoInvalidate <- reactiveTimer(60000)
  observe({
    autoInvalidate()
    cat(".")
  })

  output$hs_select_for_subsample <- renderUI({
    hs_all <- names(hs$val)
    selected <- NULL
    if ("raw" %in% hs_all) {
      selected <- "raw"
    }
    selectInput("hs_selector_for_subsample", "Choose target", choices = hs_all, selected = selected)
  })

  output$hs_select_for_trim <- renderUI({
    hs_all <- names(hs$val)
    selected <- NULL
    if ("sampled" %in% hs_all) {
      selected <- "sampled"
    }
    selectInput("hs_selector_for_trim", "Choose target", choices = hs_all, selected = selected)
  })

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

  output$hs_select_for_baseline <- renderUI({
    hs_all <- names(hs$val)
    selected <- NULL
    if ("smoothed" %in% hs_all) {
      selected <- "smoothed"
    } else if ("filtered" %in% hs_all) {
      selected <- "filtered"
    }
    selectInput("hs_selector_for_baseline", "Choose target", choices = hs_all, selected = selected)
  })

  # Unzipping files on click of button
  observeEvent(input$unzip, {
    shinyjs::disable("unzip")
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
        shinyjs::enable("unzip")
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
    }
    shinyjs::enable("unzip")
  })


  # load metadata table
  observeEvent(input$load_meta, {
    shinyjs::disable("load_meta")
    if (!is.null(input$meta_file$datapath)) {
      df <- read.table(input$meta_file$datapath, header = T, sep = "\t")
      # check whether all spectra file have metadata lines
      if (is.null(scrs$spc)) {
        # showNotification("Please load spectrum files first!", type = "error", duration = 10)
        showModal(modalDialog("Please load spectrum files first!",
          title = "Error", easyClose = TRUE
        ))
        shinyjs::enable("load_meta")
        return()
      }
      file_ids <- scrs$spc$ID_Cell
      diffs <- setdiff(file_ids, df$ID_Cell)
      if (length(diffs) > 0) {
        showNotification("Metadata does not include all spectrum files!", type = "error", duration = 10)
        shinyjs::enable("load_meta")
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
      showModal(modalDialog(paste0("Successfully load metadata for ", nrow(meta$tbl), " spectra."),
        title = "Message", easyClose = TRUE
      ))
      output$meta_table <- renderDataTable({
        DT::datatable(meta$tbl, escape = FALSE, selection = "single", options = list(searchHighlight = TRUE, scrollX = TRUE))
      })
    }
    shinyjs::enable("load_meta")
  })


  # sabsample scrs on click of button
  observeEvent(input$subsample, {
    shinyjs::disable("subsample")
    if (input$hs_selector_for_subsample == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      shinyjs::enable("subsample")
      return()
    } else {
      hs_cur <- hs$val[[input$hs_selector_for_subsample]]
      total <- nrow(hs_cur)
      size <- floor(input$percentage / 100.0 * total)
      index <- isample(hs_cur, size = max(size, 2))
      if (input$shuffle) {
        index <- shuffle(index)
      }
      sampled <- hs_cur[index]
      hs$val[["sampled"]] <- sampled
      # showNotification(paste0("Subsampled ", nrow(sampled), " spectra."), type = "message", duration = 10)
      showModal(modalDialog(paste0("Subsampled ", nrow(sampled), " spectra."),
        title = "Message", easyClose = TRUE
      ))
      output$sampled_table <- renderDataTable({
        DT::datatable(sampled$spc[, 1:6], escape = FALSE, selection = "single", options = list(searchHighlight = TRUE, scrollX = TRUE))
      })
    }
    shinyjs::enable("subsample")
  })

  observeEvent(input$sampled_table_rows_selected, {
    index <- input$sampled_table_rows_selected
    item <- hs$val[["sampled"]][index]
    output$after_subsample_plot <- renderPlot({
      plot(item)
    })
  })

  # trim scrs on click of button
  observeEvent(input$trim, {
    shinyjs::disable("trim")
    if (input$hs_selector_for_trim == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      shinyjs::enable("trim")
      return()
    } else {
      hs_cur <- hs$val[[input$hs_selector_for_trim]]
      minR <- input$trim_range[1]
      maxR <- input$trim_range[2]
      hs_tm <- hs_cur[, , minR ~ maxR]
      hs$val[["trimmed"]] <- hs_tm
      output$after_trim <- renderDataTable({
        DT::datatable(hs_tm$spc[, 1:6], escape = FALSE, selection = "single", options = list(searchHighlight = TRUE, scrollX = TRUE))
      })
    }
    shinyjs::enable("trim")
  })

  observeEvent(input$after_trim_rows_selected, {
    index <- input$after_trim_rows_selected
    item <- hs$val[["trimmed"]][index]
    output$after_trim_plot <- renderPlot({
      plot(item)
    })
  })

  # smooth scrs on click of button
  observeEvent(input$smooth, {
    shinyjs::disable("smooth")
    if (input$hs_selector_for_smooth == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      shinyjs::enable("smooth")
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
        df <- as.data.frame(hs_sm$spc) %>% mutate_if(is.numeric, round2)
        colnames(df) <- hs_sm@wavelength
        rownames(df) <- rownames(hs_sm$spc)
        DT::datatable(df[, 1:6],
          escape = FALSE, selection = "single",
          options = list(searchHighlight = TRUE, scrollX = TRUE)
        )
      })
    }
    shinyjs::enable("smooth")
  })

  observeEvent(input$smoothed_table_rows_selected, {
    index <- input$smoothed_table_rows_selected
    item <- hs$val[["smoothed"]][index]
    output$after_smooth_plot <- renderPlot({
      plot(item)
    })
  })


  # after select baseline method, show corresponding parameters
  observeEvent(input$select_baseline, {
    output$baseline_config <- renderUI({
      if (input$select_baseline == "polyfit") {
        numericInput("polyfit_order", "Order", 7, min = 3, max = 10, step = 1)
      } else if (input$select_baseline == "als") {
        ""
      } else {
        "Error: not implemented yet!"
      }
    })
  })


  # baseline scrs on click of button
  observeEvent(input$baseline, {
    shinyjs::disable("baseline")
    if (input$hs_selector_for_baseline == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      shinyjs::enable("baseline")
      return()
    } else {
      hs_cur <- hs$val[[input$hs_selector_for_baseline]]
      wavelength <- wl(hs_cur)
      # baseline
      if (input$select_baseline == "als") {
        b_als <- baseline(hs_cur$spc, method = "als")
        data <- hs_cur@data
        data$spc <- NULL
        hs_bl <- new("hyperSpec",
          data = data,
          spc = getCorrected(b_als), wavelength = wavelength
        )
      } else if (input$select_baseline == "polyfit") {
        order <- input$polyfit_order
        hs_bl <- hs_cur - spc.fit.poly.below(hs_cur, poly.order = 7)
        hs_bl$spc <- unAsIs(hs_bl$spc)
        dimnames(hs_bl$spc) <- dimnames(hs_cur$spc)
      } else {
        shinyalert("Oops!", "Baseline method not implemented yet.", type = "error")
        shinyjs::enable("baseline")
        return()
      }
      # handle negative
      if (input$select_negative == "zero") {
        hs_bl_spc <- hs_bl$spc
        hs_bl_spc[hs_bl_spc < 0] <- 0
        hs_bl$spc <- hs_bl_spc
      } else if (input$select_negative == "up") {
        yminset <- apply(hs_bl$spc, 1, min)
        hs_bl_spc <- hs_bl$spc + abs(yminset)
        hs_bl$spc <- hs_bl_spc
      } else if (input$select_negative == "keep") {
        # need to do nothing
      } else {
        # treat as keep
      }
      hs$val[["baselined"]] <- hs_bl
      output$baselined_table <- renderDataTable({
        df <- as.data.frame(hs_bl$spc) %>% mutate_if(is.numeric, round2)
        rownames(df) <- rownames(hs_bl$spc)
        DT::datatable(df[, 1:6],
          escape = FALSE, selection = "single",
          options = list(searchHighlight = TRUE, scrollX = TRUE)
        )
      })
    }
    shinyjs::enable("baseline")
  })

  observeEvent(input$baselined_table_rows_selected, {
    index <- input$baselined_table_rows_selected
    item <- hs$val[["baselined"]][index]
    output$after_baseline_plot <- renderPlot({
      plot(item)
    })
  })
}
