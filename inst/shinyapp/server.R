library(shiny)
library(shinyFiles)
library(shinyjs)
library(fs)
library(DT)
library(ggpubr)
library(stringr)
library(RColorBrewer)
library(shinybusy)

library(hyperSpec)
library(baseline)

options(encoding = "UTF-8")

options(shiny.maxRequestSize = 6000 * 1024^2)

source("globals.R")

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

  # Unzipping files on click of button
  observeEvent(input$unzip, {
    if (!is.null(input$scrs_file$datapath)) {
      # show_modal_spinner(spin="fulfilling-square",color="#ff1d5e",text="Please wait ...")
      show_modal_progress_line(value = 0, text = "Decompressing ...")
      files <- unzip(input$scrs_file$datapath, list = FALSE, exdir = tempdir())
      txtfiles <- str_subset(files, ".*..txt")
      i <- 1
      total <- length(txtfiles)
      if (total == 0) {
        remove_modal_progress()
        output$runtimeInfo <- renderText({
          msg <- "Error: No spectrum files found!"
        })
        return()
      }
      shift <- read.table(txtfiles[1], header = F, sep = "\t")$V1
      scrs_df <- c("filename", shift)
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
      # remove_modal_spinner()
      remove_modal_progress()
      output$runtimeInfo <- renderText({
        msg <- paste0("Found ", length(txtfiles), " spectrum files.")
      })
    }
  })
}
