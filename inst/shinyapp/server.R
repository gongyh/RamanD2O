library(shiny)
library(shinyFiles)
library(shinyjs)
library(fs)
library(DT)
library(ggpubr)
library(stringr)

options(encoding ="UTF-8")

options(shiny.maxRequestSize = 6000*1024^2)

source("globals.R")
#source("helpers.R")

#prepare colors
cols1<-brewer.pal(12,'Paired')
cols1<-cols1[-11]
cols2<-brewer.pal(8,'Dark2')
cols<-c(cols1,cols2)


function(input, output, session) {


  # Unzipping files on click of button
  observeEvent(input$unzip, {
                 if(!is.null(input$scrs_file$datapath)) {
                   files <- unzip(input$scrs_file$datapath, list=FALSE, exdir=tempdir())
                   txtfiles <- str_subset(files, ".*..txt")
                   output$zipped <- renderTable({
                     txtfiles
                   })
                 }
              }
  )


}
