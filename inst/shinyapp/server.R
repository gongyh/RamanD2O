library(shiny)
library(shinyFiles)
library(shinyjs)
library(fs)
library(DT)
library(htmlwidgets)

library(ggpubr)
#library(RColorBrewer)

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

  output$filedf <- renderTable({
    if(is.null(input$scrs_file)){return ()}
    input$scrs_file
  })


  # Unzipping files on click of button
  observeEvent(input$unzip,
               output$zipped <- renderTable({
                 unzip(input$scrs_file$datapath, list = TRUE, exdir = getwd())
               })
  )


}
