# connect mongodb on click of button
observeEvent(input$connectdb, {
  tryCatch(
    {
      mongo_connection$obj <- mongo(collection = input$collection, db = input$db, url = input$url)
    },
    error = function(e) {
      shinyalert("Oops!", e$message, type = "error")
    },
    finally = {
      output$db_message <- renderText({
        paste0("MongoDB is connected at ", Sys.time())
      })
      shinyjs::disable("connectdb")
    }
  )
})

observeEvent(
  {
    input$collection
    input$db
    input$url
  },
  {
    shinyjs::enable("connectdb")
  }
)

output$hs_select_for_database <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("normalized" %in% hs_all) {
    selected <- "normalized"
  } else if ("raw" %in% hs_all) {
    selected <- "raw"
  }
  selectInput("hs_selector_for_database", "Choose target", choices = hs_all, selected = selected)
})

# save spectra to database on click of button
observeEvent(input$savedb, {
  req(mongo_connection$obj)
  withBusyIndicatorServer("savedb", {
    if (input$hs_selector_for_database == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[input$hs_selector_for_database]]
      hs_df <- as.wide.df(hs_cur)
      hs_df$dtype <- input$hs_selector_for_database
      tryCatch(
        {
          mongo_connection$obj$insert(hs_df)
        },
        error = function(e) {
          shinyalert("Oops!", e$message, type = "error")
        },
        finally = {
          output$db_message <- renderText({
            paste0("Data loaded into database at ", Sys.time())
          })
        }
      )
    }
  })
})
