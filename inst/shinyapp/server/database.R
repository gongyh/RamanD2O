# connect mongodb on click of button
observeEvent(input$connectdb, {
  tryCatch(
    {
      mongo_connection$obj <- mongo(collection = input$collection, db = input$db, url = input$url)
      dbstats$projects <- mongo_connection$obj$aggregate(
        '[{"$group":{"_id":"$project", "count": {"$sum":1}}}]',
        options = '{"allowDiskUse":true}'
      )
      names(dbstats$projects) <- c("project", "count")
    },
    error = function(e) {
      shinyalert("Oops!", e$message, type = "error")
    },
    finally = {
      output$db_message <- renderText({
        paste0("MongoDB is connected at ", Sys.time(), '\n\nproject, count\n', toString(dbstats$projects))
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
      hs_df$project <- input$project
      tryCatch(
        {
          mongo_connection$obj$insert(hs_df)
        },
        error = function(e) {
          shinyalert("Oops!", e$message, type = "error")
        },
        finally = {
          output$db_message2 <- renderText({
            paste0("Data loaded into database at ", Sys.time())
          })
        }
      )
    }
  })
})


output$project_select_for_database <- renderUI({
  pj_all <- dbstats$projects$project
  selectInput("project_selector_for_database", "Choose target", choices = pj_all)
})


# load spectra from database on click of button
observeEvent(input$load_from_db, {
  req(mongo_connection$obj)
  withBusyIndicatorServer("load_from_db", {
    if (input$project_selector_for_database == "") {
      shinyalert("Oops!", "Please connect to database first.", type = "error")
      return()
    } else {
      prj <- input$project_selector_for_database
      tryCatch(
        {
          cells <- mongo_connection$obj$find(paste0('{"project": "', prj, '", "dtype": "raw"}'))
        },
        error = function(e) {
          shinyalert("Oops!", e$message, type = "error")
        },
        finally = {
          output$db_message3 <- renderText({
            paste0("Data loaded from database at ", Sys.time())
          })
        }
      )
    }
  })
})

