# connect mongodb on click of button
observeEvent(input$connectdb, {
  tryCatch(
    {
      mongo_connection$obj <- mongo(collection = isolate(input$collection),
                                    db = isolate(input$db),
                                    url = isolate(input$url))
      dbstats$projects <- mongo_connection$obj$aggregate(
        '[{"$group":{"_id":"$project", "count": {"$sum":1}}}]',
        options = '{"allowDiskUse":true}'
      )
      if (nrow(dbstats$projects) > 0)
        names(dbstats$projects) <- c("project", "count")
      output$db_message <- renderText({
        msg <- paste0("MongoDB is connected at ",
                      Sys.time(), "\n\nproject, spectra_count\n")
        if (nrow(dbstats$projects) > 0) {
          for (i in seq_len(nrow(dbstats$projects))) {
            msg <- paste(msg, toString(dbstats$projects[i, ]), "\n")
          }
        }
        msg
      })
      updateActionButton(session, "connectdb", label = "Refresh")
    },
    error = function(e) {
      shinyalert("Oops!", e$message, type = "error")
      output$db_message <- renderText({
        "Failed! To connect to your MongoDB database,
        please contact your database manager."
      })
    }
  )
})

output$hs_select_for_database <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("normalized" %in% hs_all) {
    selected <- "normalized"
  } else if ("raw" %in% hs_all) {
    selected <- "raw"
  }
  selectInput("hs_selector_for_database", "Choose target",
              choices = hs_all, selected = selected)
})

# save spectra to database on click of button
observeEvent(input$savedb, {
  req(mongo_connection$obj)
  withBusyIndicatorServer("savedb", {
    if (isolate(input$hs_selector_for_database) == "") {
      shinyalert("Oops!",
                 "Please first load your spectra data.", type = "error")
      return()
    } else {
      show_modal_spinner(
                         spin = "flower", color = "red",
                         text = "Pushing data ....")
      hs_cur <- hs$val[[isolate(input$hs_selector_for_database)]]
      hs_df <- as.wide.df(hs_cur, wl.prefix = "spc.")
      hs_df$dtype <- isolate(input$hs_selector_for_database)
      hs_df$project <- isolate(input$project)
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
      remove_modal_spinner()
    }
  })
})


output$project_select_for_database <- renderUI({
  pj_all <- dbstats$projects$project
  selectInput("project_selector_for_database",
              "Choose project", choices = pj_all)
})


# load spectra from database on click of button
observeEvent(input$load_from_db, {
  req(mongo_connection$obj)
  withBusyIndicatorServer("load_from_db", {
    if (isolate(input$project_selector_for_database) == "") {
      shinyalert("Oops!", "Please connect to database first.", type = "error")
      return()
    } else {
      show_modal_spinner(
                         spin = "flower", color = "red",
                         text = "Pulling data ....")
      prj <- isolate(input$project_selector_for_database)
      tryCatch(
        {
          cells <- mongo_connection$obj$find(
                                             paste0('{"project": "', prj,
                                                    '", "dtype": "raw"}'))
          dmeta <- cells %>% select(!starts_with("spc"))
          dmeta$dtype <- NULL
          dmeta$project <- NULL
          spc <- cells %>% select(starts_with("spc"))
          wavelength <- colnames(spc)
          wavelength <- str_replace_all(wavelength, "_", ".")
          wavelength <- str_remove(wavelength, "spc.")
          colnames(spc) <- wavelength
          hs_raw <- new("hyperSpec",
            data = dmeta, spc = as.matrix(spc),
            wavelength = as.numeric(wavelength)
          )
          hs$val[["raw"]] <- hs_raw
          scrs$spc <- spc
          meta$tbl <- dmeta
        },
        error = function(e) {
          shinyalert("Oops!", e$message, type = "error")
        },
        finally = {
          output$db_message3 <- renderText({
            if (is.null(hs_raw)) {
              paste0("spectra loading failed at ", Sys.time())
            } else {
              paste0(length(hs_raw),
                     " spectra loaded from database at ", Sys.time())
            }
          })
        }
      )
      remove_modal_spinner()
    }
  })
})
