# connect mongodb on click of button
observeEvent(input$connectdb, {
  withBusyIndicatorServer("connectdb", {
    tryCatch({
      mongo_connection$obj <- mongo(collection = input$collection, db = input$db, url = input$url)
    }, error = function(e) {
      shinyalert("Oops!", e$message, type = "error")
    })

  })
})
