function(input, output, session) {

  # set user info
  if (user_name == "Anonymous" && !is.null(session$user)) {
    user_name <- session$user
  }
  output$user <- renderUser({
    dashboardUser(
      name = user_name,
      src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg"
    )
  })

  # print one dot every minute to prevent gray-out
  autoInvalidate <- reactiveTimer(60000)
  observe({
    autoInvalidate()
    cat(".")
  })

  source(file.path("server", "unzip_scrs.R"), local = TRUE)$value
  source(file.path("server", "load_meta.R"), local = TRUE)$value
  source(file.path("server", "subsample.R"), local = TRUE)$value
  source(file.path("server", "trim.R"), local = TRUE)$value
  source(file.path("server", "smooth.R"), local = TRUE)$value
  source(file.path("server", "baseline.R"), local = TRUE)$value
  source(file.path("server", "normalize.R"), local = TRUE)$value
  source(file.path("server", "visualize.R"), local = TRUE)$value
  source(file.path("server", "database.R"), local = TRUE)$value
  source(file.path("server", "snr.R"), local = TRUE)$value
}
