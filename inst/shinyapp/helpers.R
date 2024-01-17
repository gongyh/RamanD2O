# helper functions for RamanD2O shiny app

removeCosmic <- function(spc) {
  medianFilt <- runmed(spc, 3)
  diff <- spc - medianFilt
  cutoff <- max(2000, 8 * sd(diff))
  df <- data.frame(raw = spc, med = medianFilt, diff = diff)
  cosmic <- FALSE
  if (length(which(df$diff > cutoff)) > 0) {
    cosmic <- TRUE
    df[which(df$diff > cutoff), "raw"] <- df[which(df$diff > cutoff), "med"]
  }
  return(list("spc" = df$raw, "cosmic" = cosmic))
}

# search peaks for each spectrum
# size: need to be odd number
findPeaks <- function(spc, size = 9, level = 0.1) {
  half <- (size - 1) / 2
  maxI <- max(spc)
  peaks <- c()
  for (i in (half + 1):(length(spc) - half)) {
    if ((spc[i] >= level * maxI) && (spc[i] >= max(spc[(i - half):(i + half)]))) {
      peaks <- c(peaks, i)
    }
  }
  peaks
}

mean_sd_filter <- function(x, n = 5) {
  x <- x - mean(x)
  s <- n * sd(x)
  (x <= s) & (x > -s)
}

round2 <- function(x) {
  round(x, digits = 2)
}

round4 <- function(x) {
  round(x, digits = 4)
}

unAsIs <- function(X) {
  if ("AsIs" %in% class(X)) {
    class(X) <- class(X)[-match("AsIs", class(X))]
  }
  X
}

withBusyIndicatorCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
"

withBusyIndicatorUI <- function(button) {
  id <- button[["attribs"]][["id"]]
  div(
    shinyjs::useShinyjs(),
    singleton(tags$head(
      tags$style(withBusyIndicatorCSS)
    )),
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(
        class = "btn-err",
        div(
          icon("exclamation-circle"),
          tags$b("Error: "),
          span(class = "btn-err-msg")
        )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })

  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch(
    {
      value <- expr
      shinyjs::show(selector = doneEl)
      shinyjs::delay(2000, shinyjs::hide(
        selector = doneEl, anim = TRUE, animType = "fade",
        time = 0.5
      ))
      value
    },
    error = function(err) {
      errorFunc(err, buttonId)
    }
  )
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

# plot_o2m for raw little matrix result plot
plot_o2m <- function (x, loading_name = c("Xjoint", "Yjoint", "gr_Xjoint", "gr_Yjoint", "Xorth", "Yorth"), i = 1, j = NULL, use_ggplot2=TRUE, label = c("number", "colnames"), ...)
{
  stopifnot(i == round(i), is.logical(use_ggplot2))

  matrix_number <- sum(!grepl("_", row.names(ig[[ig$cur_result]]$W.)))
  fit <- list()
  if(length(label) > 2) stop("label should be a single string, 'number' or 'colnames'")
  loading_name = match.arg(loading_name)
  if((loading_name %in% c("gr_Xjoint", "gr_Yjoint")) & x$flags$method != "GO2PLS") stop("Loading plots at group level only available in GO2PLS")

  which_load = switch(loading_name, Xjoint = "W.", Yjoint = "C.",
                      gr_Xjoint = "W_gr", gr_Yjoint = "C_gr", Xorth = "P_Yosc.", Yorth = "P_Xosc.")
  # fit$load = as.matrix(x[which_load][[1]][1:matrix_number,])
  fit$load <- if(which_load == "W.") as.matrix(x[which_load][[1]][1:matrix_number,]) else as.matrix(x[which_load][[1]])
  if(ncol(fit$load) < max(i,j) )
    stop("i and j cannot exceed #components = ",ncol(fit$load))
  fit$load = fit$load[,c(i,j)]

  p = nrow(as.matrix(fit$load))
  if(is.null(j)){
    fit$load = cbind(1:p,fit$load)
    colnames(fit$load) = c("index",paste(loading_name,"loadings",i))
  }else{
    stopifnot(j == round(j))
    colnames(fit$load) = c(paste(loading_name,"loadings",i),paste(loading_name,"loadings",j))
  }

  label2 = match.arg(label)
  if(label2 == "colnames" && !is.null(rownames(x[which_load][[1]]))) {
    label = rownames(x[which_load][[1]])
  } else label = 1:p
  if(label2 == "colnames" && is.null(rownames(x[which_load][[1]]))) message("No labels found in colnames, proceeding...","\n")

  if (use_ggplot2) {
    plt = with(fit, {
      ggplot(data.frame(x = load[, 1], y = load[, 2]), aes(x = x, y = y, label = I(label))) +
        geom_text(...) +
        labs(x = colnames(load)[1], y = colnames(load)[2])
    })
    plt = plt + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
    #print(plt)
    return(plt)
  }
  else {
    with(fit, {
      plot(load[, 1], load[, 2], type = "n")
      text(load[, 1], load[, 2])
    })
    abline(v=0,h=0)
  }
}
