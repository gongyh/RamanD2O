output$hs_select_for_smooth <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("filtered" %in% hs_all) {
    selected <- "filtered"
  } else if ("sampled" %in% hs_all) {
    selected <- "sampled"
  }
  selectInput("hs_selector_for_smooth", "Choose target",
              choices = hs_all, selected = selected)
})

# smooth scrs on click of button
observeEvent(input$smooth, {
  withBusyIndicatorServer("smooth", {
    if (isolate(input$hs_selector_for_smooth) == "") {
      shinyalert("Oops!",
                 "Please first load your spectra data.", type = "error")
      return()
    } else {
      show_modal_spinner(spin = "flower",
                         color = "red", text = "Processing ....")
      hs_cur <- hs$val[[isolate(input$hs_selector_for_smooth)]]
      wavelength <- wl(hs_cur)
      hs_sm <- hs_cur
      smooth_method <- isolate(input$select_smooth)
      if (smooth_method == "LOESS") {
        if (isolate(input$interp)) {
          minWl <- round(min(wavelength)) + 1
          maxWl <- round(max(wavelength)) - 1
          wavelength <- minWl:maxWl
        }
        hs_sm <- spc.loess(hs_cur, wavelength, normalize = FALSE)
      } else if (smooth_method == "SG") {
        p <- isolate(input$sg_order)
        n <- isolate(input$sg_length)
        spc_sg <- apply(hs_cur$spc, 1, function(x) {
          signal::sgolayfilt(x, p = p, n = n)
        })
        hs_sm$spc <- t(spc_sg)
      } else if (smooth_method == "HVD") {
        fp <- isolate(input$hvd_cutf)
        spc_hvd <- apply(hs_cur$spc, 1, function(x) {
          hvd_x <- hvd(x, 1, fp)
          hvd_x$Y[, 1]
        })
        hs_sm$spc <- t(spc_hvd)
      } else if (smooth_method == "EEMD") {
        cv.level <- isolate(input$emd_cv_level)
        cv.kfold <- isolate(input$emd_cv_kfold)
        cv.index <- cvtype(n = length(wavelength),
                           cv.kfold = cv.kfold,
                           cv.random = FALSE)$cv.index
        spc_i <- 1
        spc_emd <- apply(hs_cur$spc, 1, function(x) {
          update_modal_spinner(paste0("Processing ", spc_i, "/", nrow(hs_cur)))
          res <- eemddenoise(x,
                             cv.index = cv.index,
                             cv.level = cv.level,
                             by.imf = TRUE)
          spc_i <- spc_i + 1
          res$dxt
        })
        hs_sm$spc <- t(spc_emd)
      }
      colnames(hs_sm$spc) <- hs_sm@wavelength
      hs$val[["smoothed"]] <- hs_sm
      remove_modal_spinner()
    }
  })
})

observeEvent(hs$val[["smoothed"]],
  {
    hs_sm <- hs$val[["smoothed"]]
    output$smoothed_table <- renderDataTable({
      DT::datatable(
        if (is.null(hs_sm)) NULL else hs_sm@data %>%
          dplyr::select(!matches("spc")),
        escape = FALSE, selection = "single",
        extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$smoothed_table_rows_selected,
  {
    output$after_smooth_plot <- renderPlotly({
      validate(need(input$smoothed_table_rows_selected, ""))
      index <- input$smoothed_table_rows_selected
      item <- hs$val[["smoothed"]][index]
      p <- qplotspc(item) +
        xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) +
        ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
