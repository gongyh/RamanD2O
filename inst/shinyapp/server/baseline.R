output$hs_select_for_baseline <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("smoothed" %in% hs_all) {
    selected <- "smoothed"
  } else if ("filtered" %in% hs_all) {
    selected <- "filtered"
  }
  selectInput("hs_selector_for_baseline", "Choose target", choices = hs_all, selected = selected)
})

# polyfit_custom_multi_parameter
polyfit_custom_num <- reactiveVal(1)
for (i in 1:5) {
  assign(paste0("polyfit_custom_order",i), reactiveVal(1))
  assign(paste0("polyfit_custom_text",i), reactiveVal("1~1798, 1800 ~ 2065, 2300 ~ 2633, 2783, 3050~4000"))
}

observeEvent(polyfit_custom_num(), {
  output$polyfit_custom_multi <- renderUI({
    inputs <- lapply(1:polyfit_custom_num(), function(i) {
      fluidRow(
        column(8, textInput(inputId = paste0("polyfit_custom_text", i), label = paste0("Line ", i),
          value = isolate(eval(parse(text = paste0("polyfit_custom_text", i, "()")))))),
        column(2, numericInput(inputId = paste0("polyfit_custom_order", i), "Order",
          value = isolate(eval(parse(text = paste0("polyfit_custom_order", i, "()")))), min = 0, max = 15, step = 1))
      )
    })
  })
}, ignoreNULL = TRUE)

# reactivate polyfit custom range number and limit in 1-5
observeEvent(input$polyfit_custom_plus, {
  if(polyfit_custom_num() < 5) {polyfit_custom_num(polyfit_custom_num() + 1)}
})
observeEvent(input$polyfit_custom_minus, {
  if(polyfit_custom_num() > 1) {polyfit_custom_num(polyfit_custom_num() - 1)}
})

# dynamic convert polyfit_custom_order and polyfit_custom_text
lapply(1:5, function(i) {
  observeEvent(input[[paste0("polyfit_custom_order", i)]], {
    eval(parse(text = paste0("polyfit_custom_order", i, "(", input[[paste0("polyfit_custom_order", i)]], ")")))
  })
  observeEvent(input[[paste0("polyfit_custom_text", i)]], {
    eval(parse(text = paste0("polyfit_custom_text", i, "(\"", input[[paste0("polyfit_custom_text", i)]], "\")")))
  })
})

# baseline scrs on click of button
observeEvent(input$baseline, {
  withBusyIndicatorServer("baseline", {
    if (input$hs_selector_for_baseline == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      # show_modal_spinner(spin = "flower", color = "red", text = "Processing ....")
      hs_cur <- hs$val[[input$hs_selector_for_baseline]]
      wavelength <- wl(hs_cur)
      # baseline
      if (input$select_baseline == "als") {
        b_als <- baseline(hs_cur$spc, method = "als")
        data <- hs_cur@data
        data$spc <- NULL
        hs_bl <- new("hyperSpec",
          data = data,
          spc = getCorrected(b_als), wavelength = wavelength
        )
      } else if (input$select_baseline == "polyfit" & input$polyfit_custom == FALSE) {
        if (input$polyfit_order > 15 | input$polyfit_order < 0) {
          shinyalert("Oops!", "Order out of range(0-15).", type = "error")
          remove_modal_spinner()
          return()
        }
        order <- input$polyfit_order
        hs_bl <- hs_cur - spc_fit_poly_below(hs_cur, poly.order = input$polyfit_order)
        hs_bl$spc <- unAsIs(hs_bl$spc)
        dimnames(hs_bl$spc) <- dimnames(hs_cur$spc)
      } else if (input$select_baseline == "polyfit" & input$polyfit_custom == TRUE) {

        order_list <- sapply(1:polyfit_custom_num(), function(i) {input[[paste0("polyfit_custom_order", i)]]})
        if (any(order_list > 15)) {
          shinyalert("Oops!", "Order out of range(0-15).", type = "error")
          remove_modal_spinner()
          return()
        }
        for (line in 1:polyfit_custom_num()) {
          # line_index_list <- which(line_list == line)
          range <- input[[paste0("polyfit_custom_text", line)]]
          if (!grepl("^[0-9~ ,]+$", range)) {
            shinyalert("Oops!", "Invalid text input.", type = "error")
            remove_modal_spinner()
            return()
          }
          range_value <- paste0("c(",gsub("~", ":", range),")")
          range_max <- max(eval(parse(text = range_value)))
          range_min <- min(eval(parse(text = range_value)))
          if (length(hs_cur[,,range_min ~ range_max]@wavelength) == 0) {
            shinyalert("Oops!", paste0("Line",line," does not have a spectrum."), type = "error")
            remove_modal_spinner()
            return()
          }
          range <- eval(parse(text = paste0("c(", range,")")))
          # avoid range duplication in same line
          if (any(duplicated(wl(hs_cur[,,range])))) {
            shinyalert("Oops!", "The range is duplicated.", type = "error")
            remove_modal_spinner()
            return()
          }
          assign(paste0("hs_line",line), hs_cur[,,range_min ~ range_max] -
            spc_fit_poly(hs_cur[,,range], hs_cur[,,range_min ~ range_max], poly.order = input[[paste0("polyfit_custom_order", line)]]))
        }
        # avaid range duplication in different lines
        hs_line_all <- do.call(cbind, mget(paste0("hs_line", 1:polyfit_custom_num())))
        all_wl <- wl(hs_line_all)
        if (any(duplicated(unlist(c(all_wl))))) {
          shinyalert("Oops!", "The range is duplicated.", type = "error")
          remove_modal_spinner()
          return()
        }
        diff <- setdiff(wl(hs_cur),wl(hs_line_all))
        unique <- hs_cur[,,as.numeric(diff), drop = FALSE]
        hs_bl <- wl_sort(cbind(unique, hs_line_all))
      } else {
        shinyalert("Oops!", "Baseline method not implemented yet.", type = "error")
        remove_modal_spinner()
        return()
      }
      # handle negative
      if (input$select_negative == "zero") {
        hs_bl_spc <- hs_bl$spc
        hs_bl_spc[hs_bl_spc < 0] <- 0
        hs_bl$spc <- hs_bl_spc
      } else if (input$select_negative == "up") {
        offsets <- apply(hs_bl, 1, min)
        hs_bl <- sweep(hs_bl, 1, offsets, "-")
      } else if (input$select_negative == "keep") {
        # need to do nothing
      } else {
        # treat as keep
      }
      hs$val[["baselined"]] <- hs_bl
      remove_modal_spinner()
    }
  })
})

observeEvent(hs$val[["baselined"]],
  {
    hs_bl <- hs$val[["baselined"]]
    output$baselined_table <- renderDataTable({
      DT::datatable(if (is.null(hs_bl)) NULL else hs_bl@data %>% dplyr::select(!matches("spc")),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$baselined_table_rows_selected,
  {
    output$after_baseline_plot <- renderPlotly({
      validate(need(input$baselined_table_rows_selected, ""))
      index <- input$baselined_table_rows_selected
      item <- hs$val[["baselined"]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
