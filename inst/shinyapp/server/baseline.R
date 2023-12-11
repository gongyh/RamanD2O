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
changing_order <- reactiveVal(1)
for (i in 1:5) {
  assign(paste0("polyfit_custom_min",i), reactiveVal(1))
  assign(paste0("polyfit_custom_max",i), reactiveVal(4000))
  assign(paste0("polyfit_custom_line",i), reactiveVal(1))
  assign(paste0("polyfit_custom_order",i), reactiveVal(1))
}

observeEvent(polyfit_custom_num(), {
  output$polyfit_custom_multi <- renderUI({
    inputs <- lapply(1:polyfit_custom_num(), function(i) {
      fluidRow(
        column(2, numericInput(inputId = paste0("polyfit_custom_min", i), "Min",
          min = 0, max = 4000, step = 1, value = isolate(eval(parse(text = paste0("polyfit_custom_min", i, "()")))), width = "100%")),
        column(4, sliderInput(inputId = paste0("polyfit_custom_range", i), label = paste0("Selecting Wavelength Ranges: ", i),
          min = 0, max = 4000, value = c(isolate(eval(parse(text = paste0("polyfit_custom_min", i, "()")))), isolate(eval(parse(text = paste0("polyfit_custom_max", i, "()"))))), step = 1, dragRange = F, width = "100%")),
        column(2, numericInput(inputId = paste0("polyfit_custom_max", i), "Max",
          min = 0, max = 4000, step = 1, value = isolate(eval(parse(text = paste0("polyfit_custom_max", i, "()")))), width = "100%")),
        column(2, numericInput(inputId = paste0("polyfit_custom_line", i), "Line",
          value = isolate(eval(parse(text = paste0("polyfit_custom_line", i, "()")))), min = 1, max = 5, step = 1)),
        column(2, numericInput(inputId = paste0("polyfit_custom_order", i), "Order",
          value = isolate(eval(parse(text = paste0("polyfit_custom_order", i, "()")))), min = 1, max = 10, step = 1))
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

# convert polyfit_custom_range and polyfit_custom_min/max
lapply(1:5, function(i) {
  observeEvent(c(input[[paste0("polyfit_custom_min", i)]], input[[paste0("polyfit_custom_max", i)]]), {
    if (all(is.numeric(c(input[[paste0("polyfit_custom_min", i)]], input[[paste0("polyfit_custom_max", i)]]))))  {
      updateSliderInput(session, paste0("polyfit_custom_range", i), value = c(input[[paste0("polyfit_custom_min", i)]], input[[paste0("polyfit_custom_max", i)]]))
    }
    eval(parse(text = paste0("polyfit_custom_min", i, "(", input[[paste0("polyfit_custom_min", i)]], ")")))
    eval(parse(text = paste0("polyfit_custom_max", i, "(", input[[paste0("polyfit_custom_max", i)]], ")")))
  })
  observeEvent(c(input[[paste0("polyfit_custom_range", i)]][1], input[[paste0("polyfit_custom_range", i)]][2]), {
    updateNumericInput(session, paste0("polyfit_custom_min", i), value = input[[paste0("polyfit_custom_range", i)]][1])
    updateNumericInput(session, paste0("polyfit_custom_max", i), value = input[[paste0("polyfit_custom_range", i)]][2])
    eval(parse(text = paste0("polyfit_custom_min", i, "(", input[[paste0("polyfit_custom_min", i)]], ")")))
    eval(parse(text = paste0("polyfit_custom_max", i, "(", input[[paste0("polyfit_custom_max", i)]], ")")))
  })
  observeEvent(input[[paste0("polyfit_custom_line", i)]], {
    eval(parse(text = paste0("polyfit_custom_line", i, "(", input[[paste0("polyfit_custom_line", i)]], ")")))
  })
  observeEvent(input[[paste0("polyfit_custom_order", i)]], {
    eval(parse(text = paste0("polyfit_custom_order", i, "(", input[[paste0("polyfit_custom_order", i)]], ")")))
    line_value <- isolate(input[[paste0("polyfit_custom_line", i)]])
    line_index_list <- which(sapply(1:polyfit_custom_num(), function(i) {isolate(input[[paste0("polyfit_custom_line", i)]])}) == line_value)
    eval(parse(text = paste0("changing_order", "(", input[[paste0("polyfit_custom_order", i)]], ")")))
    sapply(setdiff(line_index_list, i), function(j) {
      updateNumericInput(session, paste0("polyfit_custom_order", j),
        value = changing_order())
    })
  })
})

# baseline scrs on click of button
observeEvent(input$baseline, {
  withBusyIndicatorServer("baseline", {
    if (input$hs_selector_for_baseline == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      show_modal_spinner(spin = "flower", color = "red", text = "Processing ....")
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
        if (input$polyfit_order > 10 | input$polyfit_order < 1) {
          shinyalert("Oops!", "Order out of range(1-10).", type = "error")
          remove_modal_spinner()
          return()
        }
        order <- input$polyfit_order
        hs_bl <- hs_cur - spc_fit_poly_below(hs_cur, poly.order = input$polyfit_order)
        hs_bl$spc <- unAsIs(hs_bl$spc)
        dimnames(hs_bl$spc) <- dimnames(hs_cur$spc)
      } else if (input$select_baseline == "polyfit" & input$polyfit_custom == TRUE) {
        line_list <- sapply(1:polyfit_custom_num(), function(i) {input[[paste0("polyfit_custom_line", i)]]})
        order_list <- sapply(1:polyfit_custom_num(), function(i) {input[[paste0("polyfit_custom_order", i)]]})
        if (any(line_list > 5) | any(order_list > 10)) {
          shinyalert("Oops!", "Out of range. (Line: 1-5, Order: 1-10)", type = "error")
          remove_modal_spinner()
          return()
        }
        for (line in unique(line_list)) {
          line_index_list <- which(line_list == line)
          range <- sapply(line_index_list, function(i) {
            range_tmp <- input[[paste0("polyfit_custom_range", i)]]
            eval(parse(text = paste0(range_tmp[1],"~",range_tmp[2])))
          })
          range_value <- sapply(line_index_list, function(i) {input[[paste0("polyfit_custom_range", i)]]})
          range_max <- max(range_value)
          range_min <- min(range_value)
          if (length(hs_cur[,,range_min ~ range_max]@wavelength) == 0) {
            shinyalert("Oops!", paste0("Line",line," does not have a spectrum."), type = "error")
            remove_modal_spinner()
            return()
          }
          assign(paste0("hs_line",line), hs_cur[,,range_min ~ range_max] -
            spc_fit_poly_below(hs_cur[,,range], hs_cur[,,range_min ~ range_max], poly.order = input[[paste0("polyfit_custom_order", line_index_list[1])]]))
        }
        # avaid range duplication
        all_wl <- sapply(unique(line_list), function(i) {eval(parse(text = paste0("wl(hs_line", i,")")))})
        if (any(duplicated(unlist(c(all_wl))))) {
          shinyalert("Oops!", "The range is duplicated.", type = "error")
          remove_modal_spinner()
          return()
        }
        hs_line_all <- do.call(cbind, mget(paste0("hs_line", unique(line_list))))
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
