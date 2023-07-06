ml_trim_num <- reactiveVal(1)
ml_trim_min1 <- reactiveVal(0)
ml_trim_max1 <- reactiveVal(4000)
ml_trim_min2 <- reactiveVal(0)
ml_trim_max2 <- reactiveVal(4000)
ml_trim_min3 <- reactiveVal(0)
ml_trim_max3 <- reactiveVal(4000)
ml_trim_min4 <- reactiveVal(0)
ml_trim_max4 <- reactiveVal(4000)
ml_trim_min5 <- reactiveVal(0)
ml_trim_max5 <- reactiveVal(4000)

output$hs_select_for_ml_prepare <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("cdr" %in% hs_all) {
    selected <- "cdr"
  } else if ("snr" %in% hs_all) {
    selected <- "snr"
  } else if ("smoothed" %in% hs_all) {
    selected <- "smoothed"
  }
  selectInput("hs_selector_for_ml_prepare", "Choose target", choices = hs_all, selected = selected)
})

observeEvent(ml_trim_num(), {
  output$ml_trim_multi <- renderUI({
    inputs <- lapply(1:ml_trim_num(), function(i) {
      fluidRow(
        column(2, numericInput(inputId = paste0("ml_trim_min", i), "Min", min = 0, max = 4000, step = 1, value = eval(parse(text = paste0("ml_trim_min", i, "()"))), width = "100%")),
        column(8,
               sliderInput(inputId = paste0("ml_trim_range", i), label = paste0("Selecting Wavelength Ranges: ", i), 
                           min = 0, max = 4000, value = c(eval(parse(text = paste0("ml_trim_min", i, "()"))), eval(parse(text = paste0("ml_trim_max", i, "()")))), step = 1, dragRange = F, width = "100%")),
        column(2, numericInput(inputId = paste0("ml_trim_max", i), "Max", min = 0, max = 4000, step = 1, value = eval(parse(text = paste0("ml_trim_max", i, "()"))), width = "100%"))
      )
    })
    # tagList(inputs)
  })
})

# reactivate trim range number and limit in 1-5
observeEvent(input$ml_plusButton, {
  if(ml_trim_num() < 5) {ml_trim_num(ml_trim_num() + 1)}
})
observeEvent(input$ml_minusButton, {
  if(ml_trim_num() > 1) {ml_trim_num(ml_trim_num() - 1)}
})

# convert ml_trim_range and ml_trim_min/max
lapply(1:5, function(i) {
  observeEvent(c(input[[paste0("ml_trim_min", i)]], input[[paste0("ml_trim_max", i)]]), {
    updateSliderInput(session, paste0("ml_trim_range", i), value = c(input[[paste0("ml_trim_min", i)]], input[[paste0("ml_trim_max", i)]]))
    eval(parse(text = paste0("ml_trim_min", i, "(", input[[paste0("ml_trim_min", i)]], ")")))
    eval(parse(text = paste0("ml_trim_max", i, "(", input[[paste0("ml_trim_max", i)]], ")")))
  })
  observeEvent(c(input[[paste0("ml_trim_range", i)]][1], input[[paste0("ml_trim_range", i)]][2]), {
    updateNumericInput(session, paste0("ml_trim_min", i), value = input[[paste0("ml_trim_range", i)]][1])
    updateNumericInput(session, paste0("ml_trim_max", i), value = input[[paste0("ml_trim_range", i)]][2])
    eval(parse(text = paste0("ml_trim_min", i, "(", input[[paste0("ml_trim_min", i)]], ")")))
    eval(parse(text = paste0("ml_trim_max", i, "(", input[[paste0("ml_trim_max", i)]], ")")))
  })
})

# prepare training datasets for scrs on click of button
observeEvent(input$prepare, {
  withBusyIndicatorServer("prepare", {
    if (isolate(input$hs_selector_for_ml_prepare) == "") {
      shinyalert("Oops!", "Please first load your spectra data.", type = "error")
      return()
    } else {
      hs_cur <- hs$val[[isolate(input$hs_selector_for_ml_prepare)]]
      if (isolate(input$ml_trim)) {
        text_range <- NULL
        for (i in 1:isolate(ml_trim_num())) {
          min_cur <- isolate(input[[paste0("ml_trim_range", i)]])[1]
          max_cur <- isolate(input[[paste0("ml_trim_range", i)]])[2]
          text_cur <- paste0(min_cur, "~", max_cur)
          text_range <- ifelse(is.null(text_range), text_cur, paste(text_range, text_cur, sep=", "))
        }
        text_range <- paste0("c(", text_range, ")")
        hs_cur <- hs_cur[, , eval(parse(text = text_range))]
      }
      # randomly split
      total <- nrow(hs_cur)
      size <- floor(isolate(input$train_pct) / 100.0 * total)
      tindex <- isample(hs_cur)
      index <- tindex[1:max(size, 2)]
      hs$val[["train"]] <- hs_cur[index]
      hs$val[["eval"]] <- hs_cur[-index]
    }
  })
})

observeEvent(hs$val[["train"]],
  {
    hs_train <- hs$val[["train"]]
    output$after_prepare <- renderDataTable({
      DT::datatable(if (is.null(hs_train)) NULL else hs_train@data %>% select(!matches("spc")),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$after_prepare_rows_selected,
  {
    output$after_prepare_plot <- renderPlotly({
      validate(need(input$after_prepare_rows_selected, ""))
      index <- input$after_prepare_rows_selected
      item <- hs$val[["train"]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)
