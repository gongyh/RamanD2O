output$hs_select_for_export <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("normalized" %in% hs_all) {
    selected <- "normalized"
  } else if ("baselined" %in% hs_all) {
    selected <- "baselined"
  }
  selectInput("hs_selector_for_export", "Choose target", choices = hs_all, selected = selected)
})


output$download <- downloadHandler(
  filename = function() {
    name <- paste0("spectra-", isolate(input$hs_selector_for_export), "-", format(Sys.time(), "%Y%m%d%H%M%S"))
    suffix <- ".csv"
    if (isolate(input$select_type) == "csv") {
      suffix <- ".csv"
    } else if (isolate(input$select_type) == "zip") {
      suffix <- ".zip"
    }
    paste0(name, suffix)
  },
  content = function(file) {
    data <- hs$val[[isolate(input$hs_selector_for_export)]]
    if (isolate(input$select_type == "csv")) {
      write.csv(data@data, file, quote=F, row.names=F)
    } else if (isolate(input$select_type == "zip")) {
      setwd(tempdir())
      zip_dir <- isolate(input$hs_selector_for_export)
      meta <- data@data
      meta$spc <- NULL
      write.table(meta, "meta.txt", row.names = F, col.names = T, quote = F, sep = "\t")
      if (!dir.exists(zip_dir)) dir.create(zip_dir)
      print("Preparing Raman Spectra files.")
      for (i in (1:nrow(data))) {
        cell <- data[i]
        txtdf <- data.frame(shift = cell@wavelength, intensity = t(cell$spc))
        txtname <- file.path(zip_dir, paste0(cell$ID_Cell, ".txt"))
        write.table(txtdf, txtname, row.names = F, col.names = F, quote = F, sep = "\t")
      }
      print("Done!")
      zip::zip(zipfile = file, c(zip_dir, "meta.txt"))
    }
  }
)

observeEvent(input$hs_selector_for_export,
  {
    hs_cur <- NULL
    if (!is.null(input$hs_selector_for_export)) {
      hs_cur <- hs$val[[input$hs_selector_for_export]]
    }

    output$visualize_table <- renderDataTable({
      DT::datatable(if (is.null(hs_cur)) NULL else hs_cur@data %>% dplyr::select(!matches("spc")),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })

    output$visualize_aggBy <- renderUI({
      metacols <- c("")
      if (!is.null(hs_cur)) {
        metacols <- colnames(hs_cur)
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("select_aggBy", "Aggregate by", choices = metacols, selected = F)
    })

    output$visualize_x <- renderUI({
      metacols <- c(".wavelength")
      if (!is.null(hs_cur)) {
        metacols <- c(metacols, colnames(hs_cur))
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("selectx", "X", choices = metacols, selected = ".wavelength")
    })

    output$visualize_y <- renderUI({
      metacols <- c("spc")
      if (!is.null(hs_cur)) {
        metacols <- colnames(hs_cur)
        metacols <- metacols[metacols != "spc"]
        metacols <- c("spc", metacols)
      }
      selectInput("selecty", "Y", choices = metacols, selected = "spc")
    })

    output$visualize_sgroup <- renderUI({
      metacols <- c("")
      if (!is.null(hs_cur)) {
        metacols <- colnames(hs_cur)
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("select_sgroup", "Group", choices = metacols, selected = F)
    })

    output$visualize_scolor <- renderUI({
      metacols <- c("")
      if (!is.null(hs_cur)) {
        metacols <- colnames(hs_cur)
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("select_scolor", "Color", choices = metacols, selected = F)
    })

    output$visualize_facet <- renderUI({
      metacols <- c("_")
      if (!is.null(hs_cur)) {
        metacols <- cbind(metacols, colnames(hs_cur))
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("select_facet", "Facet by", choices = metacols, selected = "")
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$visualize_table_rows_selected,
  {
    output$after_visualize_plot <- renderPlotly({
      validate(need(isolate(input$hs_selector_for_export), ""))
      validate(need(input$visualize_table_rows_selected, ""))
      index <- input$visualize_table_rows_selected
      item <- hs$val[[isolate(input$hs_selector_for_export)]][index]
      p <- qplotspc(item) + xlab(TeX("\\Delta \\tilde{\\nu }/c{{m}^{-1}}")) + ylab("I / a.u.")
      ggplotly(p) %>% config(mathjax = "cdn")
    })
  },
  ignoreNULL = FALSE
)


# plot all on click of button
observeEvent(input$plot_all,
  {
    withBusyIndicatorServer("plot_all", {
      type <- isolate(input$select_ptype)
      output$simple_plot <- renderPlot({
        validate(need(isolate(input$hs_selector_for_export), ""))
        hs_cur <- hs$val[[isolate(input$hs_selector_for_export)]]
        plot(hs_cur, type)
      })
    })
  },
  ignoreNULL = FALSE
)

# plot agg on click of button
observeEvent(input$plot_agg,
  {
    withBusyIndicatorServer("plot_agg", {
      output$agg_plot <- renderPlot({
        validate(need(isolate(input$hs_selector_for_export), ""))
        req(isolate(input$select_aggBy), cancelOutput = T)
        aggby <- isolate(input$select_aggBy)
        hs_cur <- hs$val[[isolate(input$hs_selector_for_export)]]
        means <- aggregate(hs_cur, by = hs_cur@data[, aggby], mean_pm_sd)
        if (any(is.na(means$spc))) {
          toastr_error("This data type does not require aggregation!", position = "top-center")
          return()
        } else {
          if (length(levels(hs_cur@data[, aggby])) <= 8) {
            plot(means, stacked = ".aggregate", fill = ".aggregate", axis.args = list(las = 1))
          } else {
            plot(means, stacked = ".aggregate", axis.args = list(las = 1))
          }

        }
      })
    })
  },
  ignoreNULL = TRUE
)

# plot compare on click of button
observeEvent(input$plot_compare,
  {
    withBusyIndicatorServer("plot_compare", {
      output$groupCmp_plot <- renderPlotly({
        validate(need(isolate(input$hs_selector_for_export), ""))
        req(isolate(input$selectx), cancelOutput = T)
        req(isolate(input$selecty), cancelOutput = T)
        hs_cur <- hs$val[[isolate(input$hs_selector_for_export)]]
        x <- isolate(input$selectx)
        y <- isolate(input$selecty)
        req(isolate(input$select_sgroup), cancelOutput = T)
        req(isolate(input$select_scolor), cancelOutput = T)
        g <- isolate(input$select_sgroup)
        c <- isolate(input$select_scolor)
        df <- as.long.df(hs_cur)
        p <- ggplot(df, aes_string(x = x, y = y, group = g, color = c))
        if (isolate(input$stype) == "Lineplot") {
          p <- p + geom_line()
        } else if (isolate(input$stype) == "Boxplot") {
          p <- p + geom_boxplot()
        }
        if (!is.null(isolate(input$select_facet)) && (isolate(input$select_facet) != "_")) {
          p <- p + facet_wrap(isolate(input$select_facet))
        }
        ggplotly(p)
      })
    })
  },
  ignoreNULL = TRUE
)
