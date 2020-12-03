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
    name <- paste0("spectra-", input$hs_selector_for_export, "-", format(Sys.time(), "%Y%m%d%H%M%S"))
    suffix <- ".csv"
    if (input$select_type == "csv") {
      suffix <- ".csv"
    } else if (input$select_type == "zip") {
      suffix <- ".zip"
    }
    paste0(name, suffix)
  },
  content = function(file) {
    data <- hs$val[[input$hs_selector_for_export]]
    if (input$select_type == "csv") {
      write.csv(data, file)
    } else if (input$select_type == "zip") {
      setwd(tempdir())
      zip_dir <- input$hs_selector_for_export
      meta <- data@data
      meta$spc <- NULL
      write.table(meta, "meta.txt", row.names = F, col.names = T, quote = F, sep = "\t")
      if (!dir.exists(zip_dir)) dir.create(zip_dir)
      files <- c()
      for (i in (1:nrow(data))) {
        cell <- data[i]
        txtdf <- data.frame(shift = cell@wavelength, intensity = t(cell$spc))
        txtname <- file.path(zip_dir, paste0(cell$ID_Cell, ".txt"))
        write.table(txtdf, txtname, row.names = F, col.names = F, quote = F, sep = "\t")
      }
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
      DT::datatable(if (is.null(hs_cur)) NULL else hs_cur@data %>% select(!matches("spc")),
        escape = FALSE, selection = "single", extensions = list("Responsive", "Scroller"),
        options = list(searchHighlight = TRUE, scrollX = TRUE)
      )
    })
    output$visualize_pcaColBy <- renderUI({
      metacols <- c("cluster_name")
      if (!is.null(hs_cur)) {
        metacols <- c(metacols, colnames(hs_cur))
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("select_pcaColBy", "Color by", choices = metacols, selected = "cluster_name")
    })
    output$visualize_aggBy <- renderUI({
      metacols <- c(" ")
      if (!is.null(hs_cur)) {
        metacols <- c(metacols, colnames(hs_cur))
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("select_aggBy", "Aggregate by", choices = metacols)
    })
  },
  ignoreNULL = FALSE
)

observeEvent(input$visualize_table_rows_selected,
  {
    output$after_visualize_plot <- renderPlotly({
      validate(need(input$hs_selector_for_export, ""))
      validate(need(input$visualize_table_rows_selected, ""))
      index <- input$visualize_table_rows_selected
      item <- hs$val[[input$hs_selector_for_export]][index]
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
      type <- input$select_ptype
      output$simple_plot <- renderPlot({
        validate(need(input$hs_selector_for_export, ""))
        hs_cur <- hs$val[[input$hs_selector_for_export]]
        plot(hs_cur, type)
      })
    })
  },
  ignoreNULL = FALSE
)


# plot pca on click of button
observeEvent(input$plot_pca,
  {
    withBusyIndicatorServer("plot_pca", {
      nclusters <- input$num_clusters
      colby <- input$select_pcaColBy
      output$pca_plot <- renderPlotly({
        validate(need(input$hs_selector_for_export, ""))
        hs_cur <- hs$val[[input$hs_selector_for_export]]
        pca <- prcomp(~spc, data = hs_cur, center = FALSE)
        scores <- pca$x
        rownames(scores) <- rownames(hs_cur$spc)
        HC <- hclust(dist(scores), method = "ward.D2")
        Clusters <- cutree(HC, k = nclusters)
        Df <- data.frame(scores, "cluster" = factor(Clusters))
        Df <- cbind(hs_cur@data %>% select(!matches("spc")), Df)
        Df <- transform(Df, cluster_name = paste("Cluster", Clusters))
        plot_ly(Df,
          x = ~PC1, y = ~PC2, text = rownames(Df), type = "scatter", symbol = ~cluster_name,
          mode = "markers", color = Df[, colby], marker = list(size = 11)
        )
      })
    })
  },
  ignoreNULL = FALSE
)


# plot agg on click of button
observeEvent(input$plot_agg,
  {
    withBusyIndicatorServer("plot_agg", {
      validate(need(input$select_aggBy, ""))
      aggby <- input$select_aggBy
      output$agg_plot <- renderPlot({
        validate(need(input$hs_selector_for_export, ""))
        hs_cur <- hs$val[[input$hs_selector_for_export]]
        means <- aggregate(hs_cur, by = hs_cur@data[, aggby], mean_pm_sd)
        plot(means, stacked = ".aggregate", fill = ".aggregate", axis.args = list(las = 1))
      })
    })
  },
  ignoreNULL = FALSE
)
