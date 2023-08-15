output$hs_select_for_statistics <- renderUI({
  hs_all <- names(hs$val)
  selected <- NULL
  if ("normalized" %in% hs_all) {
    selected <- "normalized"
  } else if ("baselined" %in% hs_all) {
    selected <- "baselined"
  }
  selectInput("hs_selector_for_statistics", "Choose target", choices = hs_all, selected = selected)
})


observeEvent(input$hs_selector_for_statistics,
  {
    hs_cur <- NULL
    if (!is.null(input$hs_selector_for_statistics)) {
      hs_cur <- hs$val[[input$hs_selector_for_statistics]]
    }

    output$visualize_pcaColBy <- renderUI({
      metacols <- c("cluster_name")
      if (!is.null(hs_cur)) {
        metacols <- c(metacols, colnames(hs_cur))
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("select_pcaColBy", "Color by", choices = metacols, selected = "cluster_name")
    })

  },
  ignoreNULL = FALSE
)

# plot pca on click of button
observeEvent(input$plot_pca,
  {
    withBusyIndicatorServer("plot_pca", {
      output$pca_plot <- renderPlotly({
        validate(need(isolate(input$hs_selector_for_statistics), ""))
        req(isolate(input$select_pcaColBy), cancelOutput = T)
        nclusters <- isolate(input$num_clusters)
        colby <- isolate(input$select_pcaColBy)
        hs_cur <- hs$val[[isolate(input$hs_selector_for_statistics)]]
        pca <- prcomp(~spc, data = hs_cur@data, center = FALSE)
        scores <- pca$x
        rownames(scores) <- rownames(hs_cur$spc)
        HC <- hclust(dist(scores), method = "ward.D2")
        Clusters <- cutree(HC, k = nclusters)
        Df <- data.frame(scores, "cluster" = factor(Clusters))
        Df <- cbind(hs_cur@data %>% select(!matches("spc")), Df)
        Df <- transform(Df, cluster_name = paste("Cluster", Clusters))
        rownames(Df) <- rownames(scores)
        plot_ly(Df,
          x = ~PC1, y = ~PC2, text = rownames(Df), type = "scatter", symbol = ~cluster_name,
          mode = "markers", color = Df[, colby], marker = list(size = 11)
        )
      })
    })
  },
  ignoreNULL = TRUE
)
