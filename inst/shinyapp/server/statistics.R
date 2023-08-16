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

    output$statistics_ldaBy <- renderUI({
      metacols <- c("")
      if (!is.null(hs_cur)) {
        metacols <- c(metacols, colnames(hs_cur))
        metacols <- metacols[metacols != "spc"]
      }
      selectInput("select_ldaBy", "Group by", choices = metacols, selected = "")
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


# perform LDA on click of button
observeEvent(input$perform_lda,
             {
               withBusyIndicatorServer("perform_lda", {
                 output$lda_plot <- renderPlotly({
                   validate(need(isolate(input$hs_selector_for_statistics), ""))
                   req(isolate(input$select_ldaBy), cancelOutput = T)
                   num_pcs <- isolate(input$num_pcs)
                   ldaby <- isolate(input$select_ldaBy)
                   hs_cur <- hs$val[[isolate(input$hs_selector_for_statistics)]]
                   pca_first <- isolate(input$use_pca)
                   eval_pct <- isolate(input$lda_eval_pct)/100.0
                   data <- hs_cur@data[ldaby]
                   colnames(data) <- 'group'
                   if (pca_first) {
                       pca <- prcomp(~spc, data = hs_cur@data, center = FALSE)
                       scores <- pca$x[,c("PC1","PC2")]
                       rownames(scores) <- rownames(hs_cur$spc)
                       data <- cbind(data, scores)
                   } else {
                       data <- cbind(data, hs_cur$spc)
                   }

                   ind <- sample(2, nrow(data), replace = T, prob = c(1-eval_pct, eval_pct))
                   training <- data[ind==1,]
                   testing <- data[ind==2,]

                   linear <- lda(group~., training)

                   p1 <- predict(linear, training)$class
                   tab1 <- table(Predicted = p1, Actual = training$group)
                   acc_train <- sum(diag(tab1))/sum(tab1)

                   p2 <- predict(linear, testing)$class
                   tab2 <- table(Predicted = p2, Actual = testing$group)
                   acc_test <- sum(diag(tab2))/sum(tab2)

                   p <- ggord(linear, training$group) +
                     labs(title=sprintf("training accuracy is %.2f, testing accuracy is %.2f", acc_train, acc_test))

                   ggplotly(p)

                 })
               })
             },
             ignoreNULL = TRUE
)

