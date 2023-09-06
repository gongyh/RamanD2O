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
        pca_center <- isolate(input$pca_center)
        pca_scale <- isolate(input$pca_scale)

        hs_cur <- hs$val[[isolate(input$hs_selector_for_statistics)]]
        pca <- prcomp(~spc, data = hs_cur@data, center = pca_center, scale=pca_scale)
        scores <- pca$x
        rownames(scores) <- rownames(hs_cur$spc)
        HC <- hclust(dist(scores), method = "ward.D2")
        Clusters <- cutree(HC, k = nclusters)
        Df <- data.frame(scores, "cluster" = factor(Clusters))
        Df <- cbind(hs_cur@data %>% dplyr::select(!matches("spc")), Df)
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
                       pca <- prcomp(~spc, data=hs_cur@data, center=T, scale=T)
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

                   if (nlevels(training$group) == 2) {
                     tp <- predict(linear, training)
                     tpdf <- data.frame(LD1 = tp$x, group = tp$class)
                     p <- ggplot(tpdf) + geom_density(aes(LD1, fill = group), alpha = 0.2)
                   } else {
                     p <- ggord(linear, training$group)
                   }

                   ggplotly(p +
                      labs(title=sprintf("training accuracy is %.2f, testing accuracy is %.2f", acc_train, acc_test))
                     )

                 })
               })
             },
             ignoreNULL = TRUE
)

# perform MCR on click of button
observeEvent(input$perform_mcr,
             {
               withBusyIndicatorServer("perform_mcr", {
                 output$mcr_plot <- renderPlotly({
                   validate(need(isolate(input$hs_selector_for_statistics), ""))
                   mcr_method <- isolate(input$select_mcr_method)
                   num_mcr_pcs <- isolate(input$num_mcr_pcs)

                   hs_cur <- hs$val[[isolate(input$hs_selector_for_statistics)]]
                   data <- hs_cur$spc

                   if (mcr_method == "MCR-Pure") {
                     m = mcrpure(data, ncomp = num_mcr_pcs)
                     summary(m)
                     cumexpvar <- m$variance[2,]
                     df <- data.frame(x=names(cumexpvar),y=cumexpvar)
                     p1 <- ggline(df, x="x", y="y") + theme_bw() +
                       labs(x="Components", y="Cumulative variance")
                     gp1 <- ggplotly(p1)
                     resspec <- melt(t(m$resspec))
                     p2 <- ggline(resspec, x="Var2", y= "value", group="Var1", color="Var1",
                            numeric.x.axis = T, shape = NA) + theme_bw()
                     gp2 <- ggplotly(p2)
                     subplot(gp2,gp1, nrows=2)
                   } else if (mcr_method == "MCR-ALS") {
                     m = mcrals(data, ncomp = num_mcr_pcs)
                     summary(m)
                     cumexpvar <- m$variance[2,]
                     df <- data.frame(x=names(cumexpvar),y=cumexpvar)
                     p1 <- ggline(df, x="x", y="y") + theme_bw() +
                       labs(x="Components", y="Cumulative variance")
                     gp1 <- ggplotly(p1)
                     resspec <- melt(t(m$resspec))
                     p2 <- ggline(resspec, x="Var2", y= "value", group="Var1", color="Var1",
                                  numeric.x.axis = T, shape = NA) + theme_bw()
                     gp2 <- ggplotly(p2)
                     subplot(gp2,gp1, nrows=2)
                   } else {
                     toastr_error("Not implemented!", position = "top-center")
                     return()
                   }

                 })
               })
             },
             ignoreNULL = TRUE
)

