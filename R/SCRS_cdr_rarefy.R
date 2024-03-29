SCRS_cdr_rarefy <- function(input, outpath) {
  input <- file_path_as_absolute(input) # SCRS CDR stats file
  setwd("./")

  ######################################
  raw.data <- read.table(input, sep = "\t", header = TRUE)
  stdev_mean <- NULL
  Time <- levels(raw.data$Time)
  for (Time in Time) {
    content_timepoint <-
      raw.data[which(raw.data$Time == Time &
                       raw.data$cells == 60), "mean"] ## 60 ???
    raw.data_tem <-
      raw.data[which(raw.data$Time == Time), "stdev"] / content_timepoint * 100
    stdev_mean <- c(stdev_mean, raw.data_tem)
  }
  raw.data_new <- cbind(raw.data, stdev_mean)

  hline.data <- raw.data_new[which(raw.data_new$stdev_mean > 5), ]
  hline.data_all <- NULL
  Time <- levels(raw.data$Time)
  for (Time in Time) {
    hline.data_tem <- hline.data[which(hline.data$Time == Time), ]
    hline.data_tem <-
      hline.data_tem[order(hline.data_tem$cells, decreasing = TRUE), ]
    n_location <- hline.data_tem[1, "cells"] + 1
    hline.data_tem <-
      raw.data_new[which(raw.data_new$Time == Time &
                           raw.data_new$cells == n_location), ]
    hline.data_all <- rbind(hline.data_all, hline.data_tem)
  }

  dir.create(outpath)
  output <- file_path_as_absolute(outpath)
  setwd(output)

  raw.data_new <- within(raw.data_new, {
    Time <-
      factor(Time, levels = str_sort(levels(raw.data_new$Time), numeric = TRUE))
  })
  p1 <- ggplot(raw.data_new, aes(x = cells, y = stdev_mean)) +
    geom_point(size = 0.05) +
    geom_line() +
    xlab("Sampling Depth") +
    ylab(paste("Deviation from cumHI in %", sep = "")) +
    geom_hline(aes(yintercept = 5),
      colour = "grey10",
      linetype = "dashed"
    ) + # 5% ???
    geom_vline(
      data = hline.data_all,
      aes(xintercept = cells),
      colour = "grey10",
      linetype = "dashed"
    ) +
    geom_point(
      data = hline.data_all,
      aes(x = cells, y = stdev_mean),
      size = 1.5,
      color = "#990000",
      shape = 19
    ) +
    geom_text(
      data = hline.data_all,
      aes(
        x = cells,
        y = stdev_mean,
        label = paste(cells, "cells", sep = " ")
      ),
      colour = "#990000",
      size = 5,
      hjust = -0.5,
      vjust = -0.5
    ) +
    theme_bw() +
    facet_wrap(~Time, scales = "free_y")

  ggsave(
    filename = "CD_ratio_cumHI_accum_df_by_Time.pdf",
    plot = p1,
    limitsize = FALSE,
    width = 6,
    height = 5
  )
}
