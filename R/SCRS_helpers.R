## Helper functions

#' remove cosmic rays for each spectrum
#'
#' @param spc an intensities vector
#'
#' @export
removeCosmic <- function(spc) {
  medianFilt <- runmed(spc, 3)
  diff <- spc - medianFilt
  cutoff <- max(2000, 8 * sd(diff))
  df <- data.frame(raw = spc, med = medianFilt, diff = diff)
  cosmic <- FALSE
  if (length(which(df$diff > cutoff)) > 0) {
    cosmic <- TRUE
    df[which(df$diff > cutoff), "raw"] <- df[which(df$diff > cutoff), "med"]
  }
  return(list("spc" = df$raw, "cosmic" = cosmic))
}


#' search peaks for each spectrum
#'
#' @param spc an intensities vector
#' @param size need to be odd number
#' @param level adjustable constrain
#'
#' @export
findPeaks <- function(spc, size = 9, level = 0.1) {
  half <- (size - 1) / 2
  maxI <- max(spc)
  peaks <- c()
  for (i in (half + 1):(length(spc) - half)) {
    if ((spc[i] >= level * maxI) && (spc[i] >= max(spc[(i - half):(i + half)]))) {
      peaks <- c(peaks, i)
    }
  }
  peaks
}

#' load SCRS txt files to dataframe
#'
#' @param files txt files vector
#'
#' @export
SCRS_toDF <- function(files) {
  shift <- read.table(files[1], header = FALSE, sep = "\t")$V1
  scrs_df <- c("filename", shift)
  for (filename in files) {
    ID_Cell <- sub(".txt", "", basename(filename))
    dt <- read.table(filename, header = FALSE, sep = "\t")$V2
    # remove Cosmic Rays
    dt2 <- removeCosmic(dt)
    if (dt2$cosmic) {
      cat(filename, "contains cosmic ray signal.\n")
    }
    data <- c(ID_Cell, dt2$spc)
    scrs_df <- cbind(scrs_df, data)
  }
  scrs_df
}

# load meta tsv file to dataframe
