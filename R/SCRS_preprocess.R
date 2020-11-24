#' Preprocess SCRS data
#'
#' @description Baseline and normalization for SCRS data
#' @param input_dir A directory
#' @param output A directory
#' @param meta_fp A TSV file
#' @param txt_filename A prefix of file names
#' @return Results were put into output directory
#' @export
SCRS_preprocess <- function(input_dir, output, meta_fp, txt_filename) {
  filepath <- file_path_as_absolute(input_dir) # directory containing SCRS txts
  meta_fp <- file_path_as_absolute(meta_fp) # meta TSV file, ID_Cell	Timepoint	Label	CellBg	Cell

  dir.create(output)
  output <- file_path_as_absolute(output)

  # Read meta data
  meta_data <- read.table(meta_fp, header = T, sep = "\t", stringsAsFactors = F) # metadata table
  ID_Cells <- meta_data$ID_Cell # Cells
  meta_colnames <- colnames(meta_data)

  # Combine txts to one txt
  setwd(filepath)
  files <- list.files(pattern = "*.txt") # at least 1 file
  shift <- read.table(files[1], header = F, sep = "\t")$V1
  final_data <- c(meta_colnames, shift)
  for (filename in files)
  {
    ID_Cell <- sub(".txt", "", filename)
    if (ID_Cell %in% ID_Cells) {
      dt <- read.table(filename, header = F, sep = "\t")$V2
      data <- c(unlist(meta_data[meta_data$ID_Cell == ID_Cell, ]), dt)
      final_data <- cbind(final_data, data)
    }
  }
  setwd(output)
  write.table(
    file = paste0(txt_filename, "_rawdata.txt"), t(final_data), sep = "\t",
    quote = F, row.names = F, col.names = F
  )

  # transform to data frame
  raw.data <- read.table(paste0(txt_filename, "_rawdata.txt"), header = T, sep = "\t")

  # wholespectra data
  Group <- paste(raw.data$Timepoint, sep = "")
  # Add No_Cell, Group and other meta information
  raw.data <- cbind(No_Cell = seq(1:nrow(raw.data)), Group = Group, raw.data)

  ### delete 1049 peak ####
  # raw.data<-raw.data[,-(which(colnames(raw.data)=="X1026.09"):which(colnames(raw.data)=="X1075.76"))]

  ## -bg##
  ncol_meta <- which(colnames(raw.data) == "Cell") # cols of meta data
  ncol_raw.data <- ncol(raw.data) # cols of raw.data

  # remove background
  Cells_bgsub <- raw.data[which(raw.data$CellBg == "Cell"), ]
  # Cells_bgsub<-raw.data_bgsub[which(raw.data_bgsub$CellBg=="Cell"),]
  # write.csv(Cells_bgsub,paste(output,"Cells_bg.csv",sep=""),quote = F,row.names = F)

  ## baseline##
  wavelength <- shift
  data_hyperSpec <- new("hyperSpec",
    data = data.frame(Cells_bgsub[, 1:ncol_meta]),
    spc = Cells_bgsub[, (ncol_meta + 1):ncol_raw.data], wavelength = wavelength
  )
  data_baseline <- data_hyperSpec - spc.fit.poly.below(data_hyperSpec, data_hyperSpec, poly.order = 7)
  # data_baseline <- data_hyperSpec - spc.rubberband(data_hyperSpec, noise=300, df=20)
  write.csv(data_baseline, "Cells_bg_baseline.csv", quote = F, row.names = F)

  ## Replace negative intensities to zero ##
  data_baseline_zero <- data_baseline$spc
  data_baseline_zero[data_baseline_zero < 0] <- 0
  data_baseline_zero_hyperSpec <- new("hyperSpec",
    data = data.frame(Cells_bgsub[, 1:ncol_meta]),
    spc = data_baseline_zero, wavelength = wavelength
  )
  write.csv(data_baseline_zero_hyperSpec, "Cells_bg_baseline_zero.csv", quote = F, row.names = F)

  # output txts
  Cells_bg_baseline_zero <- "Cells_bg_baseline_zero/"
  dir.create(Cells_bg_baseline_zero)
  for (i in (1:nrow(data_baseline_zero_hyperSpec))) {
    Cells <- data.frame(shift = shift, intensity = t(data_baseline_zero_hyperSpec[i]$spc))
    write.table(Cells, paste0(Cells_bg_baseline_zero, data_baseline_zero_hyperSpec$ID_Cell[i], ".txt"),
      row.names = F, col.names = F, quote = F, sep = "\t"
    )
  }

  ## normalization
  data_baseline_zero_scale_hyperSpec <- data_baseline_zero_hyperSpec / rowMeans(data_baseline_zero_hyperSpec)
  # data_baseline_zero_scale_hyperSpec <- data_baseline_zero_hyperSpec / rowSums (data_baseline_zero_hyperSpec)
  write.csv(data_baseline_zero_scale_hyperSpec, "Cells_bg_baseline_zero_scale.csv",
    quote = F, row.names = F
  )

  # output txts
  Cells_bg_baseline_zero_scale <- "Cells_bg_baseline_zero_scale/"
  dir.create(Cells_bg_baseline_zero_scale)
  for (i in (1:nrow(data_baseline_zero_scale_hyperSpec))) {
    Cells <- data.frame(shift = shift, intensity = t(data_baseline_zero_scale_hyperSpec[i]$spc))
    write.table(Cells, paste0(Cells_bg_baseline_zero_scale, data_baseline_zero_scale_hyperSpec$ID_Cell[i], ".txt"),
      row.names = F, col.names = F, quote = F, sep = "\t"
    )
  }
}
