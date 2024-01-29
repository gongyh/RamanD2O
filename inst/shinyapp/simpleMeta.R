#!/usr/bin/env Rscript

## generate simple meta.tsv file

# Command line argument processing
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: simpleMeta.R <input_folder> <output_tsv>", call. = FALSE)
}

input_fn <- args[1] # input dir
output_tsv <- args[2] # output tsv

setwd(input_fn)

groups_dirs <- list.dirs(path = input_fn, full.names = FALSE, recursive = FALSE)

meta_df <- data.frame()

for (dir in groups_dirs) {
  spectra_files <- list.files(path = dir, pattern = "*.txt")
  filenames <- sub("\\.txt$", "", spectra_files)
  meta <- data.frame(ID_Cell = filenames, Group = dir)
  meta_df <- rbind(meta_df, meta)
}

write.table(meta_df, output_tsv, sep = "\t", quote = FALSE, row.names = F)
