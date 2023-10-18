---
title: "RamanD2O"
author: "Yanhai Gong"
date: "2020/12/16"
output: html_document
runtime: shiny
---

### Overview

`RamanD2O` is a web application developed using R Shiny to interactively analyze single cell Raman spectra data especially from different cells with different metabolic activities and incorporated heavy water.

### Load data

You can load your spectra data and metadata using `Load data` tab.
For spectrum file, two columns TXT file is supported, all spectra files should be archived in a single zip file.
For metadata table, it should be in TSV format with headers in the first line. The first column should be `ID_Cell`, feel free to add at least one columns.
To try RamanD2O, you can load a small demo using the link in `Load data` tab.

### Regular pipeline

1. `Sample`: subsample your dataset, optional.
2. `Trim`: trim wavelength, optional.
3. `Filter`: based on intensities or manually remove bad spectra.
4. `Smooth`: reduce high frequency noise, time consuming but recommended.
5. `Baseline`: remove baseline for each spectrum, recommended.
6. `Normalize`: normalize to allow equal comparison between spectrum, recommended.
7. `Average`: Calculate average spectra in each selected group.
8. `Carotenoid`: detect cells with carotenoid peaks, recommended if you need to calculate C-D ratio.
9. `SNR`: calculate signal noise ratio (SNR) and optionally filter dataset based on SNR values (recommended).
10. `CDR`: calculate C-D ratio for cells incorporated heavy water.

### Machine learning

1. `Prepare`: split the dataset into `train` and `eval` datasets.
2. `Explore`: visualize the dataset using t-SNE embedding (into two dimensions), optional.
3. `Random forest`: train a random forest classifier and evaluate for the `eval` dataset.

### Integration analysis
1. `Ramanome & Transcriptome`: Integrate Ramanome and Transcriptome using O2PLS.
2. `Ramanome & Metabolome`: Integrate Ramanome and Metabolome, TBD.
3. `MultiOmics Integration`: Integrate Ramanome with multiple omics datasets, TBD.
