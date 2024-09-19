# Introduction of analytical modules

## Load data

1. Data format

**SCRS zip file**: Each Raman spectrum dataset is stored in a .txt file with two columns: the first column indicates Raman shift frequency points, and the second column indicates Raman intensity. Prior to uploading, all Raman spectrum data should be compressed into a zip archive, which may include subdirectories. It is imperative to avoid duplicate file names for .txt files, and the compressed package should exclusively contain Raman spectrum data without any unrelated .txt files.

**Metadata tsv file**:  It should be in TSV file format, the header of the first column should be "ID_Cell" to store the filenames of the Raman spectroscopy files without the .txt extension.

**Options**: Interpolation: Raman spectra will be interpolated at integer frequencies.

**Notes**: Cosmic rays are eliminated during spectral uploading and immediately before interpolation (when the interpolation option is chosen). Interpolation becomes essential when the spectral files do not align in Raman shift frequencies.

2. Load RamEx data

RamanD2O and RamEx are interoperable; users can choose to import RamEx's Ramanome object (stored in RDS format) into RamanD2O.

## Pipeline

1. Sample

Raman spectra are subsampled randomly to retain a specific percentage.

2. Trim

All Raman spectra will be trimmed to retain specific wavelength ranges only.

3. Smooth

For enhanced signal-to-noise ratio (SNR), each Raman spectrum should undergo a smoothing process. RamanD2O incorporates three smoothing methods:
**LOESS**: LOESS (locally weighted smoothing), are non-parametric strategies for fitting a smooth curve to data points.
**SG**: Savitzky–Golay (SG) filtering, a widely used method, involves local least-squares fitting of data using polynomials. It is commonly employed for smoothing data and calculating derivatives from noisy datasets.
**EMD**: Denoising by empirical mode decomposition (EMD) and thresholding. Please refer to <a href="https://www.rdocumentation.org/packages/EMD/versions/1.5.9/topics/emddenoise" target="_blank">emddenoise</a> for detailed descriptions.

4. Baseline

Two methods of baseline removing are implemented in RamanD2O.
**polyfit**: Polynomial baseline fitting. please refer to <a href="https://r-hyperspec.github.io/hyperSpec/reference/baselines.html" target="_blank">spc_fit_poly or spc_fit_poly</a> for detailed descriptions.
**als**: Asymmetric Least Squares. please refer to <a href="https://www.rdocumentation.org/packages/baseline/versions/1.3-5/topics/baseline.als" target="_blank">baseline.als</a> for detailed descriptions.

**Notes**: Following baseline removal, some Raman intensities may exhibit negative values. These negative values can be reset to zero by selecting "Set to zero," or the entire Raman spectra can be vertically adjusted by choosing "Pull up." 

5. Normalize

6. Average

7. SNR

8. CDR

## Visualize

1. Export data

2. Visualization

## Statistics

1. PCA

2. LDA

3. MCR

## Machine learning

1. Prepare

2. Explore

3. Random forest

## Integration analysis

1. Ramanome & Transcriptome

2. Ramanome & Metabolome
TBD.

3. Multi-omics integration
TBD.
