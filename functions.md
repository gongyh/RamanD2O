# Introduction of analytical modules

## Load data

1. **Data format**

**SCRS zip file**: Each Raman spectrum dataset is stored in a .txt file with two columns: the first column indicates Raman shift frequency points, and the second column indicates Raman intensity. Prior to uploading, all Raman spectrum data should be compressed into a zip archive, which may include subdirectories. It is imperative to avoid duplicate file names for .txt files, and the compressed package should exclusively contain Raman spectrum data without any unrelated .txt files.

**Metadata tsv file**:  It should be in TSV file format, the header of the first column should be "ID_Cell" to store the filenames of the Raman spectroscopy files without the .txt extension.

**Options**: Interpolation: Raman spectra will be interpolated at integer frequencies.

**Notes**: Cosmic rays are eliminated during spectral uploading and immediately before interpolation (when the interpolation option is chosen). Interpolation becomes essential when the spectral files do not align in Raman shift frequencies.

2. **Load RamEx data**

RamanD2O and RamEx are interoperable; users can choose to import RamEx's Ramanome object (stored in RDS format) into RamanD2O.

## Pipeline

1. **Sample**

Raman spectra are subsampled randomly to retain a specific percentage.

2. **Trim**

All Raman spectra will be trimmed to retain specific wavelength ranges only.

3. **Smooth**

For enhanced signal-to-noise ratio (SNR), each Raman spectrum should undergo a smoothing process. RamanD2O incorporates three smoothing methods:

**LOESS**: LOESS (locally weighted smoothing), are non-parametric strategies for fitting a smooth curve to data points.

**SG**: Savitzky–Golay (SG) filtering, a widely used method, involves local least-squares fitting of data using polynomials. It is commonly employed for smoothing data and calculating derivatives from noisy datasets.

**EMD**: Denoising by empirical mode decomposition (EMD) and thresholding. Please refer to <a href="https://www.rdocumentation.org/packages/EMD/versions/1.5.9/topics/emddenoise" target="_blank">emddenoise</a> for detailed descriptions. **Caution: time-consuming!!!**

4. **Baseline**

Two methods of baseline removal are implemented in RamanD2O.

**polyfit**: Polynomial baseline fitting. please refer to <a href="https://r-hyperspec.github.io/hyperSpec/reference/baselines.html" target="_blank">spc_fit_poly or spc_fit_poly</a> for detailed descriptions.

**als**: Asymmetric Least Squares. please refer to <a href="https://www.rdocumentation.org/packages/baseline/versions/1.3-5/topics/baseline.als" target="_blank">baseline.als</a> for detailed descriptions.

**Notes**: Following baseline removal, some Raman intensities may exhibit negative values. These negative values can be reset to zero by selecting "Set to zero", or the entire Raman spectra can be vertically adjusted by choosing "Pull up". 

5. **Normalize**

Raman spectra can be normalized based on the area within a specific wavelength range. When the "Fingerprint" checkbox is chosen, normalization is conducted within the wavelength range of 500 to 2000 cm<sup>-1</sup>; otherwise, the entire spectrum is utilized.

6. **Average**

The average Raman spectra are computed based on selected metadata categories.

7. **SNR**

There are two calculation methods for signal-to-noise ratio (SNR).

**new**: $$SNR=\frac{\frac{max(I(2800-3050))-sum(I(1730-1800))}{length(1730-1800)}}{sd(I(1730-1800))}$$

**old**: $$SNR=\frac{\frac{max(I(1400-1460))-sum(I(1760-1790))}{length(1760-1790)}}{sd(I(1760-1790))}$$

**Quality controlled using SNR**: Low-quality spectra will be removed according to SNR if "Remove Low-SNR Spectra" is selected.

8. **CDR**

CDR refers to the ratio of the Raman spectrum area in the C-D region to the sum of the C-D and C-H regions, i.e., $CDR=\frac{area(C-D)}{area(C-D) + area(C-H)}$.

C-D intensity region: 2050~2300 cm<sup>-1</sup>

C-H intensity region: 2800~3050 cm<sup>-1</sup>

Two metadata columns (CDR and D2O) will be appended after calculation. The "CDR" column stores CDR values for each spectrum; the "D2O" column store whether C-D peak is exist (1) or not (0).

## Visualize

1. **Export data**

The generated Raman spectra and metadata can be exported in CSV, ZIP, or RamEx format for subsequent analysis.

2. **Visualization**

The Raman spectra after various processing steps can be visualized and compared interactively.

## Statistics

Raman spectra data can be analyzed using multivariable statistics (PCA, LDA and MCR-ALS).

## Machine learning

Raman spectra can be used for various classification tasks. RamanD2O supply functions to build and test random forest models.

1. **Prepare**

The Raman spectra can be splitted to train/eval/test datasets. Specific wavelength region can be used as features.

2. **Explore**

Before modeling, the splitted datasets can be visualized using t-SNE.

3. **Random forest**

randomForest implements Breiman's random forest algorithm (based on Breiman and Cutler's original Fortran code) for classification and regression. The RandomForestClassifier is trained using bootstrap aggregation, where each new tree is fit from a bootstrap sample of the training observations. The out-of-bag (OOB) error is the average error for each observation calculated using predictions from the trees that do not contain this observation in their respective bootstrap sample. This allows the RandomForestClassifier to be fit and validated whilst being trained.

## Integration analysis

1. **Ramanome & Transcriptome**

An O2PLS algorithm is implemented to integrate Ramanome datasets and transcriptome datasets.

2. **Ramanome & Metabolome**
TBD.

3. **Multi-omics integration**
TBD.
