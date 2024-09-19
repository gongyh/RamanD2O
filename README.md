<!---
resource_files:
  - RamanD2O.png
--->
# RamanD2O

<!-- badges: start -->
[![R-CMD-check](https://github.com/gongyh/RamanD2O/actions/workflows/check-full.yaml/badge.svg)](https://github.com/gongyh/RamanD2O/actions/workflows/check-full.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Introduction

RamanD2O is an R Shiny application that allows biologists to perform Raman spectra analysis.

![User interface of RamanD2O](RamanD2O.png)

## Quick start

1. Download and Install R

https://www.r-project.org

**Notes:** This app was tested in **R 4.x**. If you are still using R 4.0-4.2, you may need to manually install specifc versions of a few dependency packages (e.g., randomForest<4.7, Matrix<1.7).

2. Install RamanD2O package
```r
#install.packages('pak')
pak::pkg_install('gongyh/RamanD2O')
```

3. Run the R shiny app
```r
RamanD2O::runRamanD2O()
```

## Documentation

1. [Function description](inst/docs/functions.md)
2. [Usage tutorial](inst/docs/usage.md)

## Contact

gongyh/RamanD2O is developed by [Yanhai Gong](mailto:gongyh@qibebt.ac.cn). We look forward to receive your feedback, bug reports, or suggestions for the further development of this package.

## License

This pipeline is open source under the MIT license, and integrates wonderful third-party softwares, which remain owned and copyrighted by their respective developers. Authors cannot be held legally or morally responsible for any consequences that may arise from using or misusing it.

