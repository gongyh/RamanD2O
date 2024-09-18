# RamanD2O

<!-- badges: start -->
[![CircleCI](https://dl.circleci.com/status-badge/img/gh/gongyh/RamanD2O/tree/master.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/gongyh/RamanD2O/tree/master)
[![Docker Repository on Quay](https://quay.io/repository/gongyh/ramand2o/status "Docker Repository on Quay")](https://quay.io/repository/gongyh/ramand2o)
<!-- badges: end -->

RamanD2O is an R Shiny application that allows biologists to perform Raman spectra analysis.

# 1. Download and Install R
https://www.r-project.org

**Notes:** This app was tested in **R versions 4.2 and 4.3**.

# 2. Install RamanD2O package
```r
#install.packages('pak')
pak::pkg_install('gongyh/RamanD2O')
```

# 3. Run the R shiny app
```r
RamanD2O::runRamanD2O()
```

# 4. Usage instructions in the user interface
![User interface of RamanD2O](RamanD2O.png)

