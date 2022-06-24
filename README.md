# Pilot Religion and Ecology Analysis
Code used for the analysis described in **Religion and Ecology: A Pilot Study Employing the Database of Religious History**. 

## Data 

Data were downloaded from the [Database of Religious History (DRH)](https://religiondatabase.org/landing/) on 6<sup>th</sup> February 2022.

## Data Analysis Software

All data preprocessing and analysis was performed using [R version 4.0.5](https://cran.r-project.org/index.html). The following packages were used:
  - [tidyverse version 1.3.1](https://cran.r-project.org/web/packages/tidyverse/index.html)
  - [data.table version 1.14.0](https://cran.r-project.org/web/packages/data.table/index.html)
  - [splitstackshape version 1.4.8](https://cran.r-project.org/web/packages/splitstackshape/index.html)
  - [sf version 1.0-4](https://cran.r-project.org/web/packages/sf/index.html)
  - [raster version 3.4-10](https://cran.r-project.org/web/packages/raster/index.html)
  - [exactextractr version 0.7.2](https://cran.r-project.org/web/packages/exactextractr/index.html)
  - [naniar version 0.6.1](https://cran.r-project.org/web/packages/naniar/index.html)
  - [testthat version 3.0.2](https://cran.r-project.org/web/packages/testthat/index.html)

For data visualization the following packages were used:
  - [corrplot version 0.88](https://cran.r-project.org/web/packages/corrplot/index.html)
  - [tmap version 3.3-1](https://cran.r-project.org/web/packages/tmap/index.html)

## Run Analysis 

To run the analysis use: 
1. Use ```setwd()``` or the RStudio file selector to set the working directory to the folder that contains the analysis code.
3. ```source("run_analysis.r")```

Note that running the analysis is a very computationally intensive process and will take multiple hours on a standard computer.