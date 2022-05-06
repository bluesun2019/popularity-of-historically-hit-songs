# popularity-of-historically-hit-songs
This project aims to investigate the historical hit songs and design a simple method to identify those that are still popular on Spotify.  
These songs are often called classics because they are not buried in oblivion over time. 
## Introduction
* `EvolutionPopUSA_MainData.csv`    Dataset containing songs once entering the Billboard
* `Covering.RData`    Datasets containing information extracted from Spotify for part of songs in the first dataset
* `oversampling methods.R` and `XGboost.R`     R functions for prediction
* `Data_import_preprocess.R`    Code for data extraction and preprocessing (No need to run it)
* `Data_visualization.R`    Code for preliminary EDA
* `Data_prediction.R`     Code for prediction
* `Reproduce.Rmd`       Reproduce all the results 
* `final_report.pdf`    Final report
* `final_slides.pdf`    Final presentation slides
## Dependencies
* knitr
* spotifyr
* billboard
* tidyverse
* dplyr
* skimr
* lubridate
* ggplot2
* cowplot
* xgboost
* pROC
* RSNNS
* caret
* flextable
