---
title: "Stats 2 Project 2"
author: "Rick Farrow, Bruce Granger, TQ Senkungu"
date: "11/28/2018"
output:
    html_document:
        keep_md: true
---



## Load Data

We have some Breast Cancer Data from the Wisconsin Diagnostic Breast Cancer (WDBC) dataset and will first load it in.


```r
wdbc_data <- read.csv("https://raw.githubusercontent.com/tikisen/6372_proj2/master/Data/breast-cancer-wisconsin-data.csv", sep = "\t",  row.names = NULL, stringsAsFactors = FALSE, header = TRUE)
```

## Including Plots

You can also embed plots, for example:



Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
