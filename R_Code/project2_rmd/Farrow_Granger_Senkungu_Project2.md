---
title: "Stats 2 Project 2"
author: "Rick Farrow, Bruce Granger, TQ Senkungu"
date: "12/1/2018"
output:
    html_document:
        keep_md: true
---



## Load Required Libraries
- Tidyverse 
- Amelia
- Corrplot
- PSCL



## Load Data

- We are using the Breast Cancer Data from the Wisconsin Diagnostic Breast Cancer (WDBC) dataset and will first load it.


```r
wdbc_data <- read.csv("https://raw.githubusercontent.com/tikisen/6372_proj2/master/Data/breast-cancer-wisconsin-data.csv", 
                      sep = ",", 
                      row.names = NULL, 
                      header = TRUE,
                      na.strings = c(""),
                      stringsAsFactors = FALSE)

wdbc_data <- wdbc_data %>% filter(ID !="Sample_code_number")
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.4
```

# Bruce's Work Starts Here

## Check for missing values

```r
sapply(wdbc_data,function(x) sum(is.na(x)))
```

```
##                          ID             Clump_Thickness 
##                           0                           2 
##        Uniformity_Cell_Size       Uniformity_Cell_Shape 
##                           2                           2 
##           Marginal_Adhesion Single_Epithelial_Cell_Size 
##                           2                           2 
##                 Bare_Nuclei             Bland_Chromatin 
##                           2                           2 
##             Normal_Nucleoli                     Mitoses 
##                           2                           2 
##                       Class 
##                           2
```

- The above output identifies there is missing data in each of the attributes, but what is not clear at this point if the missing values are for the same of different IDs.

- The following visualizes where the missing data is occurring:


```r
missmap(wdbc_data, main = "Missing values vs observed")
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/CheckForMissingValuesViz-1.png)<!-- -->

- The above visualization shows that the missing feature values are missing from the same ID.  Since each of the respective ID's is missing values for all of the fetures it doesn't make since to impute values, therefore each of the records will be removed from the data set. 


```r
wdbc.data <- wdbc_data %>% 
  filter(!is.na(wdbc_data$Uniformity_Cell_Shape))
rm(wdbc_data)
```

- Attributes will be coerced from character to numeric data type. 
- 32 NA will be introduced into the Bare_Nuclei attribute, the median will replace the NA values.
- Add an attribute called CancerState, which is similiar to the Class attribute, the difference is that CancerState uses words to describe the condition, meaning when Class == 2, then "Benign" and when Class == 4, then "Malignant "


```r
wdbc.data$ID <- as.integer(wdbc.data$ID)
wdbc.data$Clump_Thickness <- as.integer(wdbc.data$Clump_Thickness)
wdbc.data$Uniformity_Cell_Size <- as.integer(wdbc.data$Uniformity_Cell_Size)
wdbc.data$Uniformity_Cell_Shape <- as.integer(wdbc.data$Uniformity_Cell_Shape)
wdbc.data$Marginal_Adhesion <- as.integer(wdbc.data$Marginal_Adhesion)
wdbc.data$Single_Epithelial_Cell_Size <- as.integer(wdbc.data$Single_Epithelial_Cell_Size)
wdbc.data$Bare_Nuclei <- as.integer(wdbc.data$Bare_Nuclei)
```

```
## Warning: NAs introduced by coercion
```

```r
wdbc.data$Bland_Chromatin <- as.integer(wdbc.data$Bland_Chromatin)
wdbc.data$Normal_Nucleoli <- as.integer(wdbc.data$Normal_Nucleoli)
wdbc.data$Mitoses <- as.integer(wdbc.data$Mitoses)
wdbc.data$Class <- as.integer(wdbc.data$Class)

wdbc.data <- wdbc.data %>%
  dplyr::select(ID, Clump = Clump_Thickness, Cell_Size = Uniformity_Cell_Size, 
         Cell_Shape = Uniformity_Cell_Shape, Adhesion = Marginal_Adhesion,
         Epithelial = Single_Epithelial_Cell_Size, Nuclei = Bare_Nuclei,
         Chromatin = Bland_Chromatin, Nucleoli = Normal_Nucleoli, everything())

wdbc.data %>% dplyr::select(Nuclei) %>% summary()
```

```
##      Nuclei      
##  Min.   : 1.000  
##  1st Qu.: 1.000  
##  Median : 1.000  
##  Mean   : 3.545  
##  3rd Qu.: 6.000  
##  Max.   :10.000  
##  NA's   :32
```

```r
wdbc.data$Nuclei <- ifelse(is.na(wdbc.data$Nuclei),
                           median(wdbc.data$Nuclei, na.rm=TRUE), 
                           wdbc.data$Nuclei)

wdbc.data %>% dplyr::select(Nuclei) %>% summary()
```

```
##      Nuclei      
##  Min.   : 1.000  
##  1st Qu.: 1.000  
##  Median : 1.000  
##  Mean   : 3.486  
##  3rd Qu.: 5.000  
##  Max.   :10.000
```

```r
entire.dataset <- wdbc.data %>% 
  mutate(CancerState = case_when(Class == 2 ~ "Benign",
                                 Class == 4 ~ "Malignant"))

entire.dataset$CancerState <- as.factor(entire.dataset$CancerState)
```


## Summary Statistics/Histograms

```r
entire.dataset %>% dplyr::select(c(2:12)) %>% summary() 
```

```
##      Clump          Cell_Size        Cell_Shape        Adhesion     
##  Min.   : 1.000   Min.   : 1.000   Min.   : 1.000   Min.   : 1.000  
##  1st Qu.: 2.000   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 1.000  
##  Median : 4.000   Median : 1.000   Median : 1.000   Median : 1.000  
##  Mean   : 4.418   Mean   : 3.134   Mean   : 3.207   Mean   : 2.807  
##  3rd Qu.: 6.000   3rd Qu.: 5.000   3rd Qu.: 5.000   3rd Qu.: 4.000  
##  Max.   :10.000   Max.   :10.000   Max.   :10.000   Max.   :10.000  
##    Epithelial         Nuclei         Chromatin         Nucleoli     
##  Min.   : 1.000   Min.   : 1.000   Min.   : 1.000   Min.   : 1.000  
##  1st Qu.: 2.000   1st Qu.: 1.000   1st Qu.: 2.000   1st Qu.: 1.000  
##  Median : 2.000   Median : 1.000   Median : 3.000   Median : 1.000  
##  Mean   : 3.216   Mean   : 3.486   Mean   : 3.438   Mean   : 2.867  
##  3rd Qu.: 4.000   3rd Qu.: 5.000   3rd Qu.: 5.000   3rd Qu.: 4.000  
##  Max.   :10.000   Max.   :10.000   Max.   :10.000   Max.   :10.000  
##     Mitoses           Class         CancerState 
##  Min.   : 1.000   Min.   :2.00   Benign   :916  
##  1st Qu.: 1.000   1st Qu.:2.00   Malignant:482  
##  Median : 1.000   Median :2.00                  
##  Mean   : 1.589   Mean   :2.69                  
##  3rd Qu.: 1.000   3rd Qu.:4.00                  
##  Max.   :10.000   Max.   :4.00
```

```r
# TOTAL COUNT BY CANCER TYPE ####
ggplot(data=entire.dataset, aes(x=CancerState, colour = CancerState)) +
  geom_bar() +
  geom_text(stat='Count', aes(label=..count..), vjust = 10) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("blue", "red")) +
  ggtitle("Total Count by Cancer Type: Blue = Benign; Red = Malignant ") 
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/SummaryStats-1.png)<!-- -->

```r
# PERCENT OF TOTAL BY CANCER TYPE ####
entire.dataset.percent <- entire.dataset %>% 
  count(CancerState) %>% 
  mutate(perc = n / nrow(entire.dataset))

ggplot(data=entire.dataset.percent, aes(x = CancerState, y = perc, colour = CancerState)) +
  geom_bar(stat = "identity") +
  geom_text(stat = "identity", aes(label=round(perc*100,2)), vjust = 10) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("blue", "red")) +
  ggtitle("Percent of Total by Cancer Type: Blue = Benign; Red = Malignant ")
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/SummaryStats-2.png)<!-- -->

```r
rm(entire.dataset.percent) 
```

- Ideally, the proportion of events and non-events in the Y variable should approximately be the same.
- From the above histograms it is obvious there is a class bias, a condition observed when the Malignant proportion of events is much smaller than proportion of Benign events, by a factor of 1 to 1.9.  
- As a result, we must sample the observations in approximately equal proportions in order to get better model.  Unfortunately we have a small sample size, which means we are not able to implement this strategy. 
 
## Pairs Plots
- Version 1:
- The objective of the "Pairs Plot" is to create plots based upon paring of variables
- Additionaly, each observation is color coded to simutaleniously see if the observation is "Benign" or "Malignant " cancer.  
  - Color Coding:
    * Blue = Benign 
    * Red = Malignant 


```r
ed.small <- entire.dataset %>% dplyr::select(-c(1,12))

cols <- character(nrow(ed.small))
cols[] <- "black"
cols[ed.small$Class == 2] <- "blue"
cols[ed.small$Class == 4] <- "red"
pairs(ed.small, col=cols, main = "WDBC Pairs Plot: Blue = Benign; Red = Malignant ")
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/pairsplot_ver1-1.png)<!-- -->

```r
rm(cols)
```

- Version 2:
- Similar to the first Pairs Plot, however version 2 introduces jitter to the observations and as a result it is easy to see the density of the observations.


```r
# the alpha argument in rgb() lets you set the transparency
cols2 = c(rgb(red=0, green=0, blue=255, alpha=50, maxColorValue=255), 
          rgb(red=255, green=0, blue=0, alpha=50, maxColorValue=255))
cols2 = ifelse(ed.small$Class==2, cols2[1], cols2[2])

# here we jitter the data
set.seed(6141)  # this makes the example exactly reproducible
jbreast = apply(ed.small[,1:9], 2, FUN=function(x){ jitter(x, amount=.5) })
jbreast = cbind(jbreast, class=ed.small[,10])  # the class variable is not jittered

#windows()  # to match up the 1st & 2nd sets requires more coding
layout(matrix(1:25, nrow=5, byrow=T))
par(mar=c(.5,.5,.5,.5), oma=c(2,2,2,2))

for(i in 1:5){
   for(j in 6:10){
    
    plot(jbreast[,j], jbreast[,i], col=cols2, pch=16,
         
         axes=F, main="", xlab="", ylab="")
    
    box()
    
    if(j==6 ){ mtext(colnames(jbreast)[i], side=2, cex=.7, line=1) }
    
    if(i==5 ){ mtext(colnames(jbreast)[j], side=1, cex=.7, line=1) }
    
    if(j==10){ axis(side=4, seq(2,10,2), cex.axis=.8) }
    
    if(i==1 ){ axis(side=3, seq(2,10,2), cex.axis=.8) }
    
  }
  
}
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/pairsplot_ver2-1.png)<!-- -->

```r
rm(list = c("jbreast", "cols2", "i", "j"))
```

## Corrleation Plot
- Positive correlations are displayed in blue and negative correlations in red color.
- Color intensity and the size of the circle are proportional to the correlation coefficients.


```r
corr <- cor(ed.small)
corrplot(corr, method ="number", type= "upper")
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/CorrelationPlot-1.png)<!-- -->

```r
corrplot(corr, method ="circle", type= "upper")
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/CorrelationPlot-2.png)<!-- -->

```r
rm(corr)
```

## Histogram
- The histograms are a look at each attribute broken down by CancerState


```r
ed.small <- entire.dataset %>% dplyr::select(-c(1,11))

ed.tall <- ed.small %>%
  gather(-10, key = "Variable", value = "Value") %>% 
  filter(!is.na(CancerState))

ggplot(data = ed.tall, aes(x=Value)) +
  geom_histogram(bins=15) + 
  facet_wrap(Variable ~ CancerState, ncol = 9)
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/histogram-1.png)<!-- -->

## Boxplots
- Boxplot of attributes, color by CancerState


```r
ed.tall <- ed.small %>% 
  gather(-10, key = "Variable", value = "Value") %>% 
  filter(!is.na(CancerState))

ggplot(ed.tall, aes(x=CancerState, y=Value, fill = CancerState)) + 
  geom_boxplot() +
  facet_wrap(~ Variable) +
  ggtitle("WDBC Boxplot ") + 
  scale_fill_manual(breaks = c("Benign", "Malignant "), values = c("blue", "red")) +
  theme(legend.position="none")
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/Boxplot-1.png)<!-- -->


```r
set.seed(12345) #to get repeatable data

wdbc.train <- sample_frac(ed.small, 0.7, replace = FALSE)

train.index <- as.numeric(rownames(wdbc.train))
wdbc.test <- ed.small[-train.index,]

rm(train.index)

# MODEL FITTING
wdbc.glm.fit <- glm(CancerState ~ ., data = wdbc.train, family = binomial(link = "logit"))

summary(wdbc.glm.fit)
```

```
## 
## Call:
## glm(formula = CancerState ~ ., family = binomial(link = "logit"), 
##     data = wdbc.train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.4793  -0.1275  -0.0613   0.0285   2.3439  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -10.01551    0.92630 -10.812  < 2e-16 ***
## Clump         0.61590    0.11491   5.360 8.32e-08 ***
## Cell_Size    -0.09790    0.15511  -0.631 0.527938    
## Cell_Shape    0.33844    0.16877   2.005 0.044931 *  
## Adhesion      0.28207    0.08946   3.153 0.001617 ** 
## Epithelial   -0.02268    0.12801  -0.177 0.859380    
## Nuclei        0.41127    0.07459   5.514 3.51e-08 ***
## Chromatin     0.48556    0.12646   3.840 0.000123 ***
## Nucleoli      0.16994    0.08750   1.942 0.052111 .  
## Mitoses       0.60983    0.25228   2.417 0.015635 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1270.6  on 978  degrees of freedom
## Residual deviance:  170.9  on 969  degrees of freedom
## AIC: 190.9
## 
## Number of Fisher Scoring iterations: 8
```
Assessing Model Fit:

The following attributes are NOT statistically significant:
* Cell_Size
* Epithelial
  
The following attributes ARE statistically significant:
* Clump
* Nuclei
* Chromatin
* Adhesion
* Cell_Shape
* Mitoses
  
- In this logit model, the response valirable (CancerState) is log odds, a unit increase in Clump increases the odds by 0.6, a unit increase in Nuclei increases the odds by 0.4, and a unit increase in Chromatin increases the odds by 0.5.
- The null deviance (the deviance just for the mean) is 1271 and the residual deviance (the deviance for the model) is 171.
- The difference between the null deviance and the residual deviance shows how the model is doing against the null model (a model with only the intercept). The wider the gap between the null deviance and the residual deviance, the better.
- Analyzing the table we can see the drop in deviance when adding each variable one at a time, with the exception of when adding Nuclei and Chromatin.
- With the addition of Clump significantly reduces the residual deviance and each additional attribute reduces the residual deviance, but in much smaller increments.


```r
anova(wdbc.glm.fit, test = "Chisq")
```

```
## Analysis of Deviance Table
## 
## Model: binomial, link: logit
## 
## Response: CancerState
## 
## Terms added sequentially (first to last)
## 
## 
##            Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
## NULL                         978    1270.59              
## Clump       1   605.87       977     664.72 < 2.2e-16 ***
## Cell_Size   1   332.16       976     332.56 < 2.2e-16 ***
## Cell_Shape  1    36.24       975     296.32 1.749e-09 ***
## Adhesion    1    34.33       974     261.99 4.647e-09 ***
## Epithelial  1     5.86       973     256.13  0.015493 *  
## Nuclei      1    54.84       972     201.29 1.304e-13 ***
## Chromatin   1    18.07       971     183.22 2.132e-05 ***
## Nucleoli    1     4.24       970     178.98  0.039418 *  
## Mitoses     1     8.08       969     170.90  0.004474 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Assessing Table of Deviance (ToD):

From the ToD we are able to see the difference between the null deviance versus the residual deviance.  The ToD shows how our model is doing against the null model, which is a model with only the intercept. The wider this gap between the model versus the null, the better.

By adding Clump and Cell_Size to the model drastically reduces the residual deviance.  All attributes have 0.05 p-value or less.

Eventhough there is not an exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit:

```r
pr2_mc <- pR2(wdbc.glm.fit)
pr2_mc[[4]]
```

```
## [1] 0.8654986
```

Assessing the predictive ability of the model

```r
glm.probs <- predict(wdbc.glm.fit,type = "response")
glm.probs[1:5]
```

```
##        1008        1224        1063        1237         637 
## 0.998342115 0.003956658 0.043678314 0.030102178 0.999781209
```

```r
glm.pred <- ifelse(glm.probs > 0.5, "Malignant ","Benign")

attach(wdbc.train)
table(glm.pred,CancerState)
```

```
##             CancerState
## glm.pred     Benign Malignant
##   Benign        616        17
##   Malignant      18       328
```


# Bruce's Work Ends Here

# Rick's Work Starts Here

## Including Plots

You can also embed plots, for example:



# Rick's Work Ends Here

#TQ's Work Starts Here


##Proportions


```r
attach(entire.dataset)
```

```
## The following objects are masked from wdbc.train:
## 
##     Adhesion, CancerState, Cell_Shape, Cell_Size, Chromatin,
##     Clump, Epithelial, Mitoses, Nuclei, Nucleoli
```

```r
#table of counts
ftable(addmargins(table(CancerState, Clump)))
```

```
##             Clump    1    2    3    4    5    6    7    8    9   10  Sum
## CancerState                                                             
## Benign             284   92  192  136  170   32    2    8    0    0  916
## Malignant            6    8   24   24   90   36   44   84   28  138  482
## Sum                290  100  216  160  260   68   46   92   28  138 1398
```

```r
ftable(addmargins(table(CancerState, Cell_Size)))
```

```
##             Cell_Size    1    2    3    4    5    6    7    8    9   10  Sum
## CancerState                                                                 
## Benign                 760   74   54   18    0    4    2    2    2    0  916
## Malignant                8   16   50   62   60   50   36   56   10  134  482
## Sum                    768   90  104   80   60   54   38   58   12  134 1398
```

```r
ftable(addmargins(table(CancerState, Cell_Shape)))
```

```
##             Cell_Shape    1    2    3    4    5    6    7    8    9   10  Sum
## CancerState                                                                  
## Benign                  702  104   66   26    6    6    4    2    0    0  916
## Malignant                 4   14   46   62   62   54   56   54   14  116  482
## Sum                     706  118  112   88   68   60   60   56   14  116 1398
```

```r
ftable(addmargins(table(CancerState, Adhesion)))
```

```
##             Adhesion    1    2    3    4    5    6    7    8    9   10  Sum
## CancerState                                                                
## Benign                750   74   62   10    8    8    0    0    2    2  916
## Malignant              64   42   54   56   38   36   26   50    8  108  482
## Sum                   814  116  116   66   46   44   26   50   10  110 1398
```

```r
ftable(addmargins(table(CancerState, Epithelial)))
```

```
##             Epithelial    1    2    3    4    5    6    7    8    9   10  Sum
## CancerState                                                                  
## Benign                   92  726   58   14   10    4    6    4    0    2  916
## Malignant                 2   46   86   82   68   78   18   38    4   60  482
## Sum                      94  772  144   96   78   82   24   42    4   62 1398
```

```r
ftable(addmargins(table(CancerState, Nuclei)))
```

```
##             Nuclei    1    2    3    4    5    6    7    8    9   10  Sum
## CancerState                                                              
## Benign              802   42   28   12   20    0    2    4    0    6  916
## Malignant            34   18   28   26   40    8   14   38   18  258  482
## Sum                 836   60   56   38   60    8   16   42   18  264 1398
```

```r
ftable(addmargins(table(CancerState, Chromatin)))
```

```
##             Chromatin    1    2    3    4    5    6    7    8    9   10  Sum
## CancerState                                                                 
## Benign                 300  318  258   16    8    2   14    0    0    0  916
## Malignant                4   14   72   64   60   18  132   56   22   40  482
## Sum                    304  332  330   80   68   20  146   56   22   40 1398
```

```r
ftable(addmargins(table(CancerState, Nucleoli)))
```

```
##             Nucleoli    1    2    3    4    5    6    7    8    9   10  Sum
## CancerState                                                                
## Benign                804   60   24    2    4    8    4    8    2    0  916
## Malignant              82   12   64   34   34   36   28   40   30  122  482
## Sum                   886   72   88   36   38   44   32   48   32  122 1398
```

```r
ftable(addmargins(table(CancerState, Mitoses)))
```

```
##             Mitoses    1    2    3    4    5    6    7    8   10  Sum
## CancerState                                                          
## Benign               890   16    4    0    2    0    2    2    0  916
## Malignant            268   54   62   24   10    6   16   14   28  482
## Sum                 1158   70   66   24   12    6   18   16   28 1398
```

```r
#in proprtions
prop.table(table(CancerState, Clump),2)
```

```
##            Clump
## CancerState          1          2          3          4          5
##   Benign    0.97931034 0.92000000 0.88888889 0.85000000 0.65384615
##   Malignant 0.02068966 0.08000000 0.11111111 0.15000000 0.34615385
##            Clump
## CancerState          6          7          8          9         10
##   Benign    0.47058824 0.04347826 0.08695652 0.00000000 0.00000000
##   Malignant 0.52941176 0.95652174 0.91304348 1.00000000 1.00000000
```

```r
prop.table(table(CancerState, Cell_Size),2)
```

```
##            Cell_Size
## CancerState          1          2          3          4          5
##   Benign    0.98958333 0.82222222 0.51923077 0.22500000 0.00000000
##   Malignant 0.01041667 0.17777778 0.48076923 0.77500000 1.00000000
##            Cell_Size
## CancerState          6          7          8          9         10
##   Benign    0.07407407 0.05263158 0.03448276 0.16666667 0.00000000
##   Malignant 0.92592593 0.94736842 0.96551724 0.83333333 1.00000000
```

```r
prop.table(table(CancerState, Cell_Shape),2)
```

```
##            Cell_Shape
## CancerState           1           2           3           4           5
##   Benign    0.994334278 0.881355932 0.589285714 0.295454545 0.088235294
##   Malignant 0.005665722 0.118644068 0.410714286 0.704545455 0.911764706
##            Cell_Shape
## CancerState           6           7           8           9          10
##   Benign    0.100000000 0.066666667 0.035714286 0.000000000 0.000000000
##   Malignant 0.900000000 0.933333333 0.964285714 1.000000000 1.000000000
```

```r
prop.table(table(CancerState, Adhesion),2)
```

```
##            Adhesion
## CancerState          1          2          3          4          5
##   Benign    0.92137592 0.63793103 0.53448276 0.15151515 0.17391304
##   Malignant 0.07862408 0.36206897 0.46551724 0.84848485 0.82608696
##            Adhesion
## CancerState          6          7          8          9         10
##   Benign    0.18181818 0.00000000 0.00000000 0.20000000 0.01818182
##   Malignant 0.81818182 1.00000000 1.00000000 0.80000000 0.98181818
```

```r
prop.table(table(CancerState, Epithelial),2)
```

```
##            Epithelial
## CancerState          1          2          3          4          5
##   Benign    0.97872340 0.94041451 0.40277778 0.14583333 0.12820513
##   Malignant 0.02127660 0.05958549 0.59722222 0.85416667 0.87179487
##            Epithelial
## CancerState          6          7          8          9         10
##   Benign    0.04878049 0.25000000 0.09523810 0.00000000 0.03225806
##   Malignant 0.95121951 0.75000000 0.90476190 1.00000000 0.96774194
```

```r
prop.table(table(CancerState, Nuclei),2)
```

```
##            Nuclei
## CancerState          1          2          3          4          5
##   Benign    0.95933014 0.70000000 0.50000000 0.31578947 0.33333333
##   Malignant 0.04066986 0.30000000 0.50000000 0.68421053 0.66666667
##            Nuclei
## CancerState          6          7          8          9         10
##   Benign    0.00000000 0.12500000 0.09523810 0.00000000 0.02272727
##   Malignant 1.00000000 0.87500000 0.90476190 1.00000000 0.97727273
```

```r
prop.table(table(CancerState, Chromatin),2)
```

```
##            Chromatin
## CancerState          1          2          3          4          5
##   Benign    0.98684211 0.95783133 0.78181818 0.20000000 0.11764706
##   Malignant 0.01315789 0.04216867 0.21818182 0.80000000 0.88235294
##            Chromatin
## CancerState          6          7          8          9         10
##   Benign    0.10000000 0.09589041 0.00000000 0.00000000 0.00000000
##   Malignant 0.90000000 0.90410959 1.00000000 1.00000000 1.00000000
```

```r
prop.table(table(CancerState, Nucleoli),2)
```

```
##            Nucleoli
## CancerState          1          2          3          4          5
##   Benign    0.90744921 0.83333333 0.27272727 0.05555556 0.10526316
##   Malignant 0.09255079 0.16666667 0.72727273 0.94444444 0.89473684
##            Nucleoli
## CancerState          6          7          8          9         10
##   Benign    0.18181818 0.12500000 0.16666667 0.06250000 0.00000000
##   Malignant 0.81818182 0.87500000 0.83333333 0.93750000 1.00000000
```

```r
prop.table(table(CancerState, Mitoses),2)
```

```
##            Mitoses
## CancerState          1          2          3          4          5
##   Benign    0.76856649 0.22857143 0.06060606 0.00000000 0.16666667
##   Malignant 0.23143351 0.77142857 0.93939394 1.00000000 0.83333333
##            Mitoses
## CancerState          6          7          8         10
##   Benign    0.00000000 0.11111111 0.12500000 0.00000000
##   Malignant 1.00000000 0.88888889 0.87500000 1.00000000
```

```r
#vizualize 
plot(CancerState~Clump,col=c("red","blue"))
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/proportions-1.png)<!-- -->

```r
plot(CancerState~Cell_Shape,col=c("red","blue"))
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/proportions-2.png)<!-- -->

```r
plot(CancerState~Cell_Size,col=c("red","blue"))
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/proportions-3.png)<!-- -->

```r
plot(CancerState~Adhesion,col=c("red","blue"))
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/proportions-4.png)<!-- -->

```r
plot(CancerState~Epithelial,col=c("red","blue"))
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/proportions-5.png)<!-- -->

```r
plot(CancerState~Nuclei,col=c("red","blue"))
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/proportions-6.png)<!-- -->

```r
plot(CancerState~Chromatin,col=c("red","blue"))
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/proportions-7.png)<!-- -->

```r
plot(CancerState~Nucleoli,col=c("red","blue"))
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/proportions-8.png)<!-- -->

```r
plot(CancerState~Mitoses,col=c("red","blue"))
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/proportions-9.png)<!-- -->

## PCA


```r
pc.result<-prcomp(entire.dataset[,2:11],scale.=TRUE)
pc.scores<-pc.result$x
pc.scores<-data.frame(pc.scores)
pc.scores$Class<-entire.dataset$Class

#Scree plot
eigenvals<-(pc.result$sdev)^2
plot(1:10,eigenvals/sum(eigenvals),type="l",main="Scree Plot PC's",ylab="Prop. Var. Explained",ylim=c(0,1))
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
lines(1:10,cumulative.prop,lty=2)
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/PCA-1.png)<!-- -->

```r
#Use ggplot2 to plot the first few pc's
ggplot(data = pc.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=Class), size=1)+
  ggtitle("PCA of Cancer Status")
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/PCA-2.png)<!-- -->

```r
ggplot(data = pc.scores, aes(x = PC2, y = PC3)) +
  geom_point(aes(col=Class), size=1)+
  ggtitle("PCA of Cancer Status")
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/PCA-3.png)<!-- -->

```r
ggplot(data = pc.scores, aes(x = PC1, y = PC3)) +
  geom_point(aes(col=Class), size=1)+
  ggtitle("PCA of Cancer Status")
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/PCA-4.png)<!-- -->
## Separation

PC1 vs PC2 and PC1 vs PC3 show good separation so some variable is a good predictor of cancer status.


## LDA & QDA


```r
mylda<- lda(Class ~ Clump+Cell_Size+Cell_Shape+Adhesion+Epithelial+Nuclei+Chromatin+Nucleoli+Mitoses, data = entire.dataset)
myqda<- qda(Class ~ Clump+Cell_Size+Cell_Shape+Adhesion+Epithelial+Nuclei+Chromatin+Nucleoli+Mitoses, data = entire.dataset)

#confusion matrix
set.seed(2134)
index<-sample(1:385,250,replace=FALSE)
test.entire.dataset<-entire.dataset[-index,]
prd<-predict(mylda, newdata = test.entire.dataset)$class
table(prd,test.entire.dataset$Class)
```

```
##    
## prd   2   4
##   2 761  29
##   4  13 345
```

```r
#ROC
ldaprd<-predict(mylda, newdata = entire.dataset)$posterior
#correcting for the way lda creates predicted probabilities
ldaprd<-ldaprd[,2]

pred <- prediction(ldaprd, entire.dataset$Class)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plot ROC
plot(roc.perf,main="LDA")
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/LDA_QDA-1.png)<!-- -->


## Stepwise Regression


```r
#?lm

full.model <- lm (Class ~., data = entire.dataset)
```

## LDA


```r
entire.dataset.continuous <- entire.dataset[, c(2:11)]
pairs(entire.dataset.continuous[,1:10])
```

![](Farrow_Granger_Senkungu_Project2_files/figure-html/LDA-1.png)<!-- -->


#TQ's Work Ends Here
