---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---



## Loading and preprocessing the data



```r
DataFile <- "./activity.csv" # working file
CacheData <- read_dataCache( DataFile )
DT <- cacheData(CacheData) 
```

## What is mean total number of steps taken per day?


```r
# Calculate total number of steps in each day
DT <- DT[, sum(steps, na.rm = T), by = date]
```

[The summary distribution] of the total number of steps in each day.

```
## mean:       9354
## median:    10395
```

## What is the average daily activity pattern?

Calculates the average number of step in each interval

```r
DT <- DT[, mean(steps, na.rm = T), by = interval]
```

[Plot of the daily activity pattern] 

```
## Maximum at interval  835
```

## Imputing missing values
Count number of missing value

```
## Total number of missing values 2304
```
Help function to replace NA with mean value of the whole window. If no values were measured in the window then replace them by zero

```r
replace_NA <- function(X) {
  MEAN <- round(mean(X, na.rm = T), 0) # round since 0.4 steps make very little meaning 
  if (is.nan(MEAN) ) { MEAN <- 0 } # If no measurments in this window
  X[is.na(X)] <- MEAN
  return (X)
}
```
Create a new dataset that is equal to the original dataset but with the missing data filled in. The missing value is substituted by mean value for particular interval in the specific date

```r
DT_NEW <- DT[, replace_NA(steps), by = list(date, interval)]
colnames(DT_NEW) <- c("date", "interval", "steps")
```

[The summary] distribution of the total number of steps in each day (updated set).

```
## mean:      9354
## median:    10395
```

```
## Summary initial set
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-02:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-03:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-04:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-05:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-06:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-07:  288   Max.   :2355.0  
##                   (Other)   :13536
```

```
## Summary updated set
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 32.48   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.:  0.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

```
## Is identical, FALSE
```
Substituting of NA values by mean value for particular window doesn't effect the total number of steps,
however the average number of steps goes down.
## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend”


```r
DT_NEW$day_type<- as.factor(wday(DT_NEW$date) == 1 | wday(DT_NEW$date) == 7)
levels(DT_NEW$day_type) <- c("weekday", "weekend")
```
[A panel plot] containing a time series plots of average number of step for different day type.

Activity is different for different day of week.


### The unformatted but more detailed R code is located in [Assignment1.R] file
Usage:

    Rscript ./Assignment1.R
  
Output: Plots and bunch of warnings  

[The summary distribution]:./figures/plot1.png
[Plot of the daily activity pattern]:./figures/plot2.png
[The summary]:./figures/plot3.png
[A panel plot]:./figures/plot4.png
[Assignment1.R]:./Assignment1.R
