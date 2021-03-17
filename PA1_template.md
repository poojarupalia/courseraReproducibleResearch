---
title: "Course Project 1 - Reproducible Research"
author: "Pooja Rupalia"
date: "17 March 2021"
output:
   html_document: 
    keep_md: true 

---
##Step1  
##Code for reading in the dataset and/or processing the data

```r
unzip("./repdata_data_activity.zip")
activityData <- read.csv("./activity.csv")
```

##Exploring the basics of this data

```r
dim(activityData)
```

```
## [1] 17568     3
```

```r
names(activityData)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(activityData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
#total number of missing data
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

```r
#transforming the date column into date format using lubridate
library(lubridate)
activityData$date<-ymd(activityData$date)
length(unique(activityData$date))
```

```
## [1] 61
```
##Step 2
## Histogram of the total number of steps taken each day
1. Calculate the total number of steps taken per day

```r
StepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm = TRUE)
```
2. Make a histogram of the total number of steps taken each day

```r
hist(StepsPerDay$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
##Step 3
## Mean and median number of steps taken each day

```r
MeanStepsPerDay <- mean(StepsPerDay$steps)
MeanStepsPerDay
```

```
## [1] 10766.19
```

```r
MedianStepsPerDay <- median(StepsPerDay$steps)
MedianStepsPerDay
```

```
## [1] 10765
```
##Step 4
##Time series plot of the average number of steps taken

```r
plot(steps ~ date, data=StepsPerDay, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
##Step 5
##The 5-minute interval that, on average, contains the maximum number of steps

```r
StepsPerInterval <- aggregate(steps ~ interval, activityData, sum, na.rm = TRUE)
intervalWithMaxNbSteps <- StepsPerInterval[which.max(StepsPerInterval$steps),]$interval
intervalWithMaxNbSteps
```

```
## [1] 835
```
##Step 6
##Code to describe and show a strategy for imputing missing data

There are multiple strategies to deal with missing values, the most common being:
1. Constant value imputations
2. Regression model value imputations
3. Mean/mode value substitutions

For simplicity I will use point 3, filling in all the missing values in the dataset with the mean per interval.

```r
totalValuesmissing <- sum(is.na(activityData$steps))
totalValuesmissing
```

```
## [1] 2304
```

```r
activityData$CompleteSteps <- ifelse(is.na(activityData$steps), round(StepsPerInterval$steps[match(activityData$interval, StepsPerInterval$interval)],0), activityData$steps)
```

##Step 7
##Histogram of the total number of steps taken each day after missing values are imputed

```r
hist(activityData$CompleteSteps,breaks=5)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
## Mean and median number of steps taken each day

```r
StepsPerDayNoNA <- aggregate(CompleteSteps ~ date, activityData, sum, na.rm = TRUE)
MeanStepsPerDayNoNA <- mean(StepsPerDayNoNA$CompleteSteps)
MeanStepsPerDayNoNA
```

```
## [1] 84188.07
```

```r
MedianStepsPerDayNoNA <- median(StepsPerDayNoNA$CompleteSteps)
MedianStepsPerDayNoNA
```

```
## [1] 11458
```

## Step 8
##Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
activityData$weekday <- weekdays(activityData$date)
activityData$DayType <- ifelse(activityData$weekday=='Saturday' |activityData$weekday=='Sunday', 'weekend', 'weekday')
StepsPerDayTypeNoNA <- aggregate(CompleteSteps ~ DayType + interval, activityData, mean)
library(lattice)
xyplot(CompleteSteps ~ interval | DayType, StepsPerDayTypeNoNA, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
