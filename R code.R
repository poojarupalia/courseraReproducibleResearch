---
title: "Course Project 1 - Reproducible Research"
author: "Pooja Rupalia"
date: "17 March 2021"
output: rmarkdown::github_document

---
##Step1  
##Code for reading in the dataset and/or processing the data
```{r, echo = TRUE}
unzip("./repdata_data_activity.zip")
activityData <- read.csv("./activity.csv")
```

##Exploring the basics of this data
```{r, echo = TRUE}
dim(activityData)
names(activityData)
head(activityData)
str(activityData)
#total number of missing data
sum(is.na(activityData$steps))
#transforming the date column into date format using lubridate
library(lubridate)
activityData$date<-ymd(activityData$date)
length(unique(activityData$date))
```
##Step 2
## Histogram of the total number of steps taken each day
1. Calculate the total number of steps taken per day
```{r, echo = TRUE}
StepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm = TRUE)
2. Make a histogram of the total number of steps taken each day
hist(StepsPerDay$steps)
```
##Step 3
## Mean and median number of steps taken each day
```{r, echo = TRUE}
MeanStepsPerDay <- mean(StepsPerDay$steps)
MeanStepsPerDay

MedianStepsPerDay <- median(StepsPerDay$steps)
MedianStepsPerDay
```
##Step 4
##Time series plot of the average number of steps taken
```{r, echo = TRUE}
plot(steps ~ date, data=StepsPerDay, type="l")
```
##Step 5
##The 5-minute interval that, on average, contains the maximum number of steps
```{r, echo = TRUE}
StepsPerInterval <- aggregate(steps ~ interval, activityData, sum, na.rm = TRUE)
intervalWithMaxNbSteps <- StepsPerInterval[which.max(StepsPerInterval$steps),]$interval
intervalWithMaxNbSteps
```
##Step 6
##Code to describe and show a strategy for imputing missing data
```{r, echo = TRUE}
There are multiple strategies to deal with missing values, the most common being:
1. Constant value imputations
2. Regression model value imputations
3. Mean/mode value substitutions

For simplicity I will use point 3, filling in all the missing values in the dataset with the mean per interval.

totalValuesmissing <- sum(is.na(activityData$steps))
totalValuesmissing


```