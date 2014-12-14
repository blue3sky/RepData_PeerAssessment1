---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
keep_md: true
---

opts_chunk$set(echo=TRUE)    

## Loading and preprocessing the data

```r
setwd("C:/Quan/MOOC/5. Reproducible Research/Peer Assessments/Peer Assessment1")
activityData <- read.csv("./activity.csv", sep=",", header=T)
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activityData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## What is mean total number of steps taken per day?

```r
library(ggplot2) 
hist <- qplot(x=as.Date(date), steps, data=subset(activityData, complete.cases(activityData)), stat='summary', fun.y=sum, main="Total No of Steps Taken Per Day", xlab="Date", ylab="No of Steps", geom = "bar")
plot(hist)
```

![plot of chunk MeanStepsPerDay](figure/MeanStepsPerDay-1.png) 

```r
aggData <- aggregate(steps ~ date, data = activityData, sum, na.rm = TRUE)
meanStepsPerDay <- mean(aggData$steps)
medStepsPerDay <- median(aggData$steps)
meanStepsPerDay
```

```
## [1] 10766.19
```

```r
medStepsPerDay
```

```
## [1] 10765
```
The mean and median top number of steps taken per day are 1.0766189 &times; 10<sup>4</sup> and 10765 respectively.
  
## What is the average daily activity pattern?

```r
aggMeanData <- aggregate(steps ~ interval, data = activityData, mean, na.rm = TRUE)
plot(steps ~ interval, data = aggMeanData, type = "l", main = "Average Daily Activity", xlab = "5 mins Time Interval", ylab = "Average No Of Steps Taken")
```

![plot of chunk AvgDailyAct](figure/AvgDailyAct-1.png) 

```r
ind <- which.max(aggMeanData[,2])
interval <- aggMeanData[ind,1]
maxSteps <- aggMeanData[ind,2]
interval
```

```
## [1] 835
```

```r
maxSteps
```

```
## [1] 206.1698
```
The maximum number of steps is 206.1698113 and takes place at the 835 5-minute interval across all the days in the dataset.

## Inputing missing values

```r
numNAValues <- sum(is.na(activityData))
completeActivityData <- activityData
completeActivityData$steps[is.na(completeActivityData$steps)] <- mean(completeActivityData$steps, na.rm=T)
library(ggplot2) 
hist <- qplot(x=as.Date(date), steps, data=completeActivityData, stat='summary', fun.y=sum, main="Total No of Steps Taken Per Day", xlab="Date", ylab="No of Steps", geom = "bar")
plot(hist)
```

![plot of chunk InputMissingValues](figure/InputMissingValues-1.png) 

```r
completeAggData <- aggregate(steps ~ date, data = completeActivityData, sum, na.rm = TRUE)
compMeanStepsPerDay <- mean(completeAggData$steps)
compMedStepsPerDay <- median(completeAggData$steps)
compMeanStepsPerDay
```

```
## [1] 10766.19
```

```r
compMedStepsPerDay
```

```
## [1] 10766.19
```
The total number of rows with NA is 2304.  
The strategy is to fill in the sample mean steps (non-NA) for all of the missing values in the dataset.  
AFter filling all missing values, the mean and median top number of steps taken per day are 1.0766189 &times; 10<sup>4</sup> and 1.0766189 &times; 10<sup>4</sup> respectively.  
The mean does not differ and the median differs slightly from the estimates from the first part of the assignment, after inputting missing data on the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?

```r
completeActivityData$day <- weekdays(as.Date(completeActivityData$date))
completeActivityData$wend <- as.factor(ifelse(weekdays(as.Date(completeActivityData$date)) %in% c("Saturday","Sunday"), "Weekend", "Weekday"))
library(lattice)
totalStepsDay <- aggregate(steps ~ interval + wend, data=completeActivityData, mean)
xyplot(steps ~ interval | factor(wend), data=totalStepsDay, layout = c(1,2), type="l")
```

![plot of chunk DiffWkDaysWkEnds](figure/DiffWkDaysWkEnds-1.png) 


