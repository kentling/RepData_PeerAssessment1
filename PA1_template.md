---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---





## Loading and preprocessing the data
Loading and preprocessing the data
Source data for this assessment is in activity.zip file.

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
Unzip and load source data:



```r
measurements <- read.csv(unz("repdata_data_activity.zip", "activity.csv"))
```


## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day:


```r
stepsPerDay <- aggregate(steps ~ date, measurements, sum)
hist(stepsPerDay$steps, main = "Steps per day", xlab = "Steps", col = "green", breaks = 8)
```

![](PA1_template_files/figure-html/total-1.png)<!-- -->


Calculate the mean and median of the total number of steps taken per day:

```r
mean(stepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

A time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
stepsInterval <- aggregate(steps ~ interval, measurements, mean)
plot(stepsInterval$interval, stepsInterval$steps, type="l", xlab = "5 min - interval", ylab = "Average steps", main = "Average Daily Activity Pattern", col = "green")
```

![](PA1_template_files/figure-html/stepsInterval-1.png)<!-- -->

Interval from 5-minute intervals, on average across all the days in the dataset, contains the maximum number of steps:


```r
stepsInterval$interval[which.max(stepsInterval$steps)]
```

```
## [1] 835
```

## Imputing missing values

The total number of missing values in the dataset is:


```r
nrow(measurements[is.na(measurements$steps),])
```

```
## [1] 2304
```

Filling in all of the missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in.

First assign NA values to be 0.

```r
measurementsWithoutNAs <- measurements
measurementsWithoutNAs[is.na(measurementsWithoutNAs$steps), "steps"] <- 0
```
Then calculate the total number of steps taken per day:

```r
stepsPerDayNoNAs <- aggregate(steps ~ date, measurementsWithoutNAs, sum)
hist(stepsPerDayNoNAs$steps, main = "Steps per day", xlab = "Steps", col = "blue", breaks = 8)
```

![](PA1_template_files/figure-html/stepsPerDayNoNAs-1.png)<!-- -->

Calculate the mean and median of the total number of steps taken per day:


```r
mean(stepsPerDayNoNAs$steps)
```

```
## [1] 9354.23
```

```r
median(stepsPerDayNoNAs$steps)
```

```
## [1] 10395
```

These values are differ from the estimates from the first part of the assignment. As we can see depends on the NAs filling function (in this case it was 0 value) we have shifted values. In this case there is left-shifted to 0. This affects the mean and the median values.

## Are there differences in activity patterns between weekdays and weekends?


Create a new factor variable in the dataset with two levels ? ?weekday? and ?weekend? indicating whether a given date is a weekday or weekend day.
0 is Sunday, 1 is Monday, etc.


```r
measurementsWithoutNAs$day <- as.POSIXlt(measurementsWithoutNAs$date)$wday
measurementsWithoutNAs$dayType <- as.factor(ifelse(measurementsWithoutNAs$day == 0 | measurementsWithoutNAs$day == 6, "weekend", "weekday"))
measurementsWithoutNAs <- subset(measurementsWithoutNAs, select = -c(day))

head(measurementsWithoutNAs)
```

```
##   steps       date interval dayType
## 1     0 2012-10-01        0 weekday
## 2     0 2012-10-01        5 weekday
## 3     0 2012-10-01       10 weekday
## 4     0 2012-10-01       15 weekday
## 5     0 2012-10-01       20 weekday
## 6     0 2012-10-01       25 weekday
```

Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken:


```r
weekdaysData <- measurementsWithoutNAs[measurementsWithoutNAs$dayType == "weekday",]
weekendsData <- measurementsWithoutNAs[measurementsWithoutNAs$dayType == "weekend",]
stepsIntervalWeekdays <- aggregate(steps ~ interval, weekdaysData, mean)
stepsIntervalWeekends <- aggregate(steps ~ interval, weekendsData, mean)

par(mfrow = c(2, 1))

plot(stepsIntervalWeekdays, type = "l", col = "green", main = "Weekdays")
plot(stepsIntervalWeekends, type = "l", col = "red", main = "Weekends")
```

![](PA1_template_files/figure-html/stepsIntervalWeekends-1.png)<!-- -->
