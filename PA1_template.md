---
title: "Reproducible Research: Peer Assessment 1"
author: "Shuqian Fan"
date: "2020/6/7"
output: 
  html_document: 
    keep_md: yes
---

---
output:
  html_document: default
  pdf_document: default
---

## Data
The data for this assignment can be downloaded from the course web site:  
Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]
  
The variables included in this dataset are:    
  
**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
**date**: The date on which the measurement was taken in YYYY-MM-DD format  
**interval**: Identifier for the 5-minute interval in which measurement was taken  

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.  

---
## Code for reading in the dataset and/or processing the data

Unzip, read original data and load some necessary packages.  

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

activity <- read.csv('activity.csv')
activity$date <- as.Date(as.character(activity$date), '%Y-%m-%d')
```


## What is total number of steps taken per day?

Calculate the total number of steps taken per day and make a histogram.

```r
sumday <- activity %>%
        na.omit %>%
        group_by(date) %>%
        summarise(sum(steps))
colnames(sumday) <- c('date', 'steps')
hist(sumday$steps, main="Daily Steps", xlab="Number of steps", breaks = 10)
```

![](activity_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## Mean and median number of steps taken each day


```r
summn <- mean(sumday$steps, na.rm = T)
summd <- median(sumday$steps, na.rm = T)
summn
```

```
## [1] 10766.19
```

```r
summd
```

```
## [1] 10765
```

The mean of number of steps taken each day is 10766.19.  
The median of number of steps taken each day is 10765.  

## Time series plot of the average number of steps taken

Make a time series plot group by 5-minute interval and make a line plot.  

```r
timeseries <- 
        activity %>%
        na.omit %>%
        group_by(as.numeric(interval)) %>%
        summarise(mean(steps))

colnames(timeseries) <- c('interval', 'steps')
with(timeseries, plot(interval, steps, type = 'l'))
```

![](activity_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## The 5-minute interval that, on average, contains the maximum number of steps

Calculate the maximum value, result is 835, which means 8:35 at morning.  

```r
timeseries$interval[which(timeseries$steps == max(timeseries$steps))]
```

```
## [1] 835
```

## Code to describe and show a strategy for imputing missing data  

Method is finding NA values and imputing with 5-minutes interval average.  

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
count = 0
newactivity <- activity
i = 1
for(i in 1:nrow(newactivity)){
        if(is.na(newactivity[i,]$steps)){
                newactivity[i,]$steps <- timeseries$steps[which(timeseries$interval == activity[i,]$interval)]
                count = count + 1
        }
        i = i + 1
}

newsum <- aggregate(newactivity$steps, by = list(newactivity$date), sum)
colnames(newsum) <- c('date', 'steps')
mean(newsum$steps)
```

```
## [1] 10766.19
```

```r
median(newsum$steps)
```

```
## [1] 10766.19
```

Mean and median of new dataset after imputing missing values are both 10766.19.  

## Histogram of the total number of steps taken each day after missing values are imputed

And then make a new histogram.  

```r
hist(newsum$steps, main="Daily Steps", xlab="Number of steps", breaks = 10)
```

![](activity_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Separate date by weekdays and weekends, then sort them to a new variable wkdays, then group activity by wkdays.

```r
newactivity$wkdays <- weekdays(newactivity$date)
i = 0
for(i in 1:nrow(newactivity)){
        if(newactivity[i,]$wkdays %in% c('Saturday', 'Sunday')){
                newactivity[i,]$wkdays <- 'Weekends'
        }
        else{
                newactivity[i,]$wkdays <- 'Weekdays'
        }
        i = i + 1
}


avgwkday <- aggregate(newactivity$steps, by = list(newactivity$wkdays, newactivity$interval), mean, na.rm = T)
colnames(avgwkday) <- c('wkday', 'interval', 'steps')

ggplot(avgwkday, aes(interval, steps, color = wkday)) + geom_line()
```

![](activity_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


