---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
setwd("c:/DS Assignments")
#unzip("repdata%2Fdata%2Factivity.zip")
data <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
data$date <-as.Date(data$date, format = "%Y-%m-%d")
```



## What is mean total number of steps taken per day?


```r
TotalSteps <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)

hist(TotalSteps$steps, col="blue",main="Histogram of Total Steps taken per day",xlab="Total Steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(TotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(TotalSteps$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?


```r
StepsInterval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
plot(steps ~ interval, data = StepsInterval, type = "l", xlab = "Time Intervals (5-minute)", ylab = "Mean number of steps taken (all Days)", main = "Average number of steps Taken at 5 minute Intervals",  col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxinterval <- StepsInterval[which.max(StepsInterval$steps), "interval"]
maxinterval
```

```
## [1] 835
```


## Imputing missing values

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

```r
## This function returns the mean steps for a given interval
getMeanStepsPerInterval <- function(interval){
    StepsInterval[StepsInterval$interval==interval,"steps"]
}

complete.activity <- data

## Filling the missing values with the mean for that 5-minute interval
flag = 0
for (i in 1:nrow(complete.activity)) {
    if (is.na(complete.activity[i,"steps"])) {
        complete.activity[i,"steps"] <- getMeanStepsPerInterval(complete.activity[i,"interval"])
        flag = flag + 1
        }
    }

total.steps.per.days <- aggregate(steps ~ date, data = complete.activity, sum)
hist(total.steps.per.days$steps, col = "blue", xlab = "Total Number of Steps", 
     ylab = "Frequency", main = "Histogram of Total Number of Steps taken each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
## Are there differences in activity patterns between weekdays and weekends?



```r
complete.activity$day <- ifelse(as.POSIXlt(as.Date(complete.activity$date))$wday%%6 == 
                                    0, "weekend", "weekday")
complete.activity$day <- factor(complete.activity$day, levels = c("weekday", "weekend"))

steps.interval= aggregate(steps ~ interval + day, complete.activity, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = steps.interval, aspect = 1/2, 
       type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
