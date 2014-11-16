# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

First things first, I'll use read.csv to add data to variable x.


```r
x <- read.csv("activity.csv", colClasses = c("integer", "character", "integer"))
```

Next, the date will be converted to POSIXct format using the strptime command and stored in the column newDate.


```r
x$newDate <- as.POSIXct(strptime(x$date, '%Y-%m-%d'))
```

## What is mean total number of steps taken per day?
First, make a histogram of the total number of steps taken each day.  For this part of the assignment, missing values can be ignored.


```r
hist(x$steps, main = "Histogram of number of steps taken each day", xlab = "number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

In this next part of the assignment, the mean and median total number of steps is calculated and reported.  


```r
meanSteps <- mean(x$steps, na.rm = TRUE)
medianSteps <- median(x$steps, na.rm = TRUE)
```

The mean number of steps taken per day is 37.3825996 and the median number of steps taken is 0.
    
## What is the average daily activity pattern?

A time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) is created.  For this part of the assignment, I'm using the dplyr package and creating a new data frame named x2.


```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


```r
x2 <- summarize(group_by(x, interval), v = mean(steps, na.rm = TRUE))
plot(x2$interval, x2$v, type = "l", main = "Average Daily Activity Pattern", 
     xlab = "5-minute interval", 
     ylab = "average number of steps taken")
```

![](./PA1_template_files/figure-html/activityPattern-1.png) 

```r
maxInterval <- x2$interval[which.max(x2$v)]
```

The interval with the highest average number of steps is 835.

## Imputing missing values

### Calculate and report the total number of missing values in the dataset

```r
sumNA <- sum(is.na(x))
```

There are 2304 missing values in the data set.

My strategy for filling in all of the missing values in the dataset is to assign all missing values to zero.  Since all missing values are in the "steps" column, I'll create a new column called steps2 with all of the missing data filled in.


```r
x$steps2 <- x$steps
x$steps2[is.na(x$steps)] <- 0
meanSteps2 <- mean(x$steps2, na.rm = TRUE)
medianSteps2 <- median(x$steps2, na.rm = TRUE)
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(x$steps2, main = "Histogram of number of steps taken each day", xlab = "number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Now we can compare the impact of removing the missing values.  

In the data set with missing values, the mean was 37.3825996 and the median number of steps taken was ` r medianSteps`.  The lower mean value isn't surprising given my replacement strategy.

With the missing values removed, the mean is now 32.4799636 and the median ` r medianSteps2`.

## Are there differences in activity patterns between weekdays and weekends?

First, I'll create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
x$dayType <- as.character(x$newDate)
x$dayType[x$dayType %in% c("Saturday", "Sunday")] <- "weekend"
x$dayType[x$dayType %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")] <- "weekday"
```