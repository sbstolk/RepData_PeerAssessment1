---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
The data was loaded from *activity.csv* using read.csv()
```{r}
setwd("/Users/blair_stolk/courseraR/repdata_data_activity/")
opts_chunk$set(echo = TRUE, results = 'hold')
rdata <- read.csv('activity.csv', header = TRUE, sep = ",",colClasses=c("numeric", "character", "numeric"))
```
The data was then cleaned and appropriately formatted. 

```{r}
rdata$date <- as.Date(rdata$date, format = "%Y-%m-%d")
rdata$interval <- as.factor(rdata$interval)
```

Used *str(rdata)* to check that the data is in the correct format. 

```{r}
str(rdata)
##'data.frame':	17568 obs. of  3 variables:
## $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
## $ date    : Date, format: "2012-10-01" "2012-10-01" "2012-10-01" ...
## $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

The data with the missing values replaced was then plotted as a histogram as before. 

```{r, echo=FALSE, message=FALSE, warning=FALSE}
fill_steps_per_day <- aggregate(steps ~ date, rdata_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")
qplot(fill_steps_per_day, aes(x = steps)) + 
    geom_histogram(fill = "blue", binwidth = 500) + 
    labs(title="Steps per Day", x = "Number of Steps per Day", y = "Number of times in a day(Count)")
```

```{r}
steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)

steps_mean_fill
[1] 10766.19

steps_median_fill
[1] 10766.19
```

## What is mean total number of steps taken per day?

The 

```{r}
steps_per_day <- aggregate(steps ~ date, rdata, sum)
> colnames(steps_per_day) <- c("date","steps")
> head(steps_per_day)
```

The result is the sum of steps grouped by day. 

```{r}
## date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

The next step is to make a histogram of this information. I chose *ggplot2* for plot creation. 

```{r}
ggplot(steps_per_day, aes(x = steps)) + 
    geom_histogram(fill = "blue", binwidth = 500) +
    labs(title="Steps per Day", x = "Number of Steps per Day", y = "Number of times in a day(Count)")
```

The histogram shows a distibution of steps. The mean and median are calculated as follows. 

```{r}
steps_mean   <- mean(steps_per_day$steps, na.rm=TRUE)
steps_median <- median(steps_per_day$steps, na.rm=TRUE)

steps_mean
[1] 10766.19

steps_median
[1] 10765
```

## What is the average daily activity pattern?

To calculate the number of steps over time, we need to slice the data into five-minute intervals. Also, it's easier to convert the numbers into integers. 

```{r}
steps_per_interval <- aggregate(rdata$steps,by = list(interval = rdata$interval),FUN=mean, na.rm=TRUE)
steps_per_interval$interval <- as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")
```

This data can now be used to create a plot of steps over time by five-minute-interval. Once again I have chosen to use ggplot2.

```{r}
ggplot(steps_per_interval, aes(x=interval, y=steps)) 
    geom_line(color="blue", size=1) 
    labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps")
```

To identify the inteval containing the greatest activity, we use the following.

```{r}
> max_interval <- steps_per_interval[which.max(steps_per_interval$steps),]
> max_interval
    interval    steps
104      835 206.1698
```

## Imputing missing values

To find how many missing values occur in the data set, we use *is.na()* and sum the count of results. 
```{r}
> missing_vals <- sum(is.na(rdata$steps))
> missing_vals
[1] 2304
```

One way to deal with the missing values is to replace them with the averages calculated earlier. 
```{r}
> na_fill <- function(data, pervalue) {
+     na_index <- which(is.na(data$steps))
+     na_replace <- unlist(lapply(na_index, FUN=function(idx){
+         interval = data[idx,]$interval
+         pervalue[pervalue$interval == interval,]$steps
+     }))
+     fill_steps <- data$steps
+     fill_steps[na_index] <- na_replace
+     fill_steps
+ }
> rdata_fill <- data.frame(
+     steps = na_fill(rdata, steps_per_interval),
+     date = rdata$date,
+     interval = rdata$interval)
> str(rdata_fill)
'data.frame':	17568 obs. of  3 variables:
 $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
 $ date    : Date, format: "2012-10-01" "2012-10-01" "2012-10-01" ...
 $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
 ```
 
 To check that the averages have propegated through and there are no missing values, I repeated the *is.na()* sum of missing values. 
 
```{r}
> sum(is.na(rdata_fill$steps))
[1] 0
```

Then I used thise numbers to create a histogram with ggplot2 as done in the previous section. 

```{r}
> fill_steps_per_day <- aggregate(steps ~ date, rdata_fill, sum)
> colnames(fill_steps_per_day) <- c("date","steps")
> ggplot(fill_steps_per_day, aes(x = steps)) + 
+ geom_histogram(fill = "blue", binwidth = 500) + 
+ labs(title="Steps per Day", x = "Number of Steps per Day", y = "Number of times in a day(Count)")
```

I then calculated the mean and median of the data with the missing values filled in. 

```{r}
> steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
> steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)
> steps_mean_fill
[1] 10766.19
> steps_median_fill
[1] 10766.19
```
The mean is the same in both instances, but the median is 1.19 steps higher in the second calculation (10766.19 instead of 10765). 

## Are there differences in activity patterns between weekdays and weekends?

For these calculations, I used the tables with the missing values filled. 
```{r}
> weekdays_steps <- function(data) {
+     weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),FUN=mean, na.rm=T)
+     weekdays_steps$interval <-  as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
+     colnames(weekdays_steps) <- c("interval", "steps")
+     weekdays_steps
+ }
> data_by_weekdays <- function(data) {
+     data$weekday <-as.factor(weekdays(data$date))
+     weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
+     weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))
+     weekend_steps <- weekdays_steps(weekend_data)
+     weekday_steps <- weekdays_steps(weekday_data)
+     weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
+     weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))
+     data_by_weekdays <- rbind(weekend_steps, weekday_steps)
+     data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
+ data_by_weekdays
+ }
> data_weekdays <- data_by_weekdays(rdata_fill)
```

From this I plotted a histogram. 

```{r,ggplot2,fig.width=4,fig.height = 3,echo=false}
library(ggplot2)
ggplot(data_weekdays, aes(x=interval, y=steps))
     geom_line(color="blue") 
     facet_wrap(~ dayofweek, nrow=2, ncol=1)
     labs(x="Interval", y="Number of steps")
```

