# Reproducible Research: Peer Assessment 1

Start with loading all the necessary libraries and setting global knitr options

```r
library(data.table)
library(dplyr)
library(lattice)
library(knitr)
opts_chunk$set(echo=TRUE)
```

## Loading and preprocessing the data

```r
if ( ! file.exists("activity.csv") ) {
    unzip("activity.zip")
}

# load the dataset, specifying proper classes
df <- fread("activity.csv", sep = ",", header = TRUE, 
            na.strings = "NA",
            colClasses = c('integer','factor','integer'),
            col.names = c('steps','date','interval')
            )

# take a pick at the data
head(df)
```

```
##    steps       date interval
## 1:    NA 2012-10-01        0
## 2:    NA 2012-10-01        5
## 3:    NA 2012-10-01       10
## 4:    NA 2012-10-01       15
## 5:    NA 2012-10-01       20
## 6:    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
# remove NA's
df_complete <- df[!is.na(df$steps), c("steps","date")]
# compute totals per day
totals_per_day <- aggregate(x = df_complete$steps, 
                           by = list(date = df_complete$date), FUN = sum)
# make column names meaningful
names(totals_per_day)[names(totals_per_day)=='x'] <- "day_totals"
# render the histogram
hist(totals_per_day$day_totals, 
     xlab = "Steps per day",
     main = "Total number of steps taken per day", col = "cyan")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# calculate aggregates
steps_mean <- mean(totals_per_day$day_totals)
steps_median <- median(totals_per_day$day_totals)
```
The *Mean* and *Median* of the toal number of steps taken per day:

> **Mean** -- 10766.19  
> **Median** -- 10765  

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# calculate totals by interval
int_agg <- aggregate(x = df$steps,
                     by = list(interval = df$interval), FUN = mean,
                     na.rm = TRUE)
names(int_agg)[names(int_agg)=='x'] <- "int_mean"
# render the plot
plot(int_agg$interval, int_agg$int_mean, type = 'l',
     xlab = "5-minute interval", ylab = "Average steps per day",
     main = "Average number of steps across all days", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# find the max index value
max_idx <- which.max(int_agg$int_mean)
max_int <- int_agg$interval[max_idx]
```
The 5-minute interval containing the *maximum* number of steps on average across all the days is: **835**.



## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# find the total number of missing values
na_flag <- is.na(df$steps)
na_rows <- sum(na_flag)
```
Total number of *missing* values in the dataset: **2304**

To impute the missing values we'll take the average of the:  
1. mean for that particular day, and  
2. mean of the 5-minute interval in question.  

For example, suppose the average number of steps taken on 2012-10-01 was **3500**, and the average of the specific 5-minute interval was **50**. Then the missing value would be calculated as follows:
( 3500 + 50 ) / 2
= **1775**


```r
# prepare totals per day, this time substituting NA with 0
df$steps <- ifelse(is.na(df$steps), 0, df$steps)
totals_per_day <- aggregate(x = df$steps, 
                           by = list(date = df$date), FUN = sum)
# make column names meaningful
names(totals_per_day)[names(totals_per_day)=='x'] <- "day_totals"
# combine averages per day with averages per interval:
avg_merge <- merge(totals_per_day, int_agg, all = TRUE) %>%
            mutate(grand_avg = (day_totals+int_mean)/2)
# fill in missing values
missing_subset <- df[na_flag,] %>% merge(avg_merge, by = c("date", "interval")) %>% 
    select(steps = grand_avg, date, interval)
# Final (full) data set, with imputted data:
df_complete <- rbind(df[!na_flag,], missing_subset) %>% arrange(date, interval)
# take a pick at the data
head(df_complete)
```

```
##        steps       date interval
## 1 0.85849057 2012-10-01        0
## 2 0.16981132 2012-10-01        5
## 3 0.06603774 2012-10-01       10
## 4 0.07547170 2012-10-01       15
## 5 0.03773585 2012-10-01       20
## 6 1.04716981 2012-10-01       25
```

```r
dim(df_complete)
```

```
## [1] 17568     3
```

```r
# calculate aggretates for the plot
totals_per_day <- aggregate(x = df_complete$steps, by = list(date = df_complete$date), FUN = sum)
names(totals_per_day)[names(totals_per_day)=='x'] <- "day_totals"
# render the histogram
hist(totals_per_day$day_totals, 
     xlab = "Steps per day",
     main = "Total number of steps taken per day (with imputted data)", col = "cyan")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# find out mean & median
steps_mean <- mean(totals_per_day$day_totals)
steps_median <- median(totals_per_day$day_totals)
```
The *Mean* and *Median* of the toal number of steps taken per day with *imputted* data:

> **Mean** -- 10060.21  
> **Median** -- 10395  

As a result of imputting missing data, both median and mean have **changed** -- they both shifted towards lower values. Before manipulation they used to be virtually the same, however after imputting missing data the mean became smaller than the median.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
# create a "weekday" column
df_complete <- df_complete %>%
    mutate(weekday = grepl("Sat|Sun", weekdays(as.Date(date), abbreviate = TRUE)))
# convert to factors
df_complete$weekday <- factor(df_complete$weekday, levels = c(FALSE, TRUE), 
                            labels = c('weekday','weekend'), ordered = TRUE)
# calculate the aggretates by interval and weekend/weekday
int_agg <- aggregate(x = df_complete$steps, 
                     by = list(interval = df_complete$interval, wkday = df_complete$weekday), 
                     FUN = mean)
names(int_agg)[names(int_agg)=='x'] <- "int_mean"
# render the plot using Lattice
xyplot(int_mean ~ interval | wkday, data = int_agg, type = 'l', 
       xlab = "Interval", ylab = "Number of steps", layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
