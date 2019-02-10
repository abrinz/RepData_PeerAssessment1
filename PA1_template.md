---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Show any code that is needed to

Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(tidyverse)
library(mice)
library(gridExtra)
```
#
Reading in the data

```r
activity <- read.csv('./activity.csv')
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Make a histogram of the total number of steps taken each day

Calculate and report the mean and median total number of steps taken per day


```r
totalSteps <- activity %>% group_by(date) %>% summarise (totalSteps = sum(steps,na.rm=TRUE))
ggplot(totalSteps,aes(x = date, y = totalSteps)) + geom_histogram(stat = 'identity')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
(meanSteps <- mean(totalSteps$totalSteps, na.rm = TRUE))
```

```
## [1] 9354.23
```

```r
(medianSteps <- median(totalSteps$totalSteps,na.rm = TRUE))
```

```
## [1] 10395
```
##
The mean number of steps taken per day is 9354.23
The median number of steps taken per day is 10395

##What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averageSteps <- activity %>% group_by(interval) %>% summarise(avg = mean(steps,na.rm = TRUE))
ggplot(averageSteps, aes(x = interval, y = avg)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
maxAvg <- max(averageSteps$avg)
filter(averageSteps, avg == maxAvg)
```

```
## # A tibble: 1 x 2
##   interval   avg
##      <int> <dbl>
## 1      835  206.
```
##
835 is the time interval with the maximum number of steps


##Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#1. Total # of missing values
totalMV <-sum(is.na(activity))
```


```r
#2. 
#I decided to fill in the missing values using the mice package
imputedValues <- mice(activity, m=5, maxit =10, method = 'pmm', seed = 975)
```

```
## 
##  iter imp variable
##   1   1  steps
##   1   2  steps
##   1   3  steps
##   1   4  steps
##   1   5  steps
##   2   1  steps
##   2   2  steps
##   2   3  steps
##   2   4  steps
##   2   5  steps
##   3   1  steps
##   3   2  steps
##   3   3  steps
##   3   4  steps
##   3   5  steps
##   4   1  steps
##   4   2  steps
##   4   3  steps
##   4   4  steps
##   4   5  steps
##   5   1  steps
##   5   2  steps
##   5   3  steps
##   5   4  steps
##   5   5  steps
##   6   1  steps
##   6   2  steps
##   6   3  steps
##   6   4  steps
##   6   5  steps
##   7   1  steps
##   7   2  steps
##   7   3  steps
##   7   4  steps
##   7   5  steps
##   8   1  steps
##   8   2  steps
##   8   3  steps
##   8   4  steps
##   8   5  steps
##   9   1  steps
##   9   2  steps
##   9   3  steps
##   9   4  steps
##   9   5  steps
##   10   1  steps
##   10   2  steps
##   10   3  steps
##   10   4  steps
##   10   5  steps
```

```r
#3. Filling in the missing values
activityImputed <- complete(imputedValues)

#4. Make a histogram of the total number of steps taken each day and 
#Calculate and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
summaryAI <- activityImputed %>% group_by(date) %>% summarise(totalSteps = sum(steps))

#Making the histogram
ggplot(summaryAI, aes(x=date, y=totalSteps)) +geom_bar(stat = 'identity')
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
(meanAISteps <- mean(summaryAI$totalSteps))
```

```
## [1] 9357.049
```

```r
(meanSteps)
```

```
## [1] 9354.23
```

```r
(median(summaryAI$totalSteps))
```

```
## [1] 10395
```

```r
(medianSteps)
```

```
## [1] 10395
```
##
With the imputed values, the mean is a little higher
The median is the same, even with imputed values
Imputing values increases the averages but doesn't change the median



##Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
#indicating whether a given date is a weekday or weekend day.
factor <- ifelse(weekdays(as.Date(activityImputed$date)) %in% c('Saturday','Sunday'),'weekend','weekday')
activityImputed2 <- activityImputed %>% mutate(typeOfDay = as.factor(factor))

#Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
dayVsEnd <- activityImputed2 %>% group_by(typeOfDay, interval) %>% summarise(avgSteps = mean(steps))
p1 <- dayVsEnd %>% filter(typeOfDay == 'weekend') %>% ggplot(aes(x=interval,y=avgSteps)) + geom_line() + ggtitle("Weekend")
p2 <- dayVsEnd %>% filter(typeOfDay == 'weekday') %>% ggplot(aes(x=interval,y=avgSteps)) + geom_line() + ggtitle("Weekday")
grid.arrange(p1,p2,nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
#Looking at the graph, the weekend is more active than the weekday but there's a big spike during the weekday
```


