# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

As per the assignment we need to show below mentioned steps

Show any code that is needed to

* Load the data (i.e. read.csv())
* Process/transform the data (if necessary) into a format suitable for your analysis

### load the data

```r
actdata <- read.csv2("activity.csv", header = T, sep = ',', na.strings = NA)
```

Check value of loaded data

```r
head(actdata)
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
str(actdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(actdata)
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
As per Str result, date is not right date format, so before doing any activity we need to convert into right format


```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.3.2
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
actdata$date <- ymd(actdata$date)
str(actdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?
First we need to install dplyr package and get the dataset which has mean total no of steps taken per day

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.3.2
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
perDaySteps <- actdata %>% group_by(date) %>%
                  summarise(total.Steps = sum(steps, na.rm = T),
                            mean.Steps = mean(steps, na.rm = T))
head(perDaySteps)
```

```
## # A tibble: 6 × 3
##         date total.Steps mean.Steps
##       <date>       <int>      <dbl>
## 1 2012-10-01           0        NaN
## 2 2012-10-02         126    0.43750
## 3 2012-10-03       11352   39.41667
## 4 2012-10-04       12116   42.06944
## 5 2012-10-05       13294   46.15972
## 6 2012-10-06       15420   53.54167
```
Plot total steps per day using ggplot2

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
summary(perDaySteps$total.Steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

```r
dailyPlot <-ggplot(perDaySteps, aes(x= total.Steps)) 
dailyPlot + geom_histogram(binwidth = 2500) 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Get mean data 

```r
summary(perDaySteps$mean.Steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.1424 30.7000 37.3800 37.3800 46.1600 73.5900       8
```

```r
perDaySteps[is.na(perDaySteps)] <- 0
summary(perDaySteps$mean.Steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   23.53   36.09   32.48   44.48   73.59
```


## What is the average daily activity pattern?

For this part of the assignment, you can ignore the missing values in the dataset.

* Calculate the total number of steps taken per day
* If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of 
* steps taken each day
Calculate and report the mean and median of the total number of steps taken per day

Based on the previous data, get the data on basis of interval


```r
avgInterval <-  actdata %>% group_by(interval) %>% 
                summarize(mean.steps = mean(steps, na.rm = T))
summary(avgInterval)
```

```
##     interval        mean.steps     
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

```r
plotAvgInterval <-  ggplot(avgInterval, aes(x= interval, y = mean.steps))
plotAvgInterval + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Imputing missing values
* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
mean(is.na(actdata$steps))
```

```
## [1] 0.1311475
```

```r
sum(is.na(actdata$steps))
```

```
## [1] 2304
```
As per the output, its about 13% of steps related value is blank and total 2304 rows are does not have value 


* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.

there could be multipe way but here I am taking average interval steps to fill the data

```r
newActData <- actdata

for(i in 1:nrow(newActData))
{
  if(is.na(newActData$steps[i]))
  {
    index <- newActData$interval[i]
    value <- subset(avgInterval, interval == index)
    newActData$steps[i] <- value$mean.steps
  }
}
```

```r
head(newActData)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
head(actdata)
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

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

As per first steps, first we need to create same data then plot the histogram


```r
perDayStepsNew <- newActData %>% group_by(date) %>%
                  summarise(total.Steps = sum(steps, na.rm = T),
                            mean.Steps = mean(steps, na.rm = T))
head(perDayStepsNew)
```

```
## # A tibble: 6 × 3
##         date total.Steps mean.Steps
##       <date>       <dbl>      <dbl>
## 1 2012-10-01    10766.19   37.38260
## 2 2012-10-02      126.00    0.43750
## 3 2012-10-03    11352.00   39.41667
## 4 2012-10-04    12116.00   42.06944
## 5 2012-10-05    13294.00   46.15972
## 6 2012-10-06    15420.00   53.54167
```
now plot the same histogram

```r
perDaySteps$data <- 'old'
perDayStepsNew$data <-  'new'
bothActTogether <- rbind(perDaySteps,perDayStepsNew)

ggplot(bothActTogether, aes(total.Steps, fill = data)) + geom_histogram(alpha = 0.2)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


Here the difference between mean and medium 

```r
statDiff <- matrix(nrow=2, ncol=2, 
dimnames=list(c("oldSteps","newSteps"),c("mean","median")))

statDiff[1,1] = mean(perDaySteps$total.Steps, na.rm = T)
statDiff[1,2] = median(perDaySteps$total.Steps, na.rm = T)
statDiff[2,1] = mean(perDayStepsNew$total.Steps, na.rm = T)
statDiff[2,2] = median(perDayStepsNew$total.Steps, na.rm = T)
statDiff
```

```
##              mean   median
## oldSteps  9354.23 10395.00
## newSteps 10766.19 10766.19
```
As per the data, its seems very less difference 

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
newActData$day <- ifelse(weekdays(newActData$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

weekday <- newActData %>% filter(day == "weekday")
weekend <- newActData %>% filter(day == "weekend")
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
weekend <- weekend %>%
      group_by(interval) %>%
      summarize(mean.steps = mean(steps)) 
weekend$day <- "weekend"

weekday <- weekday %>%
      group_by(interval) %>%
      summarize(mean.steps = mean(steps)) 
weekday$day <- "weekday"

completeWeek <- rbind(weekday, weekend)

g <- ggplot (completeWeek, aes (interval, mean.steps))
g + geom_line() + facet_grid (day~.) + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
