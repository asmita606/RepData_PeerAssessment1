---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
dat<-data.frame(read.csv("activity.csv",header=TRUE))
dat$date<-as.Date(dat$date,"%Y-%m-%d")
library(dplyr)
```


## What is mean total number of steps taken per day?


```r
dat_steps_day<-dat %>% group_by(date) %>% summarise (steps=sum(steps,na.rm=TRUE))
hist(dat_steps_day$steps,breaks=10,xlab="Total number of steps per day",main="Histogram of total number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
## The mean total number of steps taken per day
mean(dat_steps_day$steps)
```

```
## [1] 9354.23
```

```r
## The median total number of steps taken per day
median(dat_steps_day$steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?


```r
dat_5min<-dat %>% group_by(interval) %>% summarise(steps=mean(steps,na.rm=TRUE))
plot(dat_5min$interval,dat_5min$steps,type="l",pch=19,xlab="Time (in minutes)",ylab="Average number of steps",main="Time Series Plot of the average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

The 5-minute interval which contains the maximum number of steps on average across all the days in the dataset is


```r
dat_5min$interval[which.max(dat_5min$steps)]
```

```
## [1] 835
```



## Imputing missing values

Totsl number of rows with NAs is

```r
nrow(dat)-sum(complete.cases(dat))
```

```
## [1] 2304
```

Replacing NAs with the average number of steps across all days for the corresponding 5 min interval


```r
dat_noNA<-dat

for (i in 1:nrow(dat_noNA)){
        if(is.na(dat_noNA[i,"steps"])){
               st<-dat_5min$steps[which(dat_5min$interval==dat[i,"interval"])] 
               dat_noNA[i,"steps"]<-st
        }
}
```

Making a histogram of the total number of steps taken each day and computing
the mean and median total number of steps taken per day


```r
dat_noNA_steps_day<-dat_noNA %>% group_by(date) %>% summarise (steps=sum(steps,na.rm=TRUE))
hist(dat_noNA_steps_day$steps,breaks=10,xlab="Total number of steps per day",main="Histogram of total number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
## The mean total number of steps taken per day
mean(dat_noNA_steps_day$steps)
```

```
## [1] 10766.19
```

```r
## The median total number of steps taken per day
median(dat_noNA_steps_day$steps)
```

```
## [1] 10766.19
```


Yes the mean and median differ after replacing the NAs. The impact is the mean and median values have become the same.


## Are there differences in activity patterns between weekdays and weekends?



```r
library(lattice)

WeekDay<-function(x){
        if (weekdays(x) %in% c("Saturday","Sunday")){
                wd<-"weekend"
        }else{
                wd<-"weekday"
        }
        wd
}
dat_noNA$wd<-sapply(dat_noNA$date,WeekDay)
dat_noNA$wd<-as.factor(dat_noNA$wd)
dat_weekend<-subset(dat_noNA,wd=="weekend")
dat_weekday<-subset(dat_noNA,wd="weekday")
par(mfrow=c(2,1))

dat_5min<-dat_weekend %>% group_by(interval) %>% summarise(steps=mean(steps,na.rm=TRUE))
plot(dat_5min$interval,dat_5min$steps,type="l",pch=19,xlab="Interval",ylab="Average number of steps",main="Weekend")

dat_5min<-dat_weekday %>% group_by(interval) %>% summarise(steps=mean(steps,na.rm=TRUE))
plot(dat_5min$interval,dat_5min$steps,type="l",pch=19,xlab="Interval",ylab="Average number of steps",main="Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->










