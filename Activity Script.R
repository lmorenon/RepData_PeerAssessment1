####### Reproducible Research###
#### Project 1 ######
#### Made by Leonardo Moreno ######

### 1. Code for reading in the dataset and/or processing the data

#### Load Libraries

library(readr)
library(dplyr)
library(tidyr)
library(lattice)
library(rmarkdown)
library(dataMaid)
library(knitr)
library(lubridate)
library(prettydoc)

##### Load the Data Base

DB_activity <- read_csv("activity.zip")
View(DB_activity)
Sys.setlocale("LC_TIME", "English")


#### Add Group Variables

days <- weekdays(DB_activity$date)
DB_activity <- cbind(DB_activity,days)

### 2. Histogram of the total number of steps taken each day

steps_day <- DB_activity %>% group_by(date) %>% summarise(steps_per_day = sum(steps,na.rm=TRUE))
    

hist(steps_day$steps_per_day,main = "Total Steps taken per day", 
     xlab = "Steps per day")

mean_steps<-mean(steps_day$steps_per_day)
median_steps<-meadian(steps_day$steps_per_day)

### 3. What is the average daily activity pattern?

average_activity <- DB_activity %>% group_by(interval) %>% summarise(avg_activity = mean(steps,na.rm=TRUE))

plot(average_activity$interval, average_activity$avg_activity, type = "l", xlab="Interval (5 minutes)", 
    ylab="Average steps", main="Average steps per 5 minutes intervals")

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Max_steps <- average_activity[which.max(average_activity$avg_activity),1]
Max_steps

### 3. Imputing missing values

#### Calculate and report the total number of missing values in the dataset 
####(i.e. the total number of rows with NAs)

NAs<-sum(is.na(DB_activity$steps))
NAs

#### Imputate Method: Average steps (5 min-intervals) per day replaces NAs

steps_impute <- average_activity$avg_activity[match(DB_activity$interval,average_activity$interval)]

DB_act_imputate <-transform(DB_activity, steps = ifelse(is.na(DB_activity$steps), 
                                                     yes = steps_impute, no = DB_activity$steps))
steps_day_imp <- DB_act_imputate %>% group_by(date) %>% summarise(steps_per_day = sum(steps))

hist(steps_day_imp$steps_per_day,main = "Total Steps taken per day (Replaced NAs)", 
     xlab = "Steps per day")

#### Mean and Median total steps per day whit replaced NAs

mean_steps_replacedNAs<-mean(steps_day_imp$steps_per_day)

meadian_steps_replacedNAs<-median(steps_day_imp$steps_per_day)

####  Do these values differ from the estimates from the first part of the assignment?

t.test(steps_day$steps_per_day,steps_day_imp$steps_per_day)

#### What is the impact of imputing missing data on the estimates of the total daily number of steps?

### 4. Are there differences in activity patterns between weekdays and weekends?

####Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
####indicating whether a given date is a weekday or weekend day.


DB_act_days <- DB_activity %>% mutate(weekdays= ifelse(days=="Saturday" | 
                                      days=="Sunday", "Weekend", "Weekday"))
DB_act_days$weekdays <-  as.factor(DB_act_days$weekdays)

####Make a panel plot containing a time series plot of the 5-minute interval (x-axis) 
####and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

avg_steps_daytype <- DB_act_days %>% group_by(weekdays,interval) %>%  
                    summarise(avg_steps=mean(steps,na.rm = TRUE))

with(avg_steps_daytype, 
     xyplot(avg_steps ~ interval | weekdays, 
            type = "l",      
            main = "Average Steps per Intervals by dayType",
            xlab = "Daily Intervals",
            ylab = "Average Steps"))
