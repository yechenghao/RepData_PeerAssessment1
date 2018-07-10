# wk 2 assignment from Reproducible Research

#required libraries for this exercise
library(lubridate)
library(dplyr)
library(ggplot2)

# Loading and processing the data
activity<-read.table("activity.csv", sep=",", header=TRUE)
activity$date <- ymd(activity$date)

# what is the mean total number of steps taken per day?
# for this part of the assignment, you can ignore missing values in the dataset
# 1. calculate the total number of steps taken per day
# 2. make a histogram of the total number of steps taken per day
# 3. calculate and report the mean and median of the total number of steps taken per day

daily_step_summary <-activity %>% filter(!is.na(steps)) %>%
                        group_by(date) %>%
                          summarize(total_steps=sum(steps))

ggplot(data=daily_step_summary, aes(x=date, y=total_steps)) + geom_bar(stat="identity")
mean(daily_step_summary$total_steps)
median(daily_step_summary$total_steps)

# what is the average daily activity pattern?
# 1. Make a time series plot (type="l") of the 5-minute interval (x-axis) and the average number
# of steps taken, averaged across all days (y-axis)
# 2. which 5-minute interval, on average across all days in the dataset, contains the 
# maximum number of steps?

interval_step_summary<-activity %>% filter(!is.na(steps)) %>%
                          group_by(interval) %>% 
                            summarize(interval_mean_steps = ceiling(mean(steps)))

ggplot(data=interval_step_summary, aes(x=interval, y=interval_mean_steps)) + geom_line()

interval_step_summary$interval[which.max(interval_step_summary$interval_mean_steps)]
interval_step_summary$interval_mean_steps[which.max(interval_step_summary$interval_mean_steps)]

# calculate number of missing values
sum(is.na(activity$steps))

# imputing missing values
activity_copy<-activity
activity_copy$steps[is.na(activity$steps)] <- (activity %>% filter(is.na(steps)) %>% left_join(interval_step_summary, by="interval"))$interval_mean_steps


daily_step_summary_nafilled <-activity_copy %>%
                                group_by(date) %>%
                                  summarize(total_steps=sum(steps))

ggplot(data=daily_step_summary_nafilled, aes(x=date, y=total_steps)) + geom_bar(stat="identity")
mean(daily_step_summary_nafilled$total_steps)
median(daily_step_summary_nafilled$total_steps)

# Are there differences in activity pattern between weekdays and weekends?

activity_copy_wdays<-activity_copy %>% mutate(day_of_week = wday(date, label=TRUE)) %>%
  mutate(day_class = if_else(day_of_week=="Sat"| day_of_week=="Sun", "weekend", "weekday"))

activity_copy_wdays$day_class <- as.factor(activity_copy_wdays$day_class)

interval_step_summary_wday <- activity_copy_wdays %>% group_by(day_class, interval) %>% 
                                  summarize(interval_mean_steps = ceiling(mean(steps)))

ggplot(data=interval_step_summary_wday, aes(x=interval, y=interval_mean_steps)) + geom_line() + facet_grid(.~day_class)
