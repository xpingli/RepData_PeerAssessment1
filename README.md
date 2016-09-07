---
title: "PA1_template"
author: "xp"
date: "September 7, 2016"
output: html_document
---

**Load packages**
```{r message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)
library(mice)
library(Hmisc)
```
#With Missing value
**Load Data**
```{r}
unzip("activity.zip")
act <- read.csv("activity.csv")
```
Transform the "date":
```{r}
act$date <- as.Date(act$date, "%Y-%m-%d")
```

__total number of steps taken per day?__
```{r}
sum_step <- act %>%
        group_by(date) %>%
        summarise( total_steps = sum(steps))

head(sum_step)
```

**histogram of total number of steps taken per day**
```{r}
ggplot(sum_step, aes(x = total_steps)) + geom_histogram(aes(y = ..density..),bins = 38) + geom_density()
```

**mean and median of the total number of steps taken per day**
```{r}
mean_step <- act %>%
        group_by(date) %>%
        summarise( avg_daily = mean(steps))
head(mean_step)
```
```{r}
median_step <- act %>% 
        group_by(date) %>%
        summarise( med_daily = median(steps))
head(median_step)

```

**Average of daily activities**
```{r}
time_series <- act %>%
        group_by(interval) %>%
        summarise(avg_steps = mean(steps, na.rm = TRUE))
head(time_series)


ggplot(time_series, aes( x =  interval, y = avg_steps)) + geom_line()
```


**interval having max steps across all the days**
```{r}
time_series[which(time_series$avg_steps == max(time_series$avg_steps)), ]

```

#Imputing Missing values

**numbers of missing values in the dateset and percentage**
```{r}
num_miss <- sum(is.na(act$steps))
num_miss

perc_miss <- mean(is.na(act$steps))
perc_miss
```

**impute the missing values with "Hmisc"" package**
```{r}
act_impute <- act

act_impute$steps <- with(act_impute, impute(steps, mean))

head(act_impute)

```

**histogram of total numbers of steps taken each day**
```{r}
sum_step_2 <- act_impute %>%
        group_by(date) %>%
        summarise(total_steps = sum(steps))

ggplot(sum_step_2, aes(x = total_steps)) + geom_histogram(aes(y = ..density..),bins = 38) + geom_density()

```
**mean and median of the total number of steps taken per day**
```{r}
mean_step2 <- act_impute %>%
        group_by(date) %>%
        summarise( avg_steps_daily = mean(steps))

head(mean_step2)

median_step2 <- act_impute %>%
        group_by(date) %>%
        summarise( med_steps_daily = median(steps))
head(median_step2)
```

##differences between "weekdays" and "weekends"



**create a variable containing 2 levels of factors "weekday" and "weekend"**

```{r}

act_impute$day <- weekdays(act_impute$date)
act_impute$wkdays <- vector(mode = "character", length = 17568)
for( i in 1:length(act_impute$day)){
        if( act_impute$day[i] == "Saturday" | act_impute$day[i] == "Sunday"){
                act_impute$wkdays[i] <- "weekend"
        } else {
                act_impute$wkdays[i] <- "weekday"
        }        

}

act_impute$wkdays <- as.factor(act_impute$wkdays)

```
**make a plot to compare "weekday" and "weekend" activiies**
```{r}

act_time_series <- act_impute %>%
        select(steps, interval, wkdays) %>%
        group_by(interval, wkdays) %>%
        mutate(avg_steps = mean(steps))

ggplot(act_time_series, aes(x = interval, y = avg_steps)) + geom_line() + facet_grid(wkdays~.)

```




