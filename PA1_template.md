---
title: "PeerAssessment1"
output: html_document
---
### Introduction

This assignment makes use of data from a personal activity monitoring device. This
device collects data at 5 minute intervals through out the day. The data consists 
of two months of data from an anonymous individual collected during the months of
October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

This document presents the results of the analyses. A markdown document with the code,
related-text and results that is converted into this html page using R's knitr package.


#### Load packages

Load necessary packages and Set system options to show code and results in the 
output. Various packages have been used in an attempt to practice variety.

```r
library(knitr)
library(dplyr)
library(ggplot2)
library(lattice)
```


#### Read the activity dataset with the columns as numeric & character.

```r
opts_chunk$set(echo=TRUE, results="hold")
df <- read.csv("activity.csv", 
        na.strings="NA", 
        colClasses=c("numeric", "character", "numeric"))
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```
#### Process/transform the data into a format suitable for the analyses.
The date column is changed to date class and interval to factor.
Also displayed is the summary of the dataset.

```r
df$date <- as.Date(df$date, format = "%Y-%m-%d")
```

```
## Error in df$date: $ operator is invalid for atomic vectors
```

```r
dft<-df
df$interval <- as.factor(df$interval)
```

```
## Error in df$interval: $ operator is invalid for atomic vectors
```

```r
summary(df)
str(df)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     198     198     198     198     198     198 
##  int 198
```

#### Mean total number of steps taken per day

* Ignoring the missing values in the dataset, calculate the total number of steps
taken per day. Also included is the glimpse of the output data.

```r
total_steps <- aggregate(steps ~ date, df, sum)
```

```
## Error in eval(predvars, data, env): not that many frames on the stack
```

```r
head(total_steps)
```

```
## Error in head(total_steps): object 'total_steps' not found
```

* This histogram gives a pictorial representation of the total number of steps 
taken each day

```r
ggplot(total_steps, aes(x = steps)) + 
       geom_histogram(fill = "pink", binwidth = 1000) +
       labs(title="Histogram of Total Steps taken per Day", 
                x = "Total Steps per Day",
                y = "Frequency") +
       theme_bw() 
```

```
## Error in ggplot(total_steps, aes(x = steps)): object 'total_steps' not found
```

* The mean and median of the total number of steps taken per day are:

```r
MeanOfSteps   <- mean(total_steps$steps, na.rm=TRUE)
```

```
## Error in mean(total_steps$steps, na.rm = TRUE): object 'total_steps' not found
```

```r
paste("Mean of total number of steps taken per day = ", MeanOfSteps)
```

```
## Error in paste("Mean of total number of steps taken per day = ", MeanOfSteps): object 'MeanOfSteps' not found
```

```r
MedianOfSteps <- median(total_steps$steps, na.rm=TRUE)
```

```
## Error in median(total_steps$steps, na.rm = TRUE): object 'total_steps' not found
```

```r
paste("Median of total number of steps taken per day = ", MedianOfSteps)
```

```
## Error in paste("Median of total number of steps taken per day = ", MedianOfSteps): object 'MedianOfSteps' not found
```
#### Average daily activity pattern

* A time series plot of the 5-minute interval (x-axis) and the average number of
  steps taken, averaged across all days (y-axis).
  

```r
# Find average number of steps on all days grouped by the 5-minute interval
mean_steps <- df %>% group_by(interval) %>% summarize(Mean=mean(steps, na.rm=TRUE))
```

```
## Error in UseMethod("group_by_"): no applicable method for 'group_by_' applied to an object of class "c('integer', 'numeric')"
```

```r
# Plot the time series plot; have to convert the x variable, interval into 
#  integer in order to plot
ggplot(mean_steps, aes(x=as.integer(levels(interval)), y=Mean)) +   
       geom_line(color="steelblue", size=1) +  
       labs(title="Average Daily Activity Pattern",
            x="Interval",
            y="Average number of steps") +  
       theme_bw()
```

```
## Error in ggplot(mean_steps, aes(x = as.integer(levels(interval)), y = Mean)): object 'mean_steps' not found
```

* Which 5-minute interval, on average across all the days in the dataset, 
  contains the maximum number of steps?

```r
# what is the maximum number of steps
paste("Maximum = ", max(mean_steps$Mean))
```

```
## Error in paste("Maximum = ", max(mean_steps$Mean)): object 'mean_steps' not found
```

```r
# which 5-minute interval corresponds to this maximum number of steps
paste("5-minute interval corresponding to the maximum number of steps = ",
       mean_steps[which.max(mean_steps$Mean),]$interval)
```

```
## Error in paste("5-minute interval corresponding to the maximum number of steps = ", : object 'mean_steps' not found
```
The **835^th^** interval has maximum 206 steps.


===========================================================================================

## Imputing missing values

There are a number of days/intervals where there are missing values. The presence
of missing days may introduce bias into some calculations or summaries of the data.

* The total number of missing values in the dataset   

```r
paste("Total number of missing values in the dataset is", sum(is.na(dft$steps)))
```

```
## Error in dft$steps: $ operator is invalid for atomic vectors
```

* The strategy for filling in all of the missing values in the dataset is using 
  the  mean for that 5-minute interval. For instance, if there is a missing steps 
  value for the 250th interval, impute it with the mean steps of 250th interval.


```r
df_new <- data.frame()    # Create an empty dataframe
StepsFilled <- numeric()  # Create an empty numeric vector

# For each row in the original dataframe, check if that row has NA and then 
# replace with the corresponding steps value from mean_steps dataset
for (i in 1:nrow(dft)) {
        obs <- dft[i, ]
        if (is.na(obs$steps)) {
                steps <- mean_steps[mean_steps$interval==obs$interval,]$Mean
        } else {
                steps <- obs$steps
        }
        StepsFilled<- c(StepsFilled, steps)
        
}
```

```
## Error in 1:nrow(dft): argument of length 0
```

* Create a new dataset that is equal to the original dataset but with the missing
  data filled in.


```r
df_new <- cbind(dft, StepsFilled)  ## column bind the stepsfilled vector to the new dataframe
df_imputed <- df_new[, 2:4]       ## Remove the columns with steps as NAs
```

```
## Error in df_new[, 2:4]: subscript out of bounds
```

```r
names(df_imputed) <- c("date", "interval", "steps")
```

```
## Error in names(df_imputed) <- c("date", "interval", "steps"): object 'df_imputed' not found
```


* Histogram of the total number of steps taken each day 

```r
total_steps_imputed <- aggregate(steps ~ date, df_imputed, sum)
```

```
## Error in eval(expr, envir, enclos): object 'df_imputed' not found
```

```r
## Make a histogram of the total number of steps taken each day

ggplot(total_steps_imputed, aes(x = steps)) + 
        geom_histogram(fill = "purple", binwidth = 1000) +
        labs(title="Histogram of Total Steps taken per Day (After imputing with mean values)", 
             x = "Total Steps per Day",
             y = "Frequency") +
        theme_bw() 
```

```
## Error in ggplot(total_steps_imputed, aes(x = steps)): object 'total_steps_imputed' not found
```

* The mean and median total number of steps taken per day.

```r
MeanOfSteps   <- mean(total_steps_imputed$steps, na.rm=TRUE)
```

```
## Error in mean(total_steps_imputed$steps, na.rm = TRUE): object 'total_steps_imputed' not found
```

```r
paste("Mean of total number of steps taken per day = ", MeanOfSteps)
```

```
## Error in paste("Mean of total number of steps taken per day = ", MeanOfSteps): object 'MeanOfSteps' not found
```

```r
MedianOfSteps <- median(total_steps_imputed$steps, na.rm=TRUE)
```

```
## Error in median(total_steps_imputed$steps, na.rm = TRUE): object 'total_steps_imputed' not found
```

```r
paste("Median of total number of steps taken per day = ", MedianOfSteps)
```

```
## Error in paste("Median of total number of steps taken per day = ", MedianOfSteps): object 'MedianOfSteps' not found
```

 * Rounded values of mean and median from before and after the imputation are as 
 follows:

- **With missing values **
  *      Mean =  10766.19
  *      Median = 10765 
  
- **After imputation **
  *      Mean =  10766.19
  *     Median = 10766.19
  
 After imputation with the mean of the corresponding intervals, the mean & median
 are equal.
 
 =====================================================================================
 
### Are there differences in activity patterns between weekdays and weekends?

*   Using the dataset with the filled-in missing values, a new factor variable is
created denoting the day of the week corresponding to the date. The new factor 
variable called 'day' can have two levels Weekday or Weekend.


```r
df_imputed["day"] <- ""
```

```
## Error in df_imputed["day"] <- "": object 'df_imputed' not found
```

```r
for (i in 1:nrow(df_imputed))
{       if (weekdays(df_imputed[i,]$date) == "Saturday") 
        { df_imputed[i,]$day <- "Weekend"} 
        
        else if (weekdays(df_imputed[i,]$date) == "Sunday") 
        {df_imputed[i,]$day <- "Weekend"}
        
        else {df_imputed[i,]$day <- "Weekday"}
}
```

```
## Error in nrow(df_imputed): object 'df_imputed' not found
```

```r
df_imputed$day <- as.factor(df_imputed$day)
```

```
## Error in is.factor(x): object 'df_imputed' not found
```

* This panel plot containing a time series plot shows the 5-mininterval (x-axis)
and the average number of steps taken, averaged across all weekday days or 
weekend days (y-axis). 

```r
mean_steps_imputed <- df_imputed %>% group_by(interval, day) %>% summarize(Mean=mean(steps))
```

```
## Error in eval(expr, envir, enclos): object 'df_imputed' not found
```

```r
xyplot(Mean ~ interval | day, mean_steps_imputed,
        type = "l",
        layout = c(1, 2), 
        xlab = "Interval", 
        ylab = "Number of steps",
        main = " Panel plot of the time series")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'mean_steps_imputed' not found
```

The panel plot provides an easy way to spot the patterns of average number of steps per interval over the 
weekend vs over the weekdays.

=========================================================================================================


 


