---
title: "PA1_template"
author: "BRohde"
date: "Friday, July 17,2015"
output: html_document
---
##PA1_template.Rmd
##Assignment1 - Reproducible Research
This assignment makes use of data from a personal activity monitoring device such as Fitbit, Nike Fuelband, or Jawbone Up.  The device collects data at 5 minute intervals throughout the day.  The data consists of two months of data from an anonymous individual collected during the months of October and November of 2012 and include the number of steps taken in 5 minute intervals each day.

##Data

The data for this assignment can be downloaded from the course web site:
.Dataset: Activity monitoring data (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

.steps: Number of steps taken in a 5-minute interval (missing values are coded as NA)

.date: The date on which the measurement was taken in YYYY-MM-DD format

.interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

##Loading and preprocessing the data
```{r}
library(lattice)
library(knitr)
setwd("~/Data Science")
wm<-as.data.frame(read.csv(file="activity.csv", header = TRUE))
```
The field "data" is temporarily converted from a factor variable to a class of 'data' to assist in the calculation of minimum and maximum day in the dataset.  The 'tn_days' variable contains the total number of days for which data was collected.
```{r}
min_date <- min (as.Date(wm$date))
max_date <- max (as.Date(wm$date))
tn_days<-as.numeric(max_date - min_date)+1
```
Incomplete rows are removed from the original dataframe by applying the logical vector 'complete.cases(wm)' which has 'TRUE' for non-NA rows and 'False' if NA for all three columns.  Subtracting the number of rows after applying 'complete cases' from the original dataframe number of rows equals the number of rows with NAs.

```{r}
wm_no_NA <-wm[complete.cases(wm),]
numb_of_NAs<-nrow(wm)-nrow(wm_no_NA)
```
##What is mean total number of steps taken per day?
Using function 'tapply' the total number of steps per day is calculated.
```{r}
tn_stps_day<-as.data.frame(tapply(wm_no_NA$steps, INDEX=wm_no_NA$date, FUN = "sum", na.rm = TRUE))
colnames(tn_stps_day)<-"steps"
tn_stps_day$date<-as.Date(rownames(tn_stps_day))
tn_stps_day$steps<-as.integer(tn_stps_day$steps)
```
The mean and median number of steps per day is calculated.
```{r}
mean_no_NA<-mean(tn_stps_day$steps, na.rm = TRUE)
median_no_NA <- median(tn_stps_day$steps, na.rm =TRUE)
```
A histogram showing the frequency of 'total number of steps per day' is created with a red line indicated the mean and a blue dotted line indicating the median.
```{r histogram1, fig.height=4, echo=TRUE}
hist(tn_stps_day$steps, main = "Total Number of Steps Per Day (excluding missing values)",
     xlab = "Total Number of Steps Per Day", ylab = "Frequency (Number of Days)",
     breaks = 10, xlim = c(0, 25000), ylim = c(0,25), col = "green")
abline(v=mean_no_NA, col = c("red"))
abline(v=median_no_NA, col = c("blue"), lty = 2)
```

The mean for the total number of steps per day with the missing values removed is 10766.19 and the median is 10765.

##What is the average daily activity pattern?
Using the 'tapply' function the total number of steps in the interval is divided by the total number of days (excluding days without data).

The resulting array is converted to a dataframe, a new field 'interval' is added to the dataframe with the column name 'avg_steps' that contains the actual 5-minute time interval corresponding to the calculated means.  The entries for the 'interval' field are the rownames in the dataframe.
```{r}
mean_stps_per_intvl<- as.data.frame(tapply(wm_no_NA$steps, INDEX = wm_no_NA$interval, FUN="mean", na.rm=TRUE))
colnames(mean_stps_per_intvl)<-"avg_steps"
mean_stps_per_intvl$interval<-rownames(mean_stps_per_intvl)
is_max <- (mean_stps_per_intvl$avg_steps == max(mean_stps_per_intvl$avg_steps))
max_mean_stps_per_intvl<-mean_stps_per_intvl[is_max, ]
max_interval <- max_mean_stps_per_intvl[1,2]
```
The average number of steps (y-axis) versus the time interval(x-axis) is plotted in a line graph. A blue line indicates the interval with the maximum number of steps.
```{r Line graph, fig.height=4, echo=TRUE}
plot(mean_stps_per_intvl$interval,
     mean_stps_per_intvl$avg_steps, type = "l",
     xlab = "Five-Minute Time Interval", ylab= "Avg Number of Steps", main = "Average Number of Steps by Time Interval (excluding missing values)")
abline(v=max_interval, col = c("blue"))
```

The maximum number of steps per interval in average is 206.17 and corresponds to the interval 835.

##Imputing missing values
There are 2304 rows with missing values.  A new field 'steps_no_NAs' is created by going through the dataframe 'fill_in_NAs' by row.  This new field contains the number of steps without any missing values.  If a field is 'NA' the new column 'steps_no_NAs is populated with the average of the number of steps.  If there is an actual value in the original 'steps' field; it is populated into the new column.
A 'for' loop is used to populate the new column 'steps_no_NAs.
```{r}
fill_in_NAs <- (merge(mean_stps_per_intvl, wm, by = "interval"))
sort_order <- order(fill_in_NAs$date, as.numeric (fill_in_NAs$interval))
fill_in_NAS <- fill_in_NAs[sort_order, ]
for (i in (1:nrow(fill_in_NAs))){
     if (is.na(fill_in_NAs$steps[i])){
         fill_in_NAs$steps_no_NAs[i]<-fill_in_NAs$avg_steps [i]}
     else {
         fill_in_NAs$steps_no_NAs[i]<- fill_in_NAs$steps[i]
       }
   }
```
The total number of steps per day is calculated using 'tapply'.  The number of steps in the dataframe are summed using distinct values in the 'date' field as the INDEX variable.
```{r}
tn_stps_day_imput <- as.data.frame(tapply
  (fill_in_NAs$steps_no_NAs, INDEX = fill_in_NAs$date, 
    FUN = "sum", na.rm = TRUE))
colnames(tn_stps_day_imput) <- "steps"
tn_stps_day_imput$date <- as.Date(rownames(tn_stps_day_imput))
tn_stps_day_imput$steps <- as.integer(tn_stps_day_imput$steps)

```
The mean and median number of steps per day is calculated.
```{r}
mean_imput<-mean(tn_stps_day_imput$steps, na.rm = TRUE)
median_imput<-median(tn_stps_day_imput$steps, na.rm=TRUE)
```
A histogram is drawn showing the frequency of 'total number of steps per day' which include the populated NA values.  A red line indicates the mean and a blue dotted line indicates the median.
```{r histogram2, fig.height=4, echo=TRUE}
hist(tn_stps_day_imput$steps, main = "Total Number of Steps Per Day (imputing missing values)",
     xlab = "Total Number of Steps Per Day", ylab = "Frequency (Number of Days)",
     breaks = 10, xlim = c(0, 25000), ylim = c(0,25), col = "green")
abline(v=mean_imput, col = c("red"))
abline(v=median_imput, col = c("blue"), lty = 2)
```

The mean for the total number of steps per day is 10766.16 (missing values populated).  The original dataset mean with rows removed that had missing data was 10766.19; this indicates the missing data from the 2304 rows had little impact on the estimates of the total average daily number of steps.

The median for the total number of steps per day is 10766.  The original dataset median with rows removed that had missing data was 10765; this indicates the missing data from the 2304 rows had little impact on the estimates of the total average daily number of steps.

The frequency increases in the second histogram due to the added rows of data.

##Are there differences in activity patterns between weekdays and weekends?
A logical vector is created filled with TRUEs and FALSEs corresponding to the results of whether the first letter of the entry in the 'date' field (after the 'weekdays' function is applied) begins with the letter 'S'.  If it begins with an 'S' that day is a 'weekend' day otherwise it is a 'weekday'.  The entries in this logical vector are used to assign the word 'weekend' or 'weekday' to a newly-created column in the large dataframe calle 'fill_in_NAs'.  This new column is used as a 'factor' variable in the graph of the average number of steps by time interval.

```{r}
weekend_log <-grepl("^[Ss]", weekdays(as.Date(fill_in_NAs$date)))
for (i in (1:nrow(fill_in_NAs))){
  if (weekend_log[i] == TRUE){
    fill_in_NAs$day_of_week[i]<-"weekend"
  }else {
    fill_in_NAs$day_of_week[i]<-"weekday"
  }
}
```
The average number of steps taken by time interval is calculated separately for the weekend days and the weekday days.  This is accompished by using the 'aggregate' function with a list of two fields in the 'by' parameter.  A dataframe with three columns is produced; the first two columns are the 'by' variables 'interval' and 'day_of_week', respectively.  The third column contains the average number of steps per day corresponding to the column 1 and column 2 combination.  Following the 'aggregate' function the columns are renamed and sort the resulting dataframe by ascending time interval.
```{r}
mean_stps_per_intvl_imput <- aggregate(fill_in_NAs$steps_no_NAs, by = list(fill_in_NAs$interval, 
                                                                           fill_in_NAs$day_of_week), FUN = "mean", na.rm = TRUE)
colnames(mean_stps_per_intvl_imput) <- c("interval", "weekday_weekend", "avg_steps")

sort_order <- order(as.numeric(mean_stps_per_intvl_imput$interval))
mean_stps_per_intvl_imput <- mean_stps_per_intvl_imput[sort_order, ]
```
The 'aggregate' function is run again to calculate the mean number of average steps (to two decimal places) for both weekdays and weekends.  The first row contains the information for weekdays and the second for weekends.
```{r}

mean_by_day_type <- aggregate(fill_in_NAs$steps_no_NAs, by = list(fill_in_NAs$day_of_week), 
                              FUN = "mean", na.rm = TRUE)
mean_weekdays <- round(mean_by_day_type[1, 2], 2)
mean_weekends <- round(mean_by_day_type[2, 2], 2)
```
The lattice two-panel plot with one column and two rows is used to show Average Number of Steps by Time Interval (with NAs populated).  The 'weekday_weekend' column is treated as a 'factor' class variable to facilitate different plots.  The 'interval' field needs to contain numerical entries rather than class 'character'.
```{r lattice, fig.height=4, echo=TRUE}
xyplot(avg_steps ~ as.numeric(interval) | as.factor(weekday_weekend), data = mean_stps_per_intvl_imput, 
       type = "l", layout = c(1, 2), col = c("blue"), main = "Average Number of Steps by Time Interval (imputing missing values)", 
       xlab = "Five-Minute Time Interval", ylab = "Avg Number of Steps")
```
 
 The Average Number of Steps by Time Interval indicates more steps are taken on weekdays rather than weekends.