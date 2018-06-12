---
title: "PA1_template"
author: "Liliana Dimas"
date: "June 12, 2018"
output:
---

  # PPROJECT 1 "REPRODUCIBLE RESEARCH"
  
  ## ntroducción
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

  ## Assigment 
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

[Here you can download the data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

- **steps**: Number of steps taking in a 5-minute interval  
- **date**: The date on which the measurement was taken in YYYY-MM-DD format
- **interval**: Identifier for the 5-minute interval in which measurement was taken


#### Questions to be answered:

- What is mean total number of steps taken per day?
- What is the average daily activity pattern?
- Imputing missing values
- Are there differences in activity patterns between weekdays and weekends?
```{r}
knitr::opts_chunk$set(warning=FALSE)
```
### Library Requeriments 
```{r}
library(ggplot2)
library(plyr)
```
# Loading and preprocessing the data
```{r}
if (!file.exists("folder1"))
{
    dir.create("folder1")
    url <-  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url, destfile = "./folder1/activities.zip")
    setwd("./folder1")
    unzip("activities.zip")
}
# 1.1- Load the data in the file "activity.csv"
rawActivity <- read.csv("activity.csv")
summary(rawActivity, echo = TRUE)
# Processing the Data (Obtain te days with the date of rawActivity)
rawActivity$day <- weekdays(as.Date(rawActivity$date))
# Pongo en formato POSIXCT la fecha y lo llamo DateTime
rawActivity$DateTime <- as.POSIXct(rawActivity$date, format="%Y-%m-%d")
## pulling data without nas
dataClean <- rawActivity[!is.na(rawActivity$steps),]
```

# What is mean total number of steps taken per day?
```{r}
## summarizing total steps per date
sumDataFrame <- aggregate(rawActivity$steps ~ rawActivity$date, FUN = sum,)
# Asignarle nombre a cada colunda creada en la variable "sumDataFrame"
colnames(sumDataFrame) <- c("Date", "Steps")
# Make a histogram of the total number of steps taken each day
hist(sumDataFrame$Steps, breaks = 5, xlab = "Steps", main = "Total Steps per Day", col = "black", echo = TRUE)
```
```{r}
## Mean of Steps
meanSteps <- as.integer(mean(sumDataFrame$Steps))
meanSteps
## Median of Steps
medianSteps <- as.integer(median(sumDataFrame$Steps))
medianSteps
```
-The average number of steps taken each day was 10766 steps.

-The median number of steps taken each day was 10765 steps.

# What is the average daily activity pattern?
```{r}
## create average number of steps per interval
intervalDataFrame <- ddply(dataClean, .(interval), summarize, Avg = mean(steps))
```
```{r}
## Create line plot of average number of steps per interval
ggplot1 <- ggplot(intervalDataFrame, aes(x = interval, y = Avg), xlab = "Interval", ylab = "Average Number of Step" ) 
ggplot1 + geom_line() + xlab("Interval") + ylab("Average Number of Steps") + ggtitle("Average Number of Steps per Interval") 
```
```{r}
## Maximum steps by interval
stepsMax <- max(intervalDataFrame$Avg)
stepsMax
## Which interval contains the maximum average number of steps
maxIntervalSteps <- intervalDataFrame[intervalDataFrame$Avg == stepsMax,]
maxIntervalSteps
``` 
-The max interval of Steps is 835 

-The max Average per Day is 2016.1698

# Imputing Missing Values 
```{r}
# Number of NAs in original data set
totalNas <- rawActivity[,1]
totalNrow <- nrow(rawActivity[is.na(totalNas),])
totalNrow
```
The total number of rows with steps = ‘NA’ is 2304.
```{r}
# Create the average number of steps per weekday and interval
avgDataFrame <- ddply(dataClean, .(interval, day), summarize, Avg = mean(steps))
# Create dataset with all NAs for substitution
naData <- rawActivity[is.na(rawActivity$steps),]
# Merge NA data with average weekday interval for substitution
newData <- merge(naData, avgDataFrame, by = c("interval", "day"))
# Reorder the new substituded data in the same format as clean data set
newData2 <- newData[,c(6,4,1,2,5)]
colnames(newData2) <- c("steps", "date", "interval", "day", "DateTime")
# Merge the NA averages and non NA data together
dataMerge <- rbind(dataClean, newData2)
# Create sum of steps per date to compare with step 1
sumDataFrame2 <- aggregate(dataMerge$steps ~ dataMerge$date, FUN = sum, )
colnames(sumDataFrame2) <- c("Data", "Steps")
# Mean of Steps with NA data taken care of
meanStepsNa <- as.integer(mean(sumDataFrame2$Steps))
# Median of Steps with NA data taken care of
medianStepsNa <- as.integer(median(sumDataFrame2$Steps))
```

```{r}
## Creating the histogram of total steps per day, categorized by data set to show impact
hist(sumDataFrame2$Steps, breaks = 5, xlab = "Steps", main = "Total Steps per Day with Nas Fixed", col = "red")
hist(sumDataFrame$Steps, breaks = 5, xlab = "Steps", main = "Total Steps per Day with NAs Fixed", col = "gray", add = T)
```

# Are there differences in activity patterns between weekdays and weekends?
```{r}
## Create new category based on the days of the week
dataMerge$dayCategory <- ifelse(dataMerge$day %in% c("sabado", "domingo"), "Weekend", "Weekday")
library(lattice) 
intervalDataFrame2 <- ddply(dataMerge, .(interval, dayCategory), summarize, Avg = mean(steps))
```
```{r}
## Plot data in a panel plot
xyplot(Avg~interval|dayCategory, data=intervalDataFrame2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval", echo = TRUE)
```

