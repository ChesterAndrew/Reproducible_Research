
# load my librarys
library(ggplot2)
library(dplyr)

### Loading and preprocessing the data ###
activity <- read.csv("C:/Users/casel/Desktop/Coursera/Reproducible_Research/repdata_data_activity/activity.csv")
View(activity)
  ### View a portion of data and its structure###
  head(activity)
  ### Check basic statistics of the data ###
  summary(activity)
  ### Check the column names ###
  names(activity)
  # basic pairs plot not really necesary
  pairs(activity)

### What is mean total number of steps taken per day?###  
  ### 
  stepsDay <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)
  stepsDay
  ### Histogram of the total number of steps taken each day ## Q2
  g <- ggplot(stepsDay, aes(steps))
  g+geom_histogram(boundary=0, binwidth=2000, col="black",fill="blue")+ggtitle("Histogram Steps per Day")+xlab("Steps")+ylab("Frequency")

  ### Mean and Median of the total number of steps taken per day ## Q3
  MeanStepsPerDay <- mean(stepsDay$steps)
  MeanStepsPerDay
  MedianStepsPerDay <- median(stepsDay$steps)
  MedianStepsPerDay

###   What is the average daily activity pattern? ### Q4
  
  ### Variable with my Data SPT = Steps Pattern in Time
  SPT <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)

  ### Plot the result of Average Steps per Time Interval
  SPT
  g1 <- ggplot(SPT, aes(interval, steps))
  g1+geom_line(col="blue")+ggtitle("Average Steps per Time Interval")+xlab("Intervals")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))

  ###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  # MISteps = Maximum Interval Steps
  MISteps <- SPT[which.max(SPT$steps),]$interval
  MISteps  
  
###   Imputing missing values   ### Q5
  ###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
  
  TMValues <- sum(is.na(activity$steps))
  TMValues
  ###Devise a strategy for filling in all of the missing values in the dataset.   
  activity$fillSteps <- ifelse(is.na(activity$steps), round(SPT$steps[match(activity$interval, SPT$interval)],0), activity$steps)
  
  ### Create a new dataset that is equal to the original dataset but with the missing data filled in.
  activityNoMissingData <- data.frame(steps=activity$fillSteps, interval=activity$interval, date=activity$date)
  head(activityNoMissingData)  
  tail(activityNoMissingData)
  
  ### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
  SPDNM <- aggregate(activityNoMissingData$steps, list(activityNoMissingData$date), FUN=sum)
  head(SPDNM)
  colnames(SPDNM) <- c("Date", "Steps")
  head(SPDNM)
  g3 <- ggplot(SPDNM, aes(Steps))
  g3+geom_histogram(boundary=0, binwidth=2500, col="black", fill="blue")+ggtitle("Histogram Steps per Day")+xlab("Steps")+ylab("Frequency")
  
  ### Calculate and report the mean and median total number of steps taken per day. 
  mean(SPDNM$Steps)
  median(SPDNM$Steps)

###  Are there differences in activity patterns between weekdays and weekends? Q6
  
  ### Create variable with date in correct format
    activityNoMissingData$RealDate <- as.Date(activityNoMissingData$date, format = "%Y-%m-%d")
    head(activityNoMissingData)
  ### Create a variable with weekdays name
    activityNoMissingData$weekday <- weekdays(activityNoMissingData$RealDate)
    head(activityNoMissingData)
  ### create a new variable indicating weekday or weekend
    activityNoMissingData$DayType <- ifelse(activityNoMissingData$weekday=='Saturday' | activityNoMissingData$weekday=='Sunday', 'weekend','weekday')
  ### View and check our data
    head(activityNoMissingData)
    activityNoMissingData
  ###Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)  
  
    # Create variable with steps per time across week days // weekend days
    SPTDT <- aggregate(steps~interval+DayType,data=activityNoMissingData,FUN=mean)
    # variable time 
    SPTDT$time <- SPTDT$interval
    # draw the line plot
    g4 <- ggplot(SPTDT, aes(time, steps))
    g4+geom_line(col="blue")+ggtitle("Average Steps per Time Interval (Weekdays / Weekends Comparison)")+xlab("Time")+ylab("Steps")+facet_grid(DayType ~ .)
