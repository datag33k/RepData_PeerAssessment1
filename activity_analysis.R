# Data Science Specialization: Reproducible Research 012
# R script for project 1: data analysis on activity data

# load libraries
library(dplyr)
library(stats)
library(lubridate)

# LOAD AND PREPROCESS DATA
# download file if not in working directory
if (!file.exists("./activity.csv")) {
  if (!file.exists("./activity.zip") | !file.exists("./repdata-data-activity.zip")) {
    message("downloading data file...")
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "./activity.zip", method="curl")
    unzip("./activity.zip")
  }
  else if (file.exists("./activity.zip")) { 
    unzip("./activity.zip")
  }
  else if (file.exists("./repdata-data-activity.zip")) {
    unzip("./repdata-data-activity.zip")
  }
}

# load file
dat <- read.csv("./activity.csv", stringsAsFactors=FALSE)

# transform data to remove incomplete cases
dat1 <- dat[complete.cases(dat),]

# WHAT IS THE MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY
# mean total number of steps taken per day (ignore NAs in steps)
meanStepsByDay <- dat1 %>% group_by(date) %>% summarize(meansteps = mean(steps, na.rm=TRUE))

# plot frequency of mean totalSteps in a histogram
hist(meanStepsByDay$meansteps, main="Total Steps", xlab="Total Steps", col="green")
dev.print(png, file="plot1.png", width=480, height=480, pointsize=10)
dev.off()

# calculate and report mean total steps taken
print (paste("Mean steps per day:", mean(meanStepsByDay$meansteps)))

# calculate and report median total steps taken
print (paste("Median steps per day:", median(meanStepsByDay$meansteps)))

# WHAT IS THE AVERAGE DAILY PATTERN

# Calculate mean interval
meanInterval <- dat %>% group_by(interval) %>% summarize(meansteps = mean(steps, na.rm=TRUE))

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# does this need date in the y axis?
mistr <- meanInterval
mistr$interval <- as.POSIXlt(as.character(mistr$interval), tz="UTC", format="%H%M")
plot(mistr, type="l", main="Average Steps per Time Interval", xlab="Time Interval", ylab="Avg Steps")
dev.print(png, file="plot2.png", width=480, height=480, pointsize=10)
dev.off()

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
print (paste("Interval with the maximum average steps: ", meanInterval[meanInterval$meansteps==max(meanInterval$meansteps),1]))
# max interval 835
# max steps 206.1698

# INPUTTING MISSING VALUES

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
print (paste("Total # of rows with missing data: ", nrow(dat[is.na(dat)==TRUE,]))) #2304

# 2. Devise a strategy for filling in all of the missing values in the dataset.
# Missing values replaced with mean steps per interval (using day alone still leaves misssing values)
meanInterval <- dat %>% group_by(interval) %>% summarize(meansteps = mean(steps, na.rm=TRUE))

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# datNoNA: steps,date,interval
datNoNA <- dat
for (i in 1:nrow(datNoNA)) {  #2304 NAs
  if (is.na(datNoNA[i,1])==TRUE) {
    datNoNA[i,1] <- meanInterval[meanInterval$interval==datNoNA[i,3],2]   
  } 
}

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and
# median total number of steps taken per day. Do these values differ from the estimates from the first
# part of the assignment? What is the impact of imputing missing data on the estimates of the total daily
# number of steps?

hist(datNoNA$steps, main="Total Steps", xlab="Total Steps", col="blue")
dev.print(png, file="plot3.png", width=480, height=480, pointsize=10)
dev.off()

# calculate and report mean total steps taken
print (paste("Mean steps per day:", mean(datNoNA$steps)))

# calculate and report median total steps taken
print (paste("Median steps per day:", median(datNoNA$steps)))

# impact of imputing missing data
print ("Filling in missing values changes the median value from the original dataset, but not the mean. This makes sense given the nature of the mean and median functions.")


# DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
datNoNA[,4] <- factor(weekdays(as.Date(datNoNA$date)), levels=c("weekday","weekend"))

for (i in 1:nrow(datNoNA)) {
  wday <- weekdays(as.Date(datNoNA[i,2],"%Y-%m-%d"))
  
  if(wday == "Saturday" | wday == "Sunday") {
    datNoNA[i,4] <- "weekend"
  }
  else {
    datNoNA[i,4] <- "weekday"
  }
}
names(datNoNA) <- c("steps","date","interval","weekday")

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
#    the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the 
#    GitHub repository to see an example of what this plot should look like using simulated data.

# filter into weekday/weekend data
weekdayMean <- datNoNA %>% filter(weekday=="weekday") %>% group_by(interval) %>% summarize(meansteps = mean(steps))
weekendMean <- datNoNA %>% filter(weekday=="weekend") %>% group_by(interval) %>% summarize(meansteps = mean(steps))

weekdayMeanI <- weekdayMean
weekendMeanI <- weekendMean

# fix time intervals for plotting
#weekdayMeanI <- weekdayMean
#weekdayMeanI$interval <- as.POSIXlt(as.character(weekdayMeanI$interval), tz="UTC", format="%H%M")

#weekendMeanI <- weekendMean
#weekendMeanI$interval <- as.POSIXlt(as.character(weekendMeanI$interval), tz="UTC", format="%H%M")

# plot
par(mfrow=c(2,1), lab=c(12,10,5))
plot(weekdayMeanI, type="l", xlab="Time Interval", ylab="Average Steps", main="Weekend", col="green")
plot(weekendMeanI, type="l", xlab="Time Interval", ylab="Average Steps", main="Weekday", col="blue")
dev.print(png, file="plot4.png", width=480, height=480, pointsize=10)
dev.off()
