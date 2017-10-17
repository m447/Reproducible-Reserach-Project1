## Question1: download and load data

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "Data.zip")
unzip("Data.zip")
steps_data <- read.csv("activity.csv")

## the structure of the dataframe
str(steps_data)

## check for the NA values
any(is.na(steps_data))

## identify and quantify missing values 
nrow(steps_data[!complete.cases(steps_data),])
nrow(steps_data[!complete.cases(steps_data),])/nrow(steps_data)
# 0.1311475 percent of rows is considerable, cannot deleted

## imputing missing values: if two samples are similar, and one of them has an unknown value in some variable, 
## there is a high probability that this value is similar to the value of the other sample.
install.packages("DMwR")
library("DMwR")
steps_data <- knnImputation(steps_data,k=10,meth="median")

## Question2: histogram of steps taken each day 
library(dplyr)
steps_per_day <- group_by(steps_data,date)
steps_per_day <- summarise(steps_per_day,total=sum(steps))

steps_per_day <- group_by(steps_data,date)
steps_per_day <- summarise(steps_per_day,average=mean(steps,na.rm = T))

hist(steps_per_day$average,
        breaks = 15,
        main = "Average Number of Steps Taked Each Day",
        xlab = "Number of Steps"
        )

## Question3: mean and median number of steps taken each day
mean(steps_per_day$total)
median(steps_per_day$total)

## Question4: time series plot of average number of steps per 5 min interval during a day
avg_per_interval <- group_by(steps_data,interval)
avg_per_interval <- summarise(avg_per_interval,average=mean(steps))

library(ggplot2)
g <- ggplot(avg_per_interval,aes(x=interval,y=average))
g + geom_line() + ggtitle("Average Steps Per Interval During a Day") + labs(y="Average number of steps") + labs(x="Interval")


## Question 8: the average of steps per 5-minute interval accross weekdays and weekends
steps_data$date <- as.Date(steps_data$date,format="%Y-%m-%d")
steps_data$weekend <- sapply(steps_data$date,function(d) {as.POSIXlt(d)$wday}) %in% c(0,6)

weekday <- subset(steps_data,weekend==FALSE)
weekend <- subset(steps_data,weekend==TRUE)

weekday <- group_by(weekday,interval)
weekday <- summarise(weekday,average=mean(steps))

weekend <- group_by(weekend,interval)
weekend <- summarise(weekend,average=mean(steps))

weekday$id <- "weekday"
weekend$id <- "weekend"

steps_by_weekday <- rbind(weekday,weekend)

g <- ggplot(steps_by_weekday,aes(interval,average))
g + geom_line() + facet_grid(id~.) + 
        ggtitle("Average Number of Steps Per 5-min. Interval Accross Weekdays and Weekends") +
        labs(y="Average Number of Steps") + labs(x="Interval")



missing_steps_vector <- is.na(steps_data$steps)
mean(steps_data$steps, na.rm = T)
str(steps_data)
summary(steps_data)       
