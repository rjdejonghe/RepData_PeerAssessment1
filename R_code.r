

### Code for Peer Assignment 1
### Reproducible Research, Coursera


### Notes - the time interval is hhmm, so there is no 1265 for instance.

#################### Code


### Read in data
data = read.csv("activity.csv")


### histogram of steps per day
hist(data$steps, col="red", xlab="Steps per Day")

### mean and median
mean(data$steps,na.rm=TRUE)
median(data$steps,na.rm=TRUE)

### time plot, type 'l', of steps per interval, averaged over days
# first compute the average
data$interval_average = tapply(data$steps,data$interval,mean,na.rm=TRUE)
#then plot, with 'l' style
plot(data$interval,data$interval_average,type="l",xlab="5 Minute Interval",ylab="Average Number of Steps")

### get interval with max daily average
max(data$interval_average)
max_int <- which.max(data$interval_average)
data[max_int,]

#####Imputing missing values

# find number of rows with missing values
nrow(data)
sum(is.na(data$steps))
sum(is.na(data$date))
sum(is.na(data$interval))

#create new data frame with NA steps values filled in, use interval average
data_fill = data
data_fill$steps[is.na(data$steps)] = data$interval_average
sum(is.na(data_fill$steps))

#histogram of new values
hist(data_fill$steps, col="red", xlab="Steps per Day")
mean(data_fill$steps,na.rm=TRUE)
median(data_fill$steps,na.rm=TRUE)

#function to determine if a given string is a weekday or a weekend
# the string should be something convertible to a date with as.date
is.weekday <- function(string){
  day <- weekdays(as.Date(string))
  return(day == "Monday" | day == "Tuesday" | day == "Wednesday" | day == "Thursday" | day == "Friday")
}

# creates a weekday/weekend factor column in "data" data frame,
# also add column with mean computed across weedays/weekends
data$day_type <- as.factor(ifelse(is.weekday(data$date),"weekday","weekend"))

data$int.day <- paste(data$interval, data$day_type, sep=".")
x <- tapply(data$steps, data$int.day, mean,na.rm="TRUE")
data$day_average <- x[data$int.day]

#plot steps for weekend and for weekday
par(mfrow=c(2,1))
with(data[data$day_type == "weekday",],plot(interval,day_average,type="l", xlab="5 Min. Interval (Weekday)", ylab="Average Number of Steps"))
title("Average Steps over 5 Minute Intervals, Weekday vs. Weeked")
with(data[data$day_type == "weekend",],plot(interval,day_average,type="l", xlab="5 Min. Interval (Weekend)", ylab="Average Number of Steps"))
     
