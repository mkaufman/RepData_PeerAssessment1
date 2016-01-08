## Section 1: Loading and preprocessing the data

## Load the data
activityDF <- read.csv("activity.csv", header=TRUE)

## Change class of the date column. Not sure this is necessary.
activityDF$date <- as.Date(activityDF$date, format = "%Y-%m-%d")

## Load dplyr as we will be using group_by
library(dplyr)

## Section 2: What is mean total number of steps taken per day?

## sum the steps per day/date. Also computed mean per day, but I don't think that's 
## what the instructions intended.
days_total_steps <- summarize(group_by(activityDF, date),
                              sum_steps = sum(steps, na.rm = FALSE),
                              mean_steps = mean(steps, na.rm = TRUE))

## alternate approach
sum_steps_day <- aggregate(steps~date,activityDF,sum, na.action=na.pass)
hist(sum_steps_day$steps, 
     main="Histogram for Total Steps in Day", 
     xlab="Total Daily Steps", 
     border="blue", 
     col="green",
     ylim=c(0,40),
     las=1, 
     breaks=5)


## Make a histogram of the total number of steps taken each day
hist(days_total_steps$sum_steps, 
     main="Histogram for Total Steps in Day", 
     xlab="Total Daily Steps", 
     border="blue", 
     col="green",
     ylim=c(0,40),
     las=1, 
     breaks=5)


## Calculate and report the mean and median of the total number of steps taken per day
total_mean <- mean(days_total_steps$sum_steps, na.rm = TRUE)
total_median <- median(days_total_steps$sum_steps, na.rm = TRUE)


## Section 3: What is the average daily activity pattern?

## Alternative Approach

mean_interval_daily_steps <- aggregate(steps~interval, activityDF, mean)

plot(mean_interval_daily_steps$interval,
     mean_interval_daily_steps$steps,
     type="l",
     main="Average Number of Steps Per 5-Minute Interval", 
     xlab="5-Minute Interval (0 through 2355)",
     ylab="Average Number of Steps"
)


mean_interval_daily_steps$interval[which.max(mean_interval_daily_steps$steps)]

## End Alternative Approach


interval_total_steps <- summarize(group_by(activityDF, interval),
                              sum_steps = sum(steps, na.rm = TRUE),
                              mean_steps = mean(steps, na.rm = TRUE))

## Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis)
## and the average number of steps taken, averaged across all days (y-axis)
plot(interval_total_steps$interval, interval_total_steps$mean_steps, type="l")

## Which 5-minute interval, on average across all the days in the dataset,
## contains the maximum number of steps?
interval_total_steps$interval[which.max(interval_total_steps$mean_steps)]



## Section 4: Imputing missing values

## Calculate and report the total number of missing values in the dataset
## (i.e. the total number of rows with 𝙽𝙰s)
sum(is.na(activityDF$steps))

## Using the mean for that 5-minute interval, will impute missing values in the 
## dataset
## Create a new dataset that is equal to the original dataset but with the missing
## data filled in.

imputedDF <- activityDF


## Alternative approach

## go through all rows of imputedDF. If steps is NA, then find value of mean steps
## for that interval and assign it in place of the NA in imputedDF.
for (i in 1:17568) {
        if (is.na(imputedDF$steps[i])) {
                imputedDF$steps[i] <- mean_interval_daily_steps[which(mean_interval_daily_steps$interval == imputedDF$interval[i]), ]$steps
        } 
}


## Make a histogram of the total number of steps taken each day
imputed_days_total_steps2 <- aggregate(steps~date, imputedDF, sum)
        

hist(imputed_days_total_steps2$steps, 
     main="Histogram for Total Steps in Day (with imputed values)", 
     xlab="Steps", 
     border="blue", 
     col="green",
     ylim=c(0,40),
     las=1, 
     breaks=5)

## End Alternative approach


## based on the value of the interval, I can get the mean steps for that interval.
## This can be removed. Just testing to use in the next section.
interval_total_steps[which(interval_total_steps$interval == "0"), ]$mean_steps

## go through all rows of imputedDF. If steps is NA, then find value of mean steps
## for that interval and assign it in place of the NA in imputedDF.
for (i in 1:17568) {
        if (is.na(imputedDF$steps[i])) {
                imputedDF$steps[i] <- interval_total_steps[which(interval_total_steps$interval == imputedDF$interval[i]), ]$mean_steps
        } 
}

## verifying the NA's got filled in. This can be removed.
sum(is.na(imputedDF$steps))

## Make a histogram of the total number of steps taken each day
imputed_days_total_steps <- summarize(group_by(imputedDF, date),
                              sum_steps = sum(steps, na.rm = TRUE),
                              mean_steps = mean(steps, na.rm = TRUE))

hist(imputed_days_total_steps$sum_steps, 
     main="Histogram for Total Steps in Day (with imputed values)", 
     xlab="Steps", 
     border="blue", 
     col="green",
     ylim=c(0,40),
     las=1, 
     breaks=5)

## Calculate and report the mean and median total number of steps taken per day.

## Alternative approach
imputed_total_mean2 <- mean(imputed_days_total_steps2$steps, na.rm = TRUE)
imputed_total_median2 <- median(imputed_days_total_steps2$steps, na.rm = TRUE)
## End Alternative approach

imputed_total_mean <- mean(imputed_days_total_steps$sum_steps, na.rm = TRUE)
imputed_total_median <- median(imputed_days_total_steps$sum_steps, na.rm = TRUE)



## Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels – “weekday” and
## “weekend” indicating whether a given date is a weekday or weekend day.

#create a vector of weekdays
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#Use `%in%` and `weekdays` to create a logical vector
#convert to `factor` and specify the `levels/labels`
imputedDF$wDay <- factor((weekdays(imputedDF$date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 


## Make a panel plot containing a time series plot (i.e. type = "l") of the
## 5-minute interval (x-axis) and the average number of steps taken, averaged
## across all weekday days or weekend days (y-axis).

## Alternative approach

aggImputedDf <- aggregate(steps~interval, imputedDF, mean, by = list(c("wDay")))

library(lattice)
xyplot(steps ~ interval | wDay, data = imputedDF, layout = c(1, 2), type="l")



imputedDF_weekday <- group_by(filter(imputedDF, wDay == "weekday"), interval)
weekday_interval_total_steps <- summarize(imputedDF_weekday, mean_steps = mean(steps, na.rm = TRUE))
weekday_interval_total_steps$wDay <- "weekday"

imputedDF_weekend <- group_by(filter(imputedDF, wDay == "weekend"), interval)
weekend_interval_total_steps <- summarize(imputedDF_weekend, mean_steps = mean(steps, na.rm = TRUE))
weekend_interval_total_steps$wDay <- "weekend"


complete <- rbind(weekday_interval_total_steps,weekend_interval_total_steps)

xyplot(mean_steps ~ interval | wDay, data = complete, layout = c(1, 2), type="l")


weekend_interval_total_steps <- summarize(group_by(filter(imputedDF, wDay == "weekend"), interval),
                                          sum_steps = sum(steps, na.rm = TRUE),
                                          mean_steps = mean(steps, na.rm = TRUE))

plot(weekday_interval_total_steps$interval, weekday_interval_total_steps$mean_steps, type="l")

plot(weekend_interval_total_steps$interval, weekend_interval_total_steps$mean_steps, type="l")