library(lubridate)

#Load the CSV File
Activity <- read.csv("/Users/sp/Downloads/rActivity/activity.csv", header = FALSE, sep=",", stringsAsFactors=FALSE)

#Fix table
names(Activity) <- Activity[1,]
Activity <- Activity[-1,]

#fix date
Activity$date <- as.Date(Activity$date, format = "%Y-%m-%d")

OriginalData <- Activity

#Omit NA values
Activity<- na.omit(Activity)

#Fix column type                 
Activity$steps <- as.numeric(Activity$steps)

#create a graph
TotalStepsDay <- aggregate(steps ~ date, data = Activity, FUN = sum)
hist(TotalStepsDay$steps,
     main = "Total number of steps taken each day",
     xlab = "Number of steps per day",
     col = "blue",
     breaks = 30)

MeanNumberSteps <- mean(TotalStepsDay$steps)
MedianNumberSteps <- median(TotalStepsDay$steps)

AverageStepsInterval <- aggregate(steps ~ interval, data = Activity, FUN = mean)
plot(x = AverageStepsInterval$interval,
     y = AverageStepsInterval$steps,
     type = "l",
     col = "blue",
     xlab = "The 5-minutes interval",
     ylab = "Average number of steps taken across all days",
     main = "The average daily activity pattern")


FiveMinInterval <- AverageStepsInterval$interval[which.max(AverageStepsInterval$steps)]

NASteps <- sum(is.na(OriginalData$steps))

Average5minInterval <- aggregate(steps ~ interval, data = OriginalData, FUN = mean, na.rm = TRUE)

FillNAData <- OriginalData

is_na_steps <- is.na(OriginalData$steps)
aux <- na.omit(subset(Average5minInterval, interval == OriginalData$interval[is_na_steps]))
FillNAData$steps[is_na_steps] <- aux[, 2]
na_steps_fillNA <- sum(is.na(FillNAData))

FillNAData$steps <- as.numeric(FillNAData$steps)

StepsDay_noNA <- aggregate(steps ~ date, data = FillNAData, FUN = sum, na.rm = TRUE)
hist(StepsDay_noNA$steps,
     main = "Total number of steps taken each day (without missing values)",
     xlab = "Number of steps per day",
     col = "blue",
     breaks = 30)


MeanStepsDay <- mean(StepsDay_noNA$steps)

MedianStepsDay <- median(StepsDay_noNA$steps)

summary(TotalStepsDay$steps)

summary(StepsDay_noNA$steps)

WeekdaysValues = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
date_type <- ifelse(
  weekdays(FillNAData$date) %in% WeekdaysValues,
  'weekday',
  'weekend')
FillNAData$day <- factor(x = date_type)


AverageStepsWeekday <- aggregate(steps ~ interval + day, data = FillNAData, FUN = mean, na.rm = TRUE)

AverageStepsWeekday$interval <- as.numeric(AverageStepsWeekday$interval)

library(ggplot2)
ggplot(AverageStepsWeekday, aes(interval, steps, color = day)) +
  geom_line() +
  facet_grid(day ~ .) +
  xlab('5-minute interval') +
  ylab('Average number of steps') +
  ggtitle('Activity pattern by the week of the day ')
