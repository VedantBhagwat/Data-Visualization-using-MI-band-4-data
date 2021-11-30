# Import libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)

# Read csv files
activityData <- read.csv("MI/ACTIVITY/ACTIVITY_1614870830089.csv")
sleepData <- read.csv("MI/SLEEP/SLEEP_1614870831941.csv")
sportData <- read.csv("MI/SPORT/SPORT_1614870834859.csv")
heartrateData <- read.csv("MI/HEARTRATE_AUTO/HEARTRATE_AUTO_1614870834305.csv")

# EDA
str(activityData)
# Remove lastSyncTime column
activityData$lastSyncTime <- NULL
# Rename date column
colnames(activityData)[1] <- "date"
# Convert date column from chr to Date
activityData$date <- as.Date(activityData$date)
summary(activityData)

str(sleepData)
# Remove lastSyncTime column
sleepData$lastSyncTime <- NULL
# Rename date column
colnames(sleepData)[1] <- "date"
# Convert date column from chr to Date
sleepData$date <- as.Date(sleepData$date)
summary(sleepData)

# calculate total sleep minutes and store in new column
sleepData$totalSleepTime <- sleepData$deepSleepTime + sleepData$shallowSleepTime + sleepData$wakeTime
# Activity and Sleep data both have "date" as a common field
# Merge activity and sleep data
activity_and_sleep <- merge(x = activityData, y = sleepData, by = "date", all = TRUE)
# Get the days
activity_and_sleep$day <- weekdays(as.Date(activity_and_sleep$date))
str(activity_and_sleep)
activity_and_sleep$day <- as.factor(activity_and_sleep$day)

# weekdays and weekend
activity_and_sleep$dayType[activity_and_sleep$day == "Monday"] <- "Weekday"
activity_and_sleep$dayType[activity_and_sleep$day == "Tuesday"] <- "Weekday"
activity_and_sleep$dayType[activity_and_sleep$day == "Wednesday"] <- "Weekday"
activity_and_sleep$dayType[activity_and_sleep$day == "Thursday"] <- "Weekday"
activity_and_sleep$dayType[activity_and_sleep$day == "Friday"] <- "Weekday"
activity_and_sleep$dayType[activity_and_sleep$day == "Saturday"] <- "Weekend"
activity_and_sleep$dayType[activity_and_sleep$day == "Sunday"] <- "Weekend"

activity_and_sleep$dayType <- as.factor(activity_and_sleep$dayType)
str(activity_and_sleep)

# Calculate monthly sleep time of each sleep type
monthlySleep <- activity_and_sleep %>% group_by(month=floor_date(date, "month")) %>%
  summarize(monthly_deepSleepTime=sum(deepSleepTime),
            monthly_shallowSleepTime=sum(shallowSleepTime),
            monthly_wakeTime=sum(wakeTime))
# monthly_totalSleepTime=sum(totalSleepTime),

monthly_sleep_by_sleep_type <- melt(monthlySleep, id.vars = "month")

# Facet for monthly sleep
sleep_by_type <- melt(activity_and_sleep[,c("date","deepSleepTime","shallowSleepTime","wakeTime")], id.vars = "date")
# Get the month number
sleep_by_type$months <- month(ymd(sleep_by_type[,"date"]),label=T)
# Convert months to factor
sleep_by_type$months <- as.factor(sleep_by_type$months)

str(sportData)
# Rename and change "type" column to factor
colnames(sportData)[1] <- "type"
sportData$type <- as.factor(sportData$type)
# Change the sport type from number to its original sport type
sportData$sportType[sportData$type == 1] <- "Outdoor running"
sportData$sportType[sportData$type == 6] <- "Walking"
sportData$sportType[sportData$type == 8] <- "Treadmill"
sportData$sportType[sportData$type == 9] <- "Cycling"
sportData$sportType[sportData$type == 16] <- "Freestyle"

# Convert sport type to factor
sportData$sportType <- as.factor(sportData$sportType)
# Convert epoch to date object
sportData$date <- as.Date(as.POSIXct(sportData$startTime, origin="1970-01-01"))
summary(sportData)

str(heartrateData)
# Rename date column
colnames(heartrateData)[1] <- "date"
# Convert date column from chr to Date
heartrateData$date <- as.Date(heartrateData$date)
summary(heartrateData)

########## Activity Data Analysis ##########
# hist(activityData$steps)
plot1 <- ggplot(data = activity_and_sleep, mapping = aes(steps)) +
  labs(title = "Histogram of steps",
       x = "steps")
plot1 + geom_histogram()
  
# Scatter plot for Calories vs Steps
plot2 <- ggplot(data = activity_and_sleep,
                mapping = aes(x = calories, y = steps)) + #, color = day 
  labs(title = "Scatter plot for Calories vs Steps",
       x = "Calories(in kcal)",
       y = "Steps")
# plot2 + geom_point(color = "cornflowerblue",alpha = .7,
#                    size = 3)
plot2 + geom_point(color = "cornflowerblue",alpha = .7, size = 3) +
  geom_smooth(method = "lm")

# Calculate monthly steps
monthlySteps <- activity_and_sleep %>% group_by(month=floor_date(date, "month")) %>%
  summarize(total_steps=sum(steps),
            total_calories=sum(calories))

# par(mfrow=c(2,1))
# Plot 3: Monthlies bar plot for total steps
# plot3 <- barplot(monthlySteps$total_steps, xlab = "Months", ylab = "Total Steps")
plot3 <- ggplot(aes(x=month, y=total_steps,label=total_steps), data = monthlySteps) +
  labs(title = "Barplot of total monthly steps",
       x="Months", y="Steps")
plot3 + geom_bar(stat = 'identity', position = 'dodge') +
  # geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_x_date(date_breaks = "1 month", date_labels = "%B")

# Plot 4: Monthwise bar plot for total calories
# plot4 <- barplot(monthlySteps$total_calories, xlab = "Months", ylab = "Total Calories")
plot4 <- ggplot(aes(x=month, y=total_calories, lable=total_calories), data = monthlySteps) +
  labs(title = "Barplot of total monthly calories",
       x="Months", y="Calories")
plot4 + geom_bar(stat = 'identity', position = 'dodge') +
  # geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_x_date(date_breaks = "1 month", date_labels = "%B")


# df <- melt(monthlySteps, id = "month")
# ggplot() + geom_bar(data = df, aes(x = month, y = value, fill = variable), position = "dodge", stat = "identity")






# Plot 5: Scatter plot for steps and calories burned in each month
plot5 <- ggplot(data = monthlySteps,
                mapping = aes(x = total_calories, y = total_steps)) +
  labs(title = "Scatter plot for steps and calories burned in each month",
       x="Total Calories", y="Total steps")
plot5 + geom_point(color = "cornflowerblue",
             alpha = .7,
             size = 3)

# boxplot of calories burned on weekdays and weekends
ggplot(data = activity_and_sleep, aes(date,calories,color=dayType)) +
  labs(title = "Scatter plot of calories burned on weekdays and weekends",
       x="Days", y="Calories") +
  geom_boxplot()


########## Sleep Data Analysis ##########
# Plot 6: Scatter plot of sleeping time of each night
plot6 <- ggplot(data = activity_and_sleep, aes(date,totalSleepTime,color=day)) +
  labs(title = "Scatter plot of sleeping time of each night",
       x="Days", y="Sleep(in minutes)")
plot6  + geom_point()

# Plot 7: Scatter plot of sleeping time of each night
plot7 <- ggplot(data = activity_and_sleep, aes(date,totalSleepTime,color=dayType)) +
  labs(title = "Scatter plot of sleeping time on weekdays and weekends",
       x="Days", y="Sleep(in minutes)")
plot7 + geom_point()

# Plot 8: Boxplot of sleeping time of each night
plot8 <- ggplot(data = activity_and_sleep, aes(date,totalSleepTime,color=day)) +
  labs(title = "Boxplot of sleeping time of each night",
       x="Days", y="Sleep(in minutes)")
plot8 + geom_boxplot()

# Plot 9: Boxplot of sleeping time by weekdays and weekends
plot9 <- ggplot(data = activity_and_sleep, aes(date,totalSleepTime,color=dayType)) +
  labs(title = "Boxplot of sleeping time on weekdays and weekends",
       x="Days", y="Sleep(in minutes)")
plot9 + geom_boxplot()

# Boxplot for sleep type
plot10 <-ggplot(monthly_sleep_by_sleep_type, aes(variable, value)) +
  labs(title = "Boxplot for different sleep types",
       x ="Sleep types", y = "Time(in minutes)")
plot10 + geom_boxplot()

#Plot 11: Stack bar chart for monthly sleep analysis
plot11 <- ggplot(monthly_sleep_by_sleep_type, aes(fill=variable, y=value, x=month,label=value)) +
  ggtitle("Stack bar chart for monthly sleep analysis")
plot11 + geom_bar(position="stack", stat="identity")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%B")

ggplot() + geom_bar(data = monthly_sleep_by_sleep_type, aes(x = month, y = value, fill = variable), position = "dodge", stat = "identity") + 
  ggtitle("Stack bar chart for monthly sleep analysis") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "none")+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%B")

str(monthly_sleep_by_sleep_type)

# Plot 12: Facet wrap for all months sleep type 
plot12 <- ggplot(data = sleep_by_type,
                mapping = aes(x = variable, 
                              y = value)) +
  labs(title = "Facet wrap for all months sleep type",
       x="Sleep type", y="Sleep Time(in minutes)")
plot12 + geom_point(alpha = .7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  facet_wrap(~months)



########## Sport Data Analysis ##########

# Plot 14: Boxplot for different sport types
plot13 <-ggplot(sportData, aes(sportType, calories)) +
  labs(title = "Boxplot for different sport types",
       x ="Sport types", y = "calories")
plot13 + geom_boxplot()

# Plot 14: Scatter plot for Calories vs sportTime with lm model
plot14 <- ggplot(data = sportData,
                mapping = aes(x = calories,
                              y = sportTime,
                              color = sportType)) +
  labs(title = "Scatter plot of calories vs sport time",
       x="Calories", y="Time(in minutes)")
plot14 + geom_point(alpha = .7) +
  geom_smooth(method = "lm",
              se = FALSE)

# Plot 15: Facet wrap for Calories vs sportTime with lm model
plot15 <- ggplot(data = sportData,
                mapping = aes(x = calories, 
                              y = sportTime,
                              color = sportType)) +
  labs(title = "Facet wrap for Calories vs sportTime with lm model")
plot15 + geom_point(alpha = .7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  geom_smooth(method = "lm", 
              se = FALSE) +
  facet_wrap(~sportType)

# Plot 16: Scatter plot for Distance vs Calories coloured by different sport types
plot16 <- ggplot(data = sportData,
                 mapping = aes(x = distance,
                               y = calories,
                               color = sportType,
                               shape = sportType)) +
  labs(title="Scatter plot for Distance vs Calories",
          x="Distance(in km)", y="Calories")
plot16 + geom_point(alpha = .7)



########## Heart Rate Data Analysis ##########

# Plot 17: Histogram of Heart Rate
plot17 <- ggplot(mapping = aes(heartRate),
                 data = heartrateData) +
  labs(title = "Histogram of Heart Rate",
       subtitle = "Heart Rate has been autocaptured every second or two",
       x = "Heart Rate"
  )
plot17 + geom_histogram(binwidth = 10)
