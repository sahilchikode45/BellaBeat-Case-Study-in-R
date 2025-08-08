daily_activity <- read.csv("C:/Users/Admin/OneDrive/Desktop/PROJECTS/Bellabeat/Fitabase Data/dailyActivity_merged.csv")
sleep <- read.csv("C:/Users/Admin/OneDrive/Desktop/PROJECTS/Bellabeat/Fitabase Data/SleepDay_merged.csv")
weight <- read.csv("C:/Users/Admin/OneDrive/Desktop/PROJECTS/Bellabeat/Fitabase Data/weightLogInfo_merged.csv")
calories <- read.csv("C:/Users/Admin/OneDrive/Desktop/PROJECTS/Bellabeat/Fitabase Data/hourlyCalories_merged.csv")
intensities <- read.csv("C:/Users/Admin/OneDrive/Desktop/PROJECTS/Bellabeat/Fitabase Data/hourlyIntensities_merged.csv")

head(daily_activity)
glimpse(daily_activity)            

#Fixing formatting of date and time.
# intensities
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
# calories
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
# activity
daily_activity$ActivityDate=as.POSIXct(daily_activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
daily_activity$date <- format(daily_activity$ActivityDate, format = "%m/%d/%y")
# sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")
#weight
weight$Date=as.POSIXct(weight$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight$time <- format(weight$Date, format = "%H:%M:%S")
weight$date <- format(weight$Date, format = "%m/%d/%y")

skim_without_charts(daily_activity)
skim_without_charts(sleep)
skim_without_charts(weight)
skim_without_charts(calories)
skim_without_charts(intensities)

n_distinct(daily_activity$Id)     
n_distinct(sleep$Id)
n_distinct(weight$Id)
n_distinct(intensities$Id)
n_distinct(calories$Id)

nrow(sleep)                 
nrow(daily_activity)
nrow(weight)

daily_activity %>% select(Id, TotalSteps, TotalDistance, SedentaryMinutes) %>% 
  summary()              # Average Steps: 6547  Average Distance: 4.664   Average Sedentary Minutes :995.3 min. 
#Steps of users should increase, Average should be more than 8000 steps.
#Sedentary min. is 995.3 min. means 16 hours, it should be reduced 

avg_individal <- daily_activity %>% group_by(Id) %>% summarise(avg_steps = mean(TotalSteps),
                                                                   avg_distance = mean(TotalDistance),
                                                               avg_SedentaryActiveDistance = mean(SedentaryActiveDistance))
print(avg_individal, n = 35)         #Prints each row of the output

daily_activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()
#The majority of the participants are lightly active.

sleep %>% select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>% 
  summary()

weight %>% select(WeightKg, WeightPounds, BMI) %>% 
  summary()
weight %>% select(Id, WeightKg) %>% filter(WeightKg > 95)
weight %>% select(Id, WeightKg, BMI) %>% filter(BMI > 30)
weight %>% select(Id, WeightKg, BMI) %>% filter(BMI >= 25 & BMI < 29.9)
#20 users are normal weight, 13 are overwieght and 2 are obese.
#I recommend is that according to their bmi we should display there weight category in app.
# And if category is overwieght and obese we should notify to control diet and do exercise.

calories %>% select(Calories) %>% summary()

intensities %>% select(TotalIntensity) %>% summary()

#Merging daily activity and sleep for more analysis
combined_data <-  merge(sleep, daily_activity, by = 'Id', all = TRUE) %>% 
  select(-date.x, -date.y, -SedentaryActiveDistance)                       #all = True perfoms full outer join
head(combined_data)

n_distinct(combined_data$Id)

ggplot(data = combined_data, aes(x=TotalMinutesAsleep, y=TotalSteps)) + geom_point() +
  geom_smooth() + labs("Total Sleep vs. Total Steps",x='Total Sleep minutes' ,y='Total Steps')
#This chart indicates that people who sleep in the 6-7.5 hrs tend to take most steps, on average
# Most of users sleep between 5-10 hrs and steps between 5000-15000 steps.

ggplot(data = combined_data, aes(x=TotalSteps, y=Calories)) + geom_point(alpha = 0.5, color = 'dodgerblue') +
  geom_smooth() + labs("Total Steps vs. Calories",x='Total Steps', y='Calories Burned') + theme_minimal()
#As steps increase, more calories gets burns

ggplot(data = combined_data, mapping = (aes(x=TotalTimeInBed, y=TotalMinutesAsleep, colour = Id))) + geom_point() +
  geom_smooth() + labs("Total Sleep vs. Total Time in bed",x='Total Time in Bed', y='Total Sleep')
      
sleep_analysis <- combined_data %>% filter(!is.na(TotalTimeInBed)) %>% 
  mutate(SleepEfficiency = TotalMinutesAsleep / TotalTimeInBed * 100) %>%          
  arrange(Id, ActivityDate) %>% group_by(Id)                            # calculating sleep efficiency, arranging through id, date and grouping by Id. filter!is.na()) is used keep only rows where tot.. column has a number.
sleep_analysis 

ggplot(data = sleep_analysis, aes(x=SleepEfficiency, y = )) + geom_histogram(fill = 'skyblue', color = 'black') +
  labs(title = 'Sleeping Efficicency (Total Sleep / Total Time in Bed)')
# Understanding sleep quality of the users.
# Overall many users sleep quality is good and efficient sleep.

new_sleep_analysis <- sleep_analysis %>% mutate(Days0fWeek = factor(weekdays(SleepDay), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thrusday', 'Friday', 'Saturday')))

ggplot(data = new_sleep_analysis, aes(x=Days0fWeek, y=SleepEfficiency, fill = Days0fWeek)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = 'Sleep Efficency by Day of Weeks', x='Day of the Week', y='Sleep Efficiency') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

new_intensities <- intensities %>% mutate(HourofDay = as.numeric(format(ActivityHour, "%H")))

ggplot(data = new_intensities, aes(x=HourofDay, y=TotalIntensity)) +
  geom_bar(stat = "summary", fill = 'orange') +
  labs(title = 'Average Activity Intensity by Hour of the day', x='Hour of the day (0-23)', y='Average total Intensity')
# Calculating the average intensity of the user by every hour of the day.




