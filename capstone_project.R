
## loading some useful libraries
library(tidyverse)
library(dplyr)
library(janitor)
library(skimr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)


## importing the most important data tables
weight_loginfo <- read_csv("D:/Courses/Data Analysis/Fitabase _Data(04.12.2016-05.12.2016)/weightLogInfo_merged.csv")
daily_activity <- read_csv("D:/Courses/Data Analysis/Fitabase _Data(04.12.2016-05.12.2016)/dailyActivity_merged.csv")
sleep_day <- read_csv("D:/Courses/Data Analysis/Fitabase _Data(04.12.2016-05.12.2016)/sleepDay_merged.csv")
hourly_steps <- read_csv("D:/Courses/Data Analysis/Fitabase _Data(04.12.2016-05.12.2016)/hourlySteps_merged.csv")


## view data tables to have a glance
head(weight_loginfo)
head(daily_activity)
head(sleep_day)
head(hourly_steps)

## count ID distinct values
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(weight_loginfo$Id)

#Check for NA & duplicates
sum(is.na(daily_activity))
sum(is.na(sleep_day))
sum(is.na(weight_loginfo))

sum(duplicated(sleep_day))
sum(duplicated(daily_activity))
sum(duplicated(weight_loginfo))

## removing duplicates
sleep_day <- sleep_day[!duplicated(sleep_day$Id), ]

## inserting new column for weekdays
daily_activity <- daily_activity %>% 
  mutate(ActivityDays = weekdays(as.Date(ActivityDate,"%m/%d/%Y")))
head(daily_activity)

## merging three tables together by ID
joined_tibble <- merge(daily_activity, sleep_day, by = c("Id"), all = TRUE)
combined_data <- merge(joined_tibble, weight_loginfo, by = c("Id"),all = TRUE)

#Order from Monday to Sunday for plot later on
combined_data$ActivityDays <- factor(combined_data$ActivityDays, levels= c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))
combined_data[order(combined_data$ActivityDays), ]

## save as csv file
write_csv(combined_data, "combined_data.csv")

## check for duplicates and NA again
sum(is.na(combined_data))
sum(duplicated(combined_data))
n_distinct(combined_data$Id)

## check how many people have entered their weight manually
weight_loginfo %>% 
  filter(IsManualReport == TRUE) %>% 
  group_by(Id) %>% 
  summarize("Manual Weight Report"=n()) %>%
  distinct()

active_users <- daily_activity %>%
  filter(FairlyActiveMinutes >= 21.4 | VeryActiveMinutes>=10.7) %>% 
  group_by(Id) %>%
  summarize("COUNT"=n()) %>%
  distinct()
write.csv(active_users,"active users.csv")

## days with most activity (Tue,wed,Thur)
ggplot(data = combined_data, mapping = aes(x=ActivityDays))+ 
  geom_bar()+
  labs(title = "Acitivity during the week")

#by week
ggplot(data=combined_data, aes(x=ActivityDays, y=TotalSteps))+ 
  geom_bar(stat="identity")+
  labs(title="Total steps in the week")

ggplot(data=combined_data, aes(x=ActivityDays, y=Calories))+ 
  geom_bar(stat="identity", fill="#f01e2c")+
  labs(title="Burned calories")

ggplot(data=combined_data, aes(x=ActivityDays, y=TotalMinutesAsleep))+ 
  geom_bar(stat="identity")+
  labs(title="Total Minutes of sleep During the Week")

ggplot(data=combined_data, aes(x=ActivityDays, y=SedentaryMinutes))+ 
  geom_bar(stat="identity")+
  labs(title="Sedentary minutes in a week")

ggplot(data=combined_data, aes(x=ActivityDays, y=TotalDistance))+ 
  geom_bar(stat="identity")+
  labs(title="Total Distance covered")

#by hour
hourly_steps$ActivityHour=as.POSIXct(hourly_steps$ActivityHour,format="%m/%d/%Y %H:%M:%S %p")
hourly_steps$Hour <-  format(hourly_steps$ActivityHour,format= "%H")
head(hourly_steps)

ggplot(data=hourly_steps)+
  geom_bar(mapping = aes(x=Hour, y=StepTotal, fill=Hour),stat = "identity")+
  labs(title="Hourly Steps")

## summary for the combined data statistics
combined_data %>%
  dplyr::select(ActivityDays,
                TotalSteps,
                TotalDistance,
                VeryActiveMinutes,
                FairlyActiveMinutes,
                LightlyActiveMinutes,
                SedentaryMinutes,
                Calories,
                TotalMinutesAsleep,
                TotalTimeInBed,
                WeightPounds,
                BMI
  ) %>%
  summary()

## convert minutes to percentages for the pie chart
total_minutes <- round(sum(daily_activity$SedentaryMinutes, daily_activity$VeryActiveMinutes, daily_activity$FairlyActiveMinutes, daily_activity$LightlyActiveMinutes),1)
sedentary_percentage <- round(sum(daily_activity$SedentaryMinutes)/total_minutes*100,1)
lightly_percentage <- round(sum(daily_activity$LightlyActiveMinutes)/total_minutes*100,1)
fairly_percentage <- round(sum(daily_activity$FairlyActiveMinutes)/total_minutes*100,1)
active_percentage <- round(sum(daily_activity$VeryActiveMinutes)/total_minutes*100,1)

## pie chart of new data frame
percentage <- data.frame(
  level=c("Sedentary", "Lightly", "Fairly", "Very Active"),
  minutes=c(sedentary_percentage,lightly_percentage,fairly_percentage,active_percentage)
)

pie(percentage$minutes, percentage$minutes, main="active minutes distribution", col = rainbow(length(percentage$minutes)))
legend("topright", percentage$level,cex = 1, fill = rainbow(length(percentage$minutes)))

## total steps analysis
ggplot(data=combined_data, aes(x=TotalSteps, y = Calories, color=SedentaryMinutes))+ 
  geom_point()+ 
  stat_smooth(method=lm, formula = 'y~x')+
  scale_color_gradient(low="red", high="steelblue")

## active minutes vs calories
ggplot(data = combined_data) + 
  geom_point(mapping=aes(x=Calories, y=FairlyActiveMinutes), color = "green3", alpha = 1/2) +
  geom_smooth(method = loess,formula =y ~ x, mapping=aes(x=Calories, y=FairlyActiveMinutes, color=FairlyActiveMinutes), color = "green3", se = FALSE)+
  
  geom_point(mapping=aes(x=Calories, y=VeryActiveMinutes), color = "red3", alpha = 1/2) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=Calories, y=VeryActiveMinutes, color=VeryActiveMinutes), color = "red3", se = FALSE) +
  
  geom_point(mapping=aes(x=Calories, y=LightlyActiveMinutes), color = "darkorange", alpha = 1/2) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=Calories, y=LightlyActiveMinutes, color=LightlyActiveMinutes), color = "darkorange", se = FALSE) +
  
  geom_point(mapping=aes(x=Calories, y=SedentaryMinutes), color = "steelblue", alpha = 1/2) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=Calories, y=SedentaryMinutes, color=SedentaryeMinutes), color = "steelblue", se = FALSE) +
  
  annotate("text", x=4800, y=180, label="Very Active", color="black", size=4)+
  annotate("text", x=4800, y=70, label="Fairly Active", color="black", size=4)+
  annotate("text", x=4800, y=900, label="Sedentary", color="black", size=4)+
  annotate("text", x=4800, y=400, label="Lightly  Active", color="black", size=4)+
  labs(x = "Calories", y = "Active Minutes", title="Calories vs Active Minutes")

## sleep analysis
## determining the amount of wake up time in bed 
sleep_day <- mutate(sleep_day, AwakeTime = TotalTimeInBed - TotalMinutesAsleep)

## categorizing the sleep quality
sleep_data <-sleep_day %>%
  mutate(Status = case_when(AwakeTime < 10 ~ "Sleep Dept",
                            AwakeTime > 20 ~ "Under Pressure",
                            TRUE           ~ "Healthy Sleep")) 

sum(str_count(sleep_data$Status, "Sleep Dept"))
sum(str_count(sleep_data$Status, "Under Pressure"))
sum(str_count(sleep_data$Status, "Healthy Sleep"))

## convert sleep Status to percentage
sleep_data <- sleep_data %>% 
  group_by(Status) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

write_csv(sleep_data, "sleep_data.csv")


##plotting sleep category portions
ggplot(sleep_data, aes(x = "", y = perc, fill = Status)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer()+
  theme_void()

## check if burned calories and total sleep minutes are correlated
ggplot(data=combined_data, aes(x=TotalMinutesAsleep, y = Calories, color=TotalMinutesAsleep))+ 
  geom_point()+ 
  labs(title="Total Minutes asleep vs Calories")+
  xlab("Total Minutes Alseep")+
  stat_smooth(method=lm)+
  scale_color_gradient(low="orange", high="steelblue")
