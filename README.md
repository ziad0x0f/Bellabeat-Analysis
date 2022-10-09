the project is yet not completed!!

# Bellabeat Fitness Data Analysis 
##### Author: Ziad Zakaria 

##### Date: 2022-09-28

##### [Tableau Dashboard](https://public.tableau.com/views/BellaBeatDashboard/Dashboard1?:language=en-US&:display_count=n&:origin=viz_share_link)

#

#### The case study follows the six step data analysis process: ####

* [Ask](#1-ask)
* [Prepare](#2-prepare)
* [Process](#3-process)
* [Analyze](#4-analyze)
* [Share](#5-share)
* [Act](#6-act)
#

## Scenario
You are a junior data analyst working on the marketing analyst team at Bellabeat, a high-tech manufacturer of health-focused products for women. Bellabeat is a successful small company, but they have the potential to become a larger player in the global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opportunities for the company. You have been asked to focus on one of Bellabeat’s products and analyze smart device data to gain insight into how consumers are using their smart devices. The insights you discover will then help guide marketing strategy for the company. You will present your analysis to the Bellabeat executive team along with your high-level recommendations for Bellabeat’s marketing strategy.
#

## 1. Ask
:red_circle: **BUSINESS TASK: Analyze Fitbit data to gain insight and help guide marketing strategy for Bellabeat to grow as a global player.**

Primary stakeholders: Urška Sršen and Sando Mur, executive team members.

Secondary stakeholders: Bellabeat marketing analytics team.

## 2. Prepare 
Data Source: [FitBit Fitness Tracker Data](https://www.kaggle.com/arashnic/fitbit)

Dataset made available through [Mobius](https://www.kaggle.com/arashnic) 

The dataset doesn't follow the **ROCCC** approach
- Reliability: The can be considered as **Critically Reliable** becouse 30 users is relativly small to represent the population according to the central limit theorem  
- Original: The data is **Not Original**. 30 FitBit users who consented to the submission of personal tracker data via Amazon  Mechanical Turk.
- Comprehensive: the data is **Not Compehensive** as most of the data is collected during three days in the week
- Current:  the data is **Not Current**. Data is from March 2016 to May 2016(6 years from today)
- Cited: the data is **Not Cited**, it is collected through Kaggle user (Mobius) via Amazon  Mechanical Turk.

## 3. Process
### Loading used packages

```{r}
library(tidyverse)
library(dplyr)
library(janitor)
library(skimr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
```

###  Start cleaning process for the three tables after importing

* View data tables to have a glance
```{r}
head(weight_loginfo)
head(daily_activity)
head(sleep_day)
head(hourly_steps)
```

* Check for NA & Duplicates
```{r}
sum(is.na(daily_activity))
sum(is.na(sleep_day))
sum(is.na(weight_loginfo))

sum(duplicated(sleep_day))
sum(duplicated(daily_activity))
sum(duplicated(weight_loginfo))
```
We have **33** unique user with recorded daily activity, **24** users with sleep records and only **8** users with weight records

* check how many people have entered their weight manually
```{r}
weight_loginfo %>% 
  filter(IsManualReport == TRUE) %>% 
  group_by(Id) %>% 
  summarize("Manual Weight Report"=n()) %>%
  distinct()
```
**5** out of **8** users in total who entered their weight manually

* Removing duplicates
```{r}
sleep_day <- sleep_day[!duplicated(sleep_day$Id), ]
```

* Inserting new column for weekdays in Date formate
```{r}
daily_activity <- daily_activity %>% 
  mutate(ActivityDays = weekdays(as.Date(ActivityDate,"%m/%d/%Y")))
head(daily_activity)
```

* Merging three tables together by ID
```{r}
joined_tibble <- merge(daily_activity, sleep_day, by = c("Id"), all = TRUE)
combined_data <- merge(joined_tibble, weight_loginfo, by = c("Id"), all = TRUE)
```

* Order from Monday to Sunday for plot later on
```{r}
combined_data$ActivityDays <- factor(combined_data$ActivityDays, levels= c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))
combined_data[order(combined_data$ActivityDays), ]
```

* Save as csv file for tableau
```{r}
write_csv(combined_data, "combined_data.csv")
```

### Analyze 
  
  
#### acitvity analysis through the day
according to the world health organization Adults between the ages of 18 and 64
should engage in 75 to 150 minutes per week of intense exercise, or an equivalent combination of moderate and vigorous activity, which approximately translates to a daily goal of 21.4 fairly active minutes or 10.7 very active minutes(30 users in result).
```{r}
active_users <- combined_data %>%
  filter(FairlyActiveMinutes >= 21.4 | VeryActiveMinutes>=10.7) %>% 
  group_by(Id) %>%
  summarize("count"=n()) %>%
  distinct()
```


* days with most activity (Tue,wed,Thur)
```{r}
ggplot(data = combined_data, mapping = aes(x=ActivityDays))+ 
  geom_bar()+
  labs(title = "Acitivity during the week")
```

* by week
```{r}
ggplot(data=combined_data, aes(x=ActivityDays, y=TotalSteps))+ 
  geom_bar(stat="identity")+
  labs(title="Total steps in the week")

ggplot(data=combined_data, aes(x=ActivityDays, y=Calories))+ 
  geom_bar(stat="identity", fill="red")+
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
```



* summary for the combined data statistics
```{r}
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
```


* convert minutes to percentages for the pie chart
```{r}
total_minutes <- round(sum(daily_activity$SedentaryMinutes, daily_activity$VeryActiveMinutes, daily_activity$FairlyActiveMinutes, daily_activity$LightlyActiveMinutes),1)
sedentary_percentage <- round(sum(daily_activity$SedentaryMinutes)/total_minutes*100,1)
lightly_percentage <- round(sum(daily_activity$LightlyActiveMinutes)/total_minutes*100,1)
fairly_percentage <- round(sum(daily_activity$FairlyActiveMinutes)/total_minutes*100,1)
active_percentage <- round(sum(daily_activity$VeryActiveMinutes)/total_minutes*100,1)
```


* pie chart of new data frame
```{r}
percentage <- data.frame(
  level=c("Sedentary", "Lightly", "Fairly", "Very Active"),
  minutes=c(sedentary_percentage,lightly_percentage,fairly_percentage,active_percentage)
)

pie(percentage$minutes, percentage$minutes, main="active minutes distribution", col = rainbow(length(percentage$minutes)))
legend("topright", percentage$level,cex = 1, fill = rainbow(length(percentage$minutes)))

```

* total steps analysis
```{r}
ggplot(data=combined_data, aes(x=TotalSteps, y = Calories, color=SedentaryMinutes))+ 
  geom_point()+ 
  stat_smooth(method=lm, formula = 'y~x')+
  scale_color_gradient(low="red", high="steelblue")
```

* active minutes vs calories
```{r}
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
```

#### Sleep analysis

* determining the amount of wake up time in bed 
```{r}
sleep_day <- mutate(sleep_day, AwakeTime = TotalTimeInBed - TotalMinutesAsleep)

```
* categorizing the sleep quality
```{r}
sleep_data <-sleep_day %>%
  mutate(Status = case_when(AwakeTime < 10 ~ "sleep dept",
                            AwakeTime > 20 ~ "under pressure",
                            TRUE           ~ "healthy sleep")) 

sleep_data <- sleep_data %>% 
  group_by(Status) %>% 
  mutate(n = n())
```

* plotting sleep category portions
```{r}
ggplot(data = sleep_data, aes(x="", y=n, fill=Status)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  scale_fill_brewer(palette="Blues")+
  theme_void()
```



* check if burned calories and total sleep minutes are correlated

```{r}
ggplot(data=combined_data, aes(x=TotalMinutesAsleep, y = Calories, color=TotalMinutesAsleep))+ 
  geom_point()+ 
  labs(title="Total Minutes asleep vs Calories")+
  xlab("Total Minutes Alseep")+
  stat_smooth(method=lm)+
  scale_color_gradient(low="orange", high="steelblue")
```
