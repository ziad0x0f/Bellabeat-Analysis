# Bellabeat Fitness Data Analysis 
![download-removebg-preview](https://user-images.githubusercontent.com/100311796/203800901-5e95fde7-97d5-4494-a758-d8151fa260bc.png)
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
:red_circle: **GOAL: Analyze Fitbit data to gain insight and help guide marketing strategy for Bellabeat to grow as a global player.**

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

* Check how many people have entered their weight manually
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

## 4. Analyze 
  
  
#### acitvity analysis through the day
according to the world health organization(WHO) Adults between the ages of 18 and 64 should engage in 75 to 150 minutes per week of intense exercise, or an equivalent combination of moderate and vigorous activity, which approximately translates to a daily goal of 21.4 fairly active minutes or 10.7 very active minutes(30 users in result).
```{r}
active_users <- combined_data %>%
  filter(FairlyActiveMinutes >= 21.4 | VeryActiveMinutes>=10.7) %>% 
  group_by(Id) %>%
  summarize("count"=n()) %>%
  distinct()
```


* Days with most activity 
```{r}
ggplot(data = combined_data, mapping = aes(x=ActivityDays))+ 
  geom_bar()+
  labs(title = "Acitivity during the week")
```
After Analyzing other activity variables like: Total Minutes of sleep During the Week, its found that most of the data is also collected in these three days 

![Activity_week](https://user-images.githubusercontent.com/100311796/194777525-76fc0804-dab0-4eb4-98c0-cd5aa38b7b64.png)
![Rplot01](https://user-images.githubusercontent.com/100311796/194777457-305cef76-bde3-469b-8cf4-5199c84e3d2d.png)


* Weekly Activity analysis
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
![Totaldistance](https://user-images.githubusercontent.com/100311796/194778669-4583d3d0-88f8-4de7-b8fd-7929902147ed.png)

There is a very distinguishable **decrease** in the total distance covered by users from saturday to sunday


* Summary for the combined data statistics to spot outliers
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
![Screenshot 2022-10-09 223234](https://user-images.githubusercontent.com/100311796/194778232-f6d6103b-86d6-4cb1-9441-6d6e57e27aa6.png)

The users spent 12 **Hours** in SedentaryMinutes average a day vs only 17 **Minutes** for FairlyActiveMinutes  


* Convert minutes to percentages for the pie chart
```{r}
total_minutes <- round(sum(daily_activity$SedentaryMinutes, daily_activity$VeryActiveMinutes, daily_activity$FairlyActiveMinutes, daily_activity$LightlyActiveMinutes),1)
sedentary_percentage <- round(sum(daily_activity$SedentaryMinutes)/total_minutes*100,1)
lightly_percentage <- round(sum(daily_activity$LightlyActiveMinutes)/total_minutes*100,1)
fairly_percentage <- round(sum(daily_activity$FairlyActiveMinutes)/total_minutes*100,1)
active_percentage <- round(sum(daily_activity$VeryActiveMinutes)/total_minutes*100,1)
```

* Pie chart of new data frame
```{r}
percentage <- data.frame(
  level=c("Sedentary", "Lightly", "Fairly", "Very Active"),
  minutes=c(sedentary_percentage,lightly_percentage,fairly_percentage,active_percentage)
)

pie(percentage$minutes, percentage$minutes, main="active minutes distribution", col = rainbow(length(percentage$minutes)))
legend("topright", percentage$level,cex = 1, fill = rainbow(length(percentage$minutes)))

```
![ActiveMinutes](https://user-images.githubusercontent.com/100311796/194778337-6adc0496-a08c-4546-8be9-f6691b76503f.png)

It's clear that the sedentary minutes is **dominating** the users lifestyle


* Total steps analysis
```{r}
ggplot(data=combined_data, aes(x=TotalSteps, y = Calories, color=SedentaryMinutes))+ 
  geom_point()+ 
  stat_smooth(method=lm, formula = 'y~x')+
  scale_color_gradient(low="red", high="steelblue")
```
![Rplot02](https://user-images.githubusercontent.com/100311796/194779239-1742123a-4d29-4a0f-91bc-18793dcd1542.png)

According to this linear regression model, it is expected that if the user has made more steps he will burn more calories. but, some users who spent more minutes in sedentary succeded to burn amount of calories which is approximatly or nearly equale to other users who spent their time in less sedentary minutes. this is **only valid at low amount of total steps(less than 4,000 step) 

* Active minutes vs calories
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
![Caloriesvsactive](https://user-images.githubusercontent.com/100311796/194780136-596382ea-be3b-434e-bdb2-93c98659611f.png)

The data is illustrating the relation between the Calories burned and Active minutes spent also, while describing the behaviour of these minutes(sedentary, lightly active, fairly active, very active). it states that the majority of the users burn in the range from 1,500 to 3,000 calorie. the regression model shows that The  sedentary line is going to level off near the end, while the fairly + very active line is reviving. Users who burn more calories spend less time sedentary and more time fairly + active.

#### Sleep analysis

* Determining the amount of wake up time in bed 
```{r}
sleep_day <- mutate(sleep_day, AwakeTime = TotalTimeInBed - TotalMinutesAsleep)
```
According to the Proffesor Jason Ellis in partnership with Northumbria University [Research](https://www.dovepress.com/articles.php?article_id=40952), It was revealed that how quickly we think we fall asleep, or'sleep latency,' is a very good indicator of our overall sleep health, followed by how long it took to fall asleep. According to the study, those who believe they fall asleep within 10 to 15 minutes of their head touching the pillow have better sleep health. Longer than 15 minutes and sleep health may be under pressure and if you fall asleep as soon as your head touches the pillow, it’s a sign you’re in sleep debt.

* Categorizing the sleep quality
```{r}
sleep_data <-sleep_day %>%
  mutate(Status = case_when(AwakeTime < 10 ~ "sleep dept",
                            AwakeTime > 20 ~ "under pressure",
                            TRUE           ~ "healthy sleep")) 

sleep_data <- sleep_data %>% 
  group_by(Status) %>% 
  mutate(n = n())
```

* Convert sleep Status to percentage
```{r}
sleep_data <- sleep_data %>% 
  group_by(Status) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))
```

* plotting sleep category portions
```{r}
ggplot(sleep_data, aes(x = "", y = perc, fill = Status)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer()+
  theme_void()
```
![PIE](https://user-images.githubusercontent.com/100311796/194875122-0e6d1751-7ac4-4cfd-9bf0-67f84b200e84.png)

The data indicates that most of our users are either under pressure or suffering sleep debt.


* check if burned calories and total sleep minutes are correlated

```{r}
ggplot(data=combined_data, aes(x=TotalMinutesAsleep, y = Calories, color=TotalMinutesAsleep))+ 
  geom_point()+ 
  labs(title="Total Minutes asleep vs Calories")+
  xlab("Total Minutes Alseep")+
  stat_smooth(method=lm)+
  scale_color_gradient(low="orange", high="steelblue")
```
![sleepvscalories](https://user-images.githubusercontent.com/100311796/194781723-9ed36a70-56f5-4929-b9de-acf95db1046e.png)

The plot of total minutes the users spent asleep and total calories that they had burned, illustrates that there is no correlation between both variables

## 5. Share

[Tableau Dashboard](https://public.tableau.com/views/BellaBeatDashboard/Dashboard1?:language=en-US&:display_count=n&:origin=viz_share_link)
![Screenshot (15)](https://user-images.githubusercontent.com/100311796/194781949-5fda6818-75e1-428b-81fb-66b1ca13aeb9.png)

## 5. Act

#### Conclusions:
  
  * 5 out of 8 users have recorded their weight manually which can cause some inconsistencies in the data
  * Sedentary activities account for 81% of users' daily active minutes. Users spend an average of 12 hours per day sedentary, 4 hours lightly active, and only a half-hour fairly+very active
  * Sunday is the least day in total steps and burned calories. so, it can be considered as the least active day
  * At low total steps people who take more time in sedentary still managed to burn a close amount of calories compared to users who spent less time in sedentary
  * only 29% of the users had a healthy sleep while other 71% had sleep deficiencies
 
#### Recommendations for Bellabeat Marketing Strategy:
 
 1. Encourage users to connect their phone with Bellabeat fit app to obtain more data and gaining more helpful insights for them
 2. Motivate more users to use their bluetooth or Wifi Digital scales to log their weight instead of manual log
 3. Adding bonus point system that can be redeemed with a sale percentage on certain bellabeat app services when the users complete more than 10,000 step per day, especially in lazy days like Sunday
 4. Making the wellness watch push notifications for the users to show their sleep analytics and recommendations for when to go to sleep and what time to wake up, by setting automaticaly an alarm for the recommended wake up time to avoid sleep deficiencies  
