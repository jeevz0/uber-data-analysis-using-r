library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse) 
library(DT)
library(scales)

apr_data <-("uber.raw.data.apr14")
 view(apr_data)
 apr_data = uber.raw.data.apr14
rm(uber.raw.data.apr14)


may_data <-("uber.raw.data.may14") 
   View(may_data)
 may_data = uber.raw.data.may14
rm(uber.raw.data.may14)


jun_data <-("uber.raw.data.jun14") 
View(jun_data)
jun_data = uber.raw.data.jun14
rm(uber.raw.data.jun14)



jul_data <-("uber.raw.data.jul14") 
View(jul_data)
jul_data = uber.raw.data.jul14
rm(uber.raw.data.jul14)


aug_data <-("uber.raw.data.aug14") 
View(aug_data)
aug_data = uber.raw.data.aug14
rm(uber.raw.data.aug14)

sep_data <-("uber.raw.data.sep14") 
View(sep_data)
sep_data = uber.raw.data.sep14
rm(uber.raw.data.sep14)


data_2014 <- rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)

head(data_2014)

str(data_2014)




data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format="%m/%d/%Y %H:%M:%S")





#summary statistics

summary(data_2014)


data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")


data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)
data_2014$day <- format(day(data_2014$Date.Time))
data_2014$month <- format(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- format(year(data_2014$Date.Time))
data_2014$dayofweek <- format(wday(data_2014$Date.Time, label=TRUE))


#hour minute second

data_2014$second = factor(second(hms(data_2014$Time)))
data_2014$minute = factor(minute(hms(data_2014$Time)))
data_2014$hour = factor(hour(hms(data_2014$Time)))


#visualization

hourly_data <- data_2014 %>% 
  group_by(hour) %>% 
  summarise(Total = n())


# Shows data in a searchable table
datatable(hourly_data)


# Plottting the data by hour
ggplot(hourly_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="green", 
           color="black") + 
  ggtitle("Trips Every Hour", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)


# Aggregating the data by month and hour
month_hour_data <- data_2014 %>% group_by(month, hour) %>%  summarise(Total = n())

ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)

sept_hour <- data_2014 %>%
   group_by(hour,month)  %>%
  filter(month == "sep") %>%
  summarise(Total = n())

ggplot(month_hour_data, aes(hour, Total, fill=hour)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month for SEPTEMBER") + 
  scale_y_continuous(labels = comma)


# Aggregating data by day of the month 
day_data <- data_2014 %>% group_by(day) %>% summarise(Trips = n())
day_data

# Plotting the data for the day
ggplot(day_data, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Trips by day of the month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)


#monthly trend
month_data <- data_2014 %>% group_by(month)  %>%  summarise(Total = n())
datatable(month_data)

#aug nd sep has highest rides

ggplot(month_data, aes(month, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Month") + 
  scale_y_continuous(labels = comma)



#analysing the bases

ggplot(data_2014, aes(Base)) +
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("TRIPS BY BASES")

#trips based on months & bases
ggplot(data_2014, aes(Base, fill =month)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("TRIPS BY BASES & MONTHS")


#creating map visualization of ride

min_lat <- 40 
max_lat <- 40.91
min_long <- -74.15
max_long <- -73.7004

ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")
