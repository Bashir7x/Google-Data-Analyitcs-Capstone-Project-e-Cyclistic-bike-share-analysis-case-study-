# Load Packages
library(readr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)
library(skimr)
library(janitor)
library(xlsx)
library(readxl)


#loading files to a variable

cs<-dir("C:/Users/bashi/Desktop/trip data 2/CSV Files/", full.names=T)
Trip_data <- map_df(cs, read_csv, col_types = cols(start_station_id = col_character(),end_station_id = col_character(),started_at=col_datetime(),ended_at=col_datetime()))
write.csv(Trip_data,"C:/Users/bashi/Desktop/trip data 2/CSV Files/combo.csv")

#removing NAs from the data
new_data<-Trip_data %>% na.omit()
write.csv(new_data,"C:/Users/bashi/Desktop/trip data 2/XLS/2020-2021_divvy-tripdata_full.csv")

#removing duplicates

sample_data <- sample_data[!duplicated(sample_data$ride_id), ]
print(paste("Removed", nrow(sample_data) - nrow(sample_data), "duplicated rows"))


#sampling the data


sample_data <- sample_n(new_data,16009,replace = FALSE)
write.csv (sample_data,"C:/Users/bashi/Desktop/trip data 2/XLS/2020-2021_divvy-tripdata_sample3.csv")

#mor cleaning done in excel refer to the cleanig process report

sample_data<- read_xlsx("C:/Users/bashi/Desktop/trip data 2/XLS/sample.xlsx")

sample_data <- sample_data %>%    
  clean_names() %>%    
  unique()
#_____Start of analysis_____



# Inspect the new table that has been created
colnames(sample_data)  #List of column names
nrow(sample_data)  #How many rows are in data frame?
dim(sample_data)  #Dimensions of the data frame?
head(sample_data)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(sample_data)  #See list of columns and data types (numeric, character, etc)
summary(sample_data)  #Statistical summary of data. Mainly for numerics

#convert ride_length to mintues by first taking the time 


sample_data <- sample_data %>% mutate(start_time= paste(strftime(sample_data$started_at, "%H"),
                                                         ":",strftime(sample_data$started_at, "%M"),":",
                                                        strftime(sample_data$started_at, "%S")))

sample_data <- sample_data %>% mutate(end_time= paste(strftime(sample_data$ended_at, "%H"),
                                                        ":",strftime(sample_data$ended_at, "%M"),":",
                                                        strftime(sample_data$ended_at, "%S")))

sample_data <- sample_data %>% mutate(ride_time = as.numeric(sample_data$end_time - sample_data$start_time) )



summary(sample_data$ride_time)

#separating year and month
sample_data <- sample_data %>% mutate(year_month = paste(strftime(sample_data$started_at, "%Y"),
                                             "-",strftime(sample_data$started_at, "%m"),
                                             paste("(",strftime(sample_data$started_at, "%b"), ")", sep="")))
unique(sample_data$year_month)


#filter ride_length
sample_data<-sample_data %>% filter(ride_length>0)
print(paste("Removed", nrow(sample_data) - nrow(sample_data), " rows"))

summary(sample_data$ride_length)

# Comparing members vs casual users
comp1<-
  aggregate( sample_data$ride_length ~ sample_data$member_casual , FUN = mean)
view(comp1)
comp2 <-
  aggregate(sample_data$ride_length ~ sample_data$member_casual, FUN = median)
view(comp2)
comp3<-
  aggregate(sample_data$ride_length ~ sample_data$member_casual, FUN = max)
view(comp3)
comp4<-
    aggregate(sample_data$ride_length ~ sample_data$member_casual, FUN = min)
view(comp4)

# Daily average ride time for members vs casual users
comp5 <- aggregate(sample_data$ride_length ~ sample_data$member_casual + sample_data$day_of_week, , FUN = mean)
view(comp5)

#counting the number of rides each type of member takes and which type of ride

Comp6<- sample_data %>% 
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%  
  group_by(rideable_type, day_of_week, member_casual) %>%  
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%    
              arrange(rideable_type, day_of_week,member_casual)
view(comp6)

comp7<-mode(sample_data$day_of_week)
view(comp7)
#creating a viz of no of rides by ride type

sample_data %>% 
  mutate(day_of_week= wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = median(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
sample_data %>% 
  mutate(day_of_week = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, month.name,bike_id) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_time)) %>% 
  arrange(member_casual, day_of_week,bike_id)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = bike_id)) +
  geom_col(position = "dodge")

counts <- aggregate(sample_data$ride_length ~ sample_data$member_casual + sample_data$day_of_week, FUN = mean)

#no of casual and member users
comp8<-sample_data %>% group_by(member_casual) %>% summarise(count = length(ride_id),
                                                'Percentage' = (length(ride_id) / nrow(sample_data)) * 100)
view(comp8)
#now we export the results of our analysis
write.csv(comp5,"C:/Users/bashi/Desktop/trip data 2/XLS/avrage ride length.csv")
