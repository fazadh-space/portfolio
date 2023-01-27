---
title: "Cyclistic R Analysis"
author: "Faza Dhiyaulhaq"
date: "2023-01-22"
output: html_document
---

# KAMPANYE TERBAIK ANNUAL MMEMBERSHIP CYCLISTIC 


## Menyiapkan Package yang dibutuhkan

```{r install_packages,error=TRUE, cache=TRUE}
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("readr")
install.packages("lubridate")
```

### Library
```{r}
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readr)
```


## Menyatukan data yang dibutuhkan

```{r eval=FALSE}
#Q4 2021 Data Import
tripdata_202110 <- read.csv("202110-divvy-tripdata.csv")
tripdata_202111 <- read.csv("202111-divvy-tripdata.csv")
tripdata_202112 <- read.csv("202112-divvy-tripdata.csv")

#Q4 2021 Combine Data
#Check the Data
colnames(tripdata_202110)
colnames(tripdata_202111)
colnames(tripdata_202112)

str(tripdata_202110)
str(tripdata_202111)
str(tripdata_202112)

q4_2021 <- bind_rows(tripdata_202110,tripdata_202111,tripdata_202112)


#Q1 2022 Data Import
tripdata_202201 <- read.csv("202201-divvy-tripdata.csv")
tripdata_202202 <- read.csv("202202-divvy-tripdata.csv")
tripdata_202203 <- read.csv("202203-divvy-tripdata.csv")

#Q1 2022 Combine Data
q1_2022 <- bind_rows(tripdata_202201,tripdata_202202,tripdata_202203)


#Q2 2022 Data Import
tripdata_202204 <- read.csv("202204-divvy-tripdata.csv")
tripdata_202205 <- read.csv("202205-divvy-tripdata.csv")
tripdata_202206 <- read.csv("202206-divvy-tripdata.csv")

#Q2 2022 Combine Data
q2_2022 <- bind_rows(tripdata_202204,tripdata_202205,tripdata_202206)


#Q3 2022 Data Import
tripdata_202207 <- read.csv("202207-divvy-tripdata.csv")
tripdata_202208 <- read.csv("202208-divvy-tripdata.csv")
tripdata_202209 <- read.csv("202209-divvy-publictripdata.csv")

#Q3 2022 Combine Data
q3_2022 <- bind_rows(tripdata_202207,tripdata_202208,tripdata_202209)


#combine all trips
all_trips <- bind_rows(q4_2021,q1_2022,q2_2022,q3_2022)
```

## Membersihkan data

```{r eval=FALSE}
#Check data
  colnames(all_trips)
  nrow(all_trips)
  dim(all_trips)  
  head(all_trips)  
  tail(all_trips)  
  str(all_trips)
  summary(all_trips)
  
  
  #Membuat kolom tanggal dari kolom started_at
  all_trips$date <- as.Date(all_trips$started_at)
  
  #Membuat kolom day, month, year dari kolom date
  all_trips$day <- format(as.Date(all_trips$date),"%d")
  all_trips$month <- format(as.Date(all_trips$date),"%m")
  all_trips$year <- format(as.Date(all_trips$date),"%y")
  
  #Membuat kolom ride_length untuk menghitung berapa detik lama perjalanan 
  all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
  
  # Mengubah data "ride_length" dari Factor menjadi Numeric sehingga data dapat dikalkulasi 
  is.factor(all_trips$ride_length)
  all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
  is.numeric(all_trips$ride_length)
  
  # Menghapus data yang rusak dan membuat versi 2 dari data
    # Menghilangkan durasi berkendara yang <0 detik
    all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
    all_trips_v2$month <- as.numeric(as.character(all_trips_v2$month))
    is.numeric(all_trips_v2$month)
  
  #Membuat kolom nama hari dari kolom date
  all_trips_v2$day_of_week <- wday(all_trips_v2$date, label=TRUE)
    
  #Convert month from numeric to 3 letter month name  
  all_trips_v2$month <- month.abb[all_trips_v2$month]
  all_trips_v2$month <- as.factor(as.character(all_trips_v2$month))
  is.factor(all_trips_v2$month)
  unique(all_trips_v2$month)
  
  #Arrange days and months
  all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat","Sun"))
  all_trips_v2$month <- ordered(all_trips_v2$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  #Mengubah membership value
  all_trips_v2["member_casual"][all_trips_v2["member_casual"] == "casual"] <- "Casual Member"
  all_trips_v2["member_casual"][all_trips_v2["member_casual"] == "member"] <- "Annual Member"
  all_trips_v2["rideable_type"][all_trips_v2["rideable_type"] == "docked_bike"] <- "Docked Bike"
  all_trips_v2["rideable_type"][all_trips_v2["rideable_type"] == "classic_bike"] <- "Classic Bike"
  all_trips_v2["rideable_type"][all_trips_v2["rideable_type"] == "electric_bike"] <- "Electric Bike"
  
```


## Data Viz

### Total Perjalanan Per Hari

```{r error=TRUE}
all_trips_v2 %>% 
    group_by(member_casual, day_of_week) %>% 
    summarise(number_of_rides = n()
              ,average_duration = mean(ride_length)) %>% 
    arrange(member_casual, day_of_week)  %>% 
    ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge") + 
    scale_fill_manual("Tipe Member", values = c("Annual Member" = "#FC766AFF", "Casual Member" = "#5B84B1FF")) +
    theme_light() +
    theme(plot.title = element_text(face="bold",colour="#36454F",hjust=0.5,size = 16),
          plot.subtitle = element_text(colour="#36454F",hjust=0.5,size = 10),
          axis.title.x = element_text(size = 10, margin=margin(t=7)),
          axis.title.y = element_text(size = 10, margin=margin(r=12)),
          legend.position = "bottom") +
    labs(x='Hari', y='Total Perjalanan', 
         title='Total Perjalanan Per Hari',
         subtitle="Oktober 2021 Sampai September 2022")+
    scale_y_continuous(breaks = c(0e+00, 2e+05, 4e+05), labels = c("0", "200 rb", "400 rb"))
```
<img src="images/Total_Trip_Day.png?raw=true"/>

### Total Perjalanan Per Hari

```{r}
all_trips_v2 %>% 
    group_by(member_casual, month) %>% 
    summarise(number_of_rides = n()
              ,average_duration = mean(ride_length)) %>% 
    arrange(member_casual, month) %>% 
    ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge") + 
    scale_fill_manual("Tipe Member", values = c("Annual Member" = "#FC766AFF", "Casual Member" = "#5B84B1FF")) +
    theme_light() +
    theme(plot.title = element_text(face="bold",colour="#36454F",hjust=0.5, size=16),
          plot.subtitle = element_text(colour="#36454F",hjust=0.5, size=10),
          axis.text.x = element_text(angle = 45),
          axis.title.x = element_text(size = 10, margin=margin(t=7)),
          axis.title.y = element_text(size = 10, margin=margin(r=12)),
          legend.position = "bottom") +
    labs(x='Bulan', y='Total Perjalanan', 
         title='Total Perjalanan Per Bulan',
         subtitle="Oktober 2021 Sampai September 2022")+
    scale_y_continuous(breaks = c(0e+00, 1e+05, 2e+05, 3e+05, 4e+05), labels = c("0", "100 rb", "200 rb", "300 rb", "400 rb"))

```


### Total Perjalanan Per Jenis Sepeda

```{r}
all_trips_v2 %>% 
    group_by(member_casual, rideable_type) %>% 
    summarise(ride_type = n()) %>% 
    ggplot(mapping=aes(x=rideable_type, y=ride_type, fill=member_casual, group=member_casual)) +
    geom_col(position = "dodge") + 
    scale_fill_manual("Tipe Member", 
                      values = c("Annual Member" = "#FC766AFF", "Casual Member" = "#5B84B1FF")) +
    theme_light() +
    theme(plot.title = element_text(face="bold",colour="#36454F",hjust=0.5, size=16),
          plot.subtitle = element_text(colour="#36454F",hjust=0.5, size=10),
          axis.title.x = element_text(size = 10, margin=margin(t=7)),
          axis.title.y = element_text(size = 10, margin=margin(r=12)),
          legend.position = "bottom") +
    labs(x='Tipe Sepeda', y='Total Perjalanan', 
         title='Total Perjalanan Per Jenis Sepeda',
         subtitle="Oktober 2021 Sampai September 2022")+
    scale_y_continuous(breaks = c(0, 500000, 1000000, 1500000), 
                       labels = c("0", "500 rb", "1 juta", "1,5 juta"))
```



### Rata-rata Durasi Perjalanan Per Hari

```{r}
all_trips_v2 %>% 
  mutate(hours = (format(as.POSIXct(all_trips_v2$started_at), format = "%H"))) %>% 
  filter(day_of_week %in%  c("Mon", "Tue", "Wed", "Thu", "Fri")) %>% 
  group_by(member_casual, hours) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hours) %>%  
  ggplot(mapping=aes(x=hours, y=number_of_rides, color=member_casual, group=member_casual)) +
  geom_line() +
  geom_point() + 
  scale_color_manual(name="Tipe Member",
                     values = c("Annual Member" = "#FC766AFF", "Casual Member" = "#5B84B1FF")) +
  theme_light() +
  theme(plot.title = element_text(face="bold",colour="#36454F",hjust=0.5,size=16),
        plot.subtitle = element_text(colour="#36454F",hjust=0.5,size=10),
        axis.title.x = element_text(size = 10, margin=margin(t=7)),
        axis.title.y = element_text(size = 10, margin=margin(r=12)),
        legend.position = "bottom") +
  labs(x='Jam', y='Rata-rata Durasi Perjalanan', 
       title='Rata-Rata Durasi Perjalanan Tiap Jam (Hari Kerja)',
       subtitle="Oktober 2021 Sampai September 2022",
       fill="Tipe Member") +
  scale_y_continuous(breaks = c(0e+00, 1e+05, 2e+05, 3e+05), labels = c("0", "100 rb", "200 rb", "300 rb")) 
  
```


### Rata-rata Durasi Perjalanan Tiap Jamnya (Weekday)

```{r}
all_trips_v2 %>% 
  mutate(hours = (format(as.POSIXct(all_trips_v2$started_at), format = "%H"))) %>% 
  filter(day_of_week %in%  c("Mon", "Tue", "Wed", "Thu", "Fri")) %>% 
  group_by(member_casual, hours) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hours) %>%  
  ggplot(mapping=aes(x=hours, y=number_of_rides, color=member_casual, group=member_casual)) +
  geom_line() +
  geom_point() + 
  scale_color_manual(name="Tipe Member",
                     values = c("Annual Member" = "#FC766AFF", "Casual Member" = "#5B84B1FF")) +
  theme_light() +
  theme(plot.title = element_text(face="bold",colour="#36454F",hjust=0.5,size=16),
        plot.subtitle = element_text(colour="#36454F",hjust=0.5,size=10),
        axis.title.x = element_text(size = 10, margin=margin(t=7)),
        axis.title.y = element_text(size = 10, margin=margin(r=12)),
        legend.position = "bottom") +
  labs(x='Jam', y='Rata-rata Durasi Perjalanan', 
       title='Rata-Rata Durasi Perjalanan Tiap Jam (Hari Kerja)',
       subtitle="Oktober 2021 Sampai September 2022",
       fill="Tipe Member") +
  scale_y_continuous(breaks = c(0e+00, 1e+05, 2e+05, 3e+05), labels = c("0", "100 rb", "200 rb", "300 rb")) 
  
```


### Rata-rata Durasi Perjalanan Tiap Jamnya (Weekend)
```{r}
 all_trips_v2 %>% 
    mutate(hours = (format(as.POSIXct(all_trips_v2$started_at), format = "%H"))) %>% 
    filter(day_of_week %in%  c("Sat", "Sun")) %>% 
    group_by(member_casual, hours) %>% 
    summarise(number_of_rides = n()
              ,average_duration = mean(ride_length)) %>% 
    arrange(member_casual, hours) %>%  
    ggplot(mapping=aes(x=hours, y=number_of_rides, color=member_casual, group=member_casual)) +
    geom_line() +
    geom_point() + 
    scale_color_manual(name="Tipe Member",
                       values = c("Annual Member" = "#FC766AFF", "Casual Member" = "#5B84B1FF")) +
    theme_light() +
    theme(plot.title = element_text(face="bold",colour="#36454F",hjust=0.5,size=16),
          plot.subtitle = element_text(colour="#36454F",hjust=0.5,size=10),
          axis.title.x = element_text(size = 10, margin=margin(t=7)),
          axis.title.y = element_text(size = 10, margin=margin(r=12)),
          legend.position = "bottom"
          ) +
    labs(x='Jam', y='Rata-rata Durasi Perjalanan', 
         title='Rata-Rata Durasi Perjalanan Tiap Jamnya (Hari Libur)',
         subtitle="Oktober 2021 Sampai September 2022",
         fill="Tipe Member") +
    scale_y_continuous(breaks = c(0, 20000, 40000, 60000, 80000), labels = c("0", "20 rb", "40 rb", "60 rb", "80 rb")) 

```


### 
