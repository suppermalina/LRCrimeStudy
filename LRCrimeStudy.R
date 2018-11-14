setwd("/Users/mali/Desktop/special_topics/course\ project/")

# This script aims to study the crimes happened in LR.
# Data are downloaded from the police department of LR. 
# The data set has following records:
# INCIDENT_DATA, data form: month/day/year hour_miutes_second AM/PM
# INCIDENT_NUMBER, data form: year-xxxxxx(six digits)
# LOCATION_DISTRICT, data form: two digits number
# OFFENS_CODE, data form: two digits with a uppercase letter
# OFFENS_DESCRIPTION: a string
# LOCATION_ADDRESS,CITY,STATE,ZIP
# LATITUDE,LONGITUDE
# Location: street name only

#A set of tools that solves a common set of problems: you
#need to break a big problem down into manageable pieces, 
#operate on each piece and then put all the pieces back together. 
#For example, you might want to fit a model to each spatial location or time point in your study, 
#summarise data by panels or collapse high-dimensional arrays to simpler summary statistics. 
#The development of 'plyr' has been generously supported by 'Becton Dickinson'.
install.packages("plyr")

#Functions to work with date-times and time-spans: fast and user friendly parsing of date-time data, 
#extraction and updating of components of a date-time (years, months, days, hours, minutes, and seconds), 
#algebraic manipulation on date-time and time-span objects. 
#The 'lubridate' package has a consistent and memorable syntax that makes working with dates easy and fun.
#Parts of the 'CCTZ' source code, released under the Apache 2.0 License,
#are included in this package. See <https://github.com/google/cctz> for more details.
install.packages("lubridate")

#A fast, consistent tool for working with data frame like objects, both in memory and out of memory.
install.packages("dplyr")

#Automatically Position Non-Overlapping Text Labels with
'ggplot2'
install.packages("ggrepel")
install.packages("ggplot2")

#Loading the packages
library(plyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggrepel)

#Loading data
#Data set can be found on this webpage:
#https://data.littlerock.gov/Safe-City/Little-Rock-Police-Department-Statistics-2017-to-Y/bz82-34ep
incidents <- read.table("Little_Rock_Police_Department_Statistics_2017_to_Year_to_Date_2018.csv", 
                        head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F)


#Peeking at the data set
head(incidents)
attach(incidents)
str(incidents)

#Since we hope to study the relation between crimes and times, so we have to prepare time variables
#The lubridata package provides us a handy tool for parshing day, time, hour, even seconds
#basic syntax:
#date
#ymd("20110604")
#> [1] "2011-06-04"
#mdy("06-04-2011")
##> [1] "2011-06-04"
#dmy("04/06/2011")
#> [1] "2011-06-04"
#arrive <- ymd_hms("2011-06-04 12:00:00", tz = "Pacific/Auckland")
#arrive
#> [1] "2011-06-04 12:00:00 NZST"
#leave <- ymd_hms("2011-08-10 14:00:00", tz = "Pacific/Auckland")
#leave
#> [1] "2011-08-10 14:00:00 NZST"
#second(arrive)
#> [1] 0
#second(arrive) <- 25
#arrive
#> [1] "2011-06-04 12:00:25 NZST"
#second(arrive) <- 0
#wday(arrive)
#> [1] 7
#wday(arrive, label = TRUE)
#> [1] Sat
#> Levels: Sun < Mon < Tue < Wed < Thu < Fri < Sat
incidents$INCIDENT_DATE <- gsub("\\/", "\\-", incidents$INCIDENT_DATE)
incidents$ymd <- mdy_hms(incidents$INCIDENT_DATE)
incidents$month <- month(incidents$ymd, label = TRUE)
incidents$year <- year(incidents$ymd)
incidents$wday <- wday(incidents$ymd, label = TRUE)
incidents$hour <- hour(incidents$ymd)

attach(incidents)
head(incidents)

#First, we hope to have a big picture of the crimes happened in the time range. Especially, 
#we want to see the 20 of crime that has been recorded with a bar chart and a pie chart

topTwenty <- as.data.frame(incidents %>%
                          group_by(OFFENSE_DESCRIPTION) %>%
                          summarize(offense_amount = n()) %>%
                          distinct() %>%
                          top_n(20))
colnames(topTwenty) <- c("offense_description", "offense_amount")
#The following codes are for the bar chart
newTopTwenty <- topTwenty[order(topTwenty$offense_amount, decreasing = TRUE), ]

p_twenty <- ggplot(newTopTwenty, aes(x = newTopTwenty$offense_description, y = newTopTwenty$offense_amount))
p_twenty + geom_bar(stat = "identity", width = 0.8, fill = "red") +
  coord_flip() + 
  labs(title = "TOP TEN", x = "OFFENSE_DESCRIPTION", y = "OFFENSE_AMOUNT")

#The following codes are fot the pie chart
pie_twenty <- ggplot(newTopTwenty, aes(x = "", y = newTopTwenty$offense_amount, fill = factor(newTopTwenty$offense_description)))
pie_twenty + geom_bar(width = 1, stat = "identity") + 
  theme(axis.line = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(fill = "OFFENSE_DESCRIPTION", x = NULL, y = NULL, title = "PIE OF TOP20",
       caption = "Source:OFFENSE_DESCRIPTION") + 
  coord_polar(theta = "y", start = 0)

#Second, we hope to figuer out how the crimes distribute in each weekday, from 0:00 to 23:59
#For, this part, we will use a heatmap to show the relation
#setting colors
col1 = "#FEFEFE" 
col2 = "#540404"

weekdayHour <- ddply(incidents, c("hour", "wday"), summarise, N = length(ymd))
weekdayHour$wday <- factor(weekdayHour$wday, levels=rev(levels(weekdayHour$wday)))
attach(weekdayHour)

#creating the heatmap
ggplot(weekdayHour, aes(hour, wday)) + geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Incidents")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Heatmap of LR crimes by Day of Week and Hour",
       x = "Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#First, analysis the relation between crimes amount with time range in each month
#In this part, the variable monthHour is used to store the summary table with two columns hour
#and month
#ddply is implmented in this section of code. ddply came with the package plyr
#ddply(data.frame, variable(s), function, optional arguments)
monthHour <- ddply(incidents, c("hour", "month"), summarize, N = length(incidents$INCIDENT_DATE))


