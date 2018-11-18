setwd("/Users/mali/Documents/myGit/LRCrimeStudy")

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
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("Scale")
install.packages("leaflet")
install.packages("maps")
#Loading the packages
library(plyr)
library(lubridate)
library(dplyr)
library(ggrepel)
library(Scale)
library(leaflet)
library(ggplot2)
library(maps)
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

#First, we hope to have a big picture of the crimes happened in the time range, more accurate,
#we want to see which types of crimes commited the most
#Thus, we will show the total count of each typ of crime with a barplot

incidentsSummary <- as.data.frame(incidents %>%
                                    group_by(OFFENSE_DESCRIPTION) %>%
                                    summarise(offense_amount = n()) %>%
                                    distinct())
attach(incidentsSummary)
colnames(incidentsSummary) <- c("offense_description", "offense_amount")
#The following codes are for the bar chart

p <- ggplot(incidentsSummary, aes(x = reorder(incidentsSummary$offense_description, desc(incidentsSummary$offense_amount)), 
                                  y = incidentsSummary$offense_amount))
p + geom_bar(stat = "identity", width = 0.8, fill = "red") +
  coord_flip() + 
  labs(title = "CATAGORIED CRIME COUNT", x = "OFFENSE_DESCRIPTION", y = "OFFENSE_AMOUNT")

#The following codes aim to plot out the pie chart
pie <- ggplot(incidentsSummary, aes(x = "", y = incidentsSummary$offense_amount, fill = factor(incidentsSummary$offense_description)))
pie + geom_bar(width = 1, stat = "identity") + 
  theme(axis.line = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(fill = "OFFENSE_DESCRIPTION", x = NULL, y = NULL, title = "PIE OF TOP20",
       caption = "Source:OFFENSE_DESCRIPTION") + 
  coord_polar(theta = "y", start = 0)

#Second, we hope to figuer out how the crimes distribute in each weekday, from 0:00 to 23:59
#For, this part, we will use a heatmap to show the relation. And, we will use all crime records
#setting colors
col1 = "#FEFEFE" 
col2 = "#540404"

weekdayHour <- ddply(incidents, c("hour", "wday"), summarise, N = length(ymd))
weekdayHour$wday <- factor(weekdayHour$wday, levels=rev(levels(weekdayHour$wday)))

#creating the heatmap
ggplot(weekdayHour, aes(hour, wday)) + geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Incidents")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Heatmap of LR crimes by Hour of a day",
       x = "Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Then, we hope to see the distribution of specific crimes in a week
crimesDay <- aggregate(incidents$OFFENSE_DESCRIPTION,
                       by = list(incidents$OFFENSE_DESCRIPTION, incidents$wday), FUN = length)
names(crimesDay) <- c("crimeDescription", "weekday", "count")
crimesDay$weekday <- factor(crimesDay$weekday, levels = rev(levels(crimesDay$weekday)))
teplist <- NULL
for (i in unlist(crimesDay$count)) {
  teplist <- c(teplist, i)
}
crimesDay <- crimesDay[order(crimesDay$count, decreasing = TRUE), ]
ggplot(crimesDay, aes(crimeDescription, weekday)) + geom_tile(aes(fill = count),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Incidents")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Heatmap of LR crime distribution per week",
       x = "Crime description", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5))
  
#Third, science the coordinations of all locations where incidents commited, we want to see
#them being marked on the map
install.packages("RJSONIO")
library(RJSONIO)
json_file <- fromJSON("CITY_LIMITS_AHTD.json")
json_file <- lapply(json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})
df<-as.data.frame(do.call("cbind", json_file))
ggplot() +
  geom_polygon(data = df, aes( x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()
