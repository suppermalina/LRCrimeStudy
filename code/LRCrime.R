setwd("/Users/mali/Documents/myGit/LRCrimeStudy/code")

#Loading the packages
library(plyr)
library(lubridate)
library(dplyr)
library(ggplot2)
library (psych)
library(reshape2)
library(Hmisc)
library(corrplot)
#Loading data
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
#Data set can be found on this webpage:
#https://data.littlerock.gov/Safe-City/Little-Rock-Police-Department-Statistics-2017-to-Y/bz82-34ep

#Peeking at the data set
#Here, the data.frame incidents contains data for 2017, 2018. 
#We need this dataframe for analysising the distribution of crimes per hour in each week
incidents <- read.table("/Users/mali/Documents/myGit/LRCrimeStudy/data/Little_Rock_Police_Department_Statistics_2017_to_Year_to_Date_2018.csv", 
                        head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F)
head(incidents)
attach(incidents)
str(incidents)

incidents$INCIDENT_DATE <- gsub("\\/", "\\-", incidents$INCIDENT_DATE)
incidents$ymd <- mdy_hms(incidents$INCIDENT_DATE)
incidents$month <- month(incidents$ymd, label = TRUE)
incidents$year <- year(incidents$ymd)
incidents$wday <- wday(incidents$ymd, label = TRUE)
incidents$hour <- hour(incidents$ymd)


#This dataframe contains all crime records from 2015-2018
total_incidents <- read.table("/Users/mali/Documents/myGit/LRCrimeStudy/data/Crime_records_Of_LR_2015-2018_v3.csv", head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F)

#First, we hope to have a big picture of the crimes happened in the time range, more accurate,
#we want to see which types of crimes commited the most
#Thus, we will show the total count of each typ of crime with a barplot

incidentsSummary <- as.data.frame(total_incidents %>%
                                    group_by(incidents_Description) %>%
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

#Third, science the coordinations of all locations where incidents commited, we want to see
#them being marked on the map
library("dplyr")
library("forcats")
register_google(key = "AIzaSyAj3UsfYVSZOvDk8wD2Xo8pIqtiTq_bRhI", account_type = "premium", day_limit = 100000)

#Overall heatmap
geo_incidents <- ddply(total_incidents, c("OFFENSE_DESCRIPTION", 
                                    "LATITUDE", "LONGITUDE"), summarise, N = length(OFFENSE_DESCRIPTION))
geo_incidents$OFFENSE_DESCRIPTION <- factor(geo_incidents$OFFENSE_DESCRIPTION)
qmplot(LONGITUDE, LATITUDE, data = geo_incidents, maptype = "toner-lite", geom = "density2d", color = I("red"))

qmplot(LONGITUDE, LATITUDE, data = geo_incidents, geom = "blank", 
       zoom = 15, maptype = "toner-background", darken = .7, legend = "topleft"
) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Incidents\nFourwo Years", low = "white", mid = "yellow", high = "red", midpoint = 45)

#All the plots shown above only reveal us the distribution of crimes in the whole city.
#However, we have no idea with the internal factors which effect the distribution.
#At the very begining, we tried to figure out the correlation between the amount of incidents
#with space and time. But, we found this is helpless.
#We cannot study the trending in a specific area without defined a specific time. And, since 
#the total amount of incidents in an area increasing with the time flows. So, we decide to see
#the relation between different types of crimes with the area in which the crimes happened.
#As well as the relation between the total amount of crimes happend in a specific weekday and
#areas

library(stringr)
incidents$dim <- str_sub(incidents$ymd, start = 6L, end = 10L)
#These 11 plots reveal the distribution of the 11 types of crimes in the city

qmplot(LONGITUDE, LATITUDE, data = geo_incidents, maptype = "toner-background", color = OFFENSE_DESCRIPTION) + 
  facet_wrap(~ OFFENSE_DESCRIPTION)

#Now, we hope to see the distribution
#of different types of crimes in a week as well as in a year. The heatmap is used.
crimesDay <- aggregate(total_incidents$incidents_Description,
                       by = list(total_incidents$incidents_Description, total_incidents$incidents_wdays), FUN = length)
names(crimesDay) <- c("crimeDescription", "weekday", "count")
class(crimesDay$weekday)
factor(crimesDay$weekday)
crimesDay$weekday <- factor(crimesDay$weekday)

crimesDay <- crimesDay[order(crimesDay$count, decreasing = TRUE), ]
ggplot(crimesDay, aes(crimeDescription, weekday)) + geom_tile(aes(fill = count),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Incidents")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Heatmap of LR crime distribution per week",
       x = "Crime description", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5))

#The distribution of crimes in each month
monthly_crimes <- aggregate(total_incidents$incidents_Description,
                            by = list(total_incidents$incidents_Description, as.factor(total_incidents$incidents_month)), FUN = length)
names(monthly_crimes) <- c("crimeDescription", "month", "count")
monthly_crimes$month <- factor(monthly_crimes$month, levels = c("JAN", "FEB", "MAR", "APR", 
                                                                "MAY", "JUN", "JUL", "AUG", 
                                                                "SEP", "OCT", "NOV", "DEC"))

monthly_crimes <- monthly_crimes[order(monthly_crimes$count, decreasing = TRUE), ]
ggplot(monthly_crimes, aes(crimeDescription, month)) + geom_tile(aes(fill = count),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Monthly Crimes")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Heatmap of LR crime distribution per month for the past four years",
       x = "Crime description", y = "Month") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5))

#Now, we want to see the distribution of crimes in each moth, no matter which type of crimes
#Bar plot will be adopted
for (i in 1 : length(total_incidents$incidents_month)) {
  if (grepl("Sep", total_incidents$incidents_month[i])) {
    total_incidents$incidents_month[i] <- gsub("Sep", "SEP", total_incidents$incidents_month[i])
  }
}
month_all <- aggregate(total_incidents$incidents_month, by = list(total_incidents$incidents_month), FUN = length)
names(month_all) <- c("Month", "Total amount")
month_all$Month <- factor(month_all$Month, levels = c("JAN", "FEB", "MAR", "APR", 
                                                     "MAY", "JUN", "JUL", "AUG", 
                                                     "SEP", "OCT", "NOV", "DEC"))

ggplot(month_all, aes(x = Month, y = `Total amount`), colo) + 
  geom_bar(stat = "identity", width = 0.8, fill = "red")

#Do this to four years separately
#total_incidents$year <- year(total_incidents$incidents_ymd)
total_incidents$incidents_month <- factor(total_incidents$incidents_month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

#factor(total_incidents$year)
separated_by_year <- data.frame(cbind(total_incidents$incidents_month, total_incidents$incidents_Description, 
                                     total_incidents$year))
head(separated_by_year)
names(separated_by_year) <- c("Month", "Description", "Year")
separated_by_year <- aggregate(separated_by_year$Description, by = list(separated_by_year$Month, separated_by_year$Year), FUN = length)
names(separated_by_year) <- c("Month", "Year", "Amount")
separated_by_year$Month <- factor(separated_by_year$Month, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                                  labels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC") )
ggplot(separated_by_year, aes(x = Month, y = Amount)) + 
  geom_bar(stat = "identity", width = 0.8, fill = "red") + 
  facet_wrap(~Year)

#Prediction
#Lon and Lat can be merged into one number
total_incidents$location <- sqrt((as.numeric(total_incidents$incidents_Lat)^2 + 
                                    (as.numeric(total_incidents$incidents_Lon))^2))
incidents$hour
total_incidents$incidents_District
pairs.panels(incidents[c("LOCATION_DISTRICT", "LONGITUDE", "LATITUDE", "hour", "month")])
pairs.panels(total_incidents[c("incidents_Lat", "incidents_Lon", "incidents_District", "incidents_month")])

#import daily temp
daily_temp <- read.table("/Users/mali/Documents/myGit/LRCrimeStudy/data/purged_daily_climate_Of_LR_2015-2018.csv", 
                         head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F)
daily_temp$X <- NULL
daily_temp$dailyAvg <- (daily_temp$dailyMax + daily_temp$dailyMin) / 2
colnames(daily_temp) <- c("date", "min_temp", "max_temp", "daily_avg_temp")
#aggregate the past years daily total crime count, and merge with daily_temp
head(total_incidents)
daily_total_crimes <- data.frame(total_incidents$incidents_ymd, total_incidents$incidents_Number, stringsAsFactors = FALSE)
daily_total_crimes <- aggregate(daily_total_crimes$total_incidents.incidents_Number, by = list(daily_total_crimes$total_incidents.incidents_ymd), FUN = length)
colnames(daily_total_crimes) <- c("date", "daily_total")
#as.Date.character(daily_temp$date[1]) == as.Date.character(daily_total_crimes$date[1])
daily_total_crimes$date <- as.Date.character(daily_total_crimes$date)
daily_temp$date <- as.Date.character(daily_temp$date)

#merging daily_total_crimes and daily_temp by date
combination_information <- merge(daily_total_crimes, daily_temp, by = "date")
combination_information$year <- year(combination_information$date)
combination_information$month <- month(combination_information$date)

total_incidents <- total_incidents[!is.na(total_incidents$incidents_Lat), ]
total_incidents <- total_incidents[!is.na(total_incidents$incidents_Lon), ]
tail(total_incidents)
head(total_incidents)
#separating the datas set into four sets based on years
combination_infor_2015 <- combination_information[combination_information$year == 2015, ]
combination_infor_2016 <- combination_information[combination_information$year == 2016, ]
combination_infor_2017 <- combination_information[combination_information$year == 2017, ]
combination_infor_2018 <- combination_information[combination_information$year == 2018, ]
#plot out this data set. let the date be the x axis
mid_2015 <- median(combination_infor_2015$daily_total)
sp_2015<-ggplot(combination_infor_2015, aes(x = date, y = daily_avg_temp)) + 
  #geom_point(aes(color = daily_total)) + 
  #scale_color_gradient2(midpoint = mid_2015, low = 'blue', mid = "grey", high = "black", space = "Lab") + 
  geom_line(aes(y = min_temp), color = "#0fbf27") + 
  #, color = "#0fbf27"
  geom_line(aes(y = daily_avg_temp), color = "#bfbc0f") + 
  #, color = "#bfbc0f"
  geom_line(aes(y = max_temp), color = "#bf1111") + 
  #, color = "#bf1111
  geom_line(aes(y = daily_total), color = "#4256f4") + 
  #, color = "#4256f4"
  ggtitle("Relation between the amount of daily crimes and daily average temparature of 2015") + 
  xlab("Date") + ylab("Daily average temparature")
  #scale_color_manual(values = c("#0fbf27", "#bfbc0f", "#bf1111", "#4256f4")) + 
  sp_2015 + facet_wrap(~month, ncol = 4, scales = "free") 
sp_2015

mid_2016 <- median(combination_infor_2016$daily_total)
sp_2016<-ggplot(combination_infor_2016, aes(x = date, y = daily_avg_temp)) + 
  #geom_point(aes(color = daily_total)) + 
  #scale_color_gradient2(midpoint = mid_2015, low = 'blue', mid = "grey", high = "black", space = "Lab") + 
  geom_line(aes(y = min_temp), color = "#0fbf27") + 
  geom_line(aes(y = daily_avg_temp), color = "#bfbc0f") + 
  geom_line(aes(y = max_temp), color = "#bf1111") + 
  geom_line(aes(y = daily_total)) + 
  ggtitle("Relation between the amount of daily crimes and daily average temparature of 2016") + 
  xlab("Date") + ylab("Daily average temparature") 
sp_2016 + facet_wrap(~month, ncol = 4, scales = "free") 
sp_2016

mid_2017 <- median(combination_infor_2017$daily_total)
sp_2017<-ggplot(combination_infor_2017, aes(x = date, y = daily_avg_temp)) + 
  #geom_point(aes(color = daily_total)) + 
  #scale_color_gradient2(midpoint = mid_2015, low = 'blue', mid = "grey", high = "black", space = "Lab") + 
  geom_line(aes(y = min_temp), color = "#0fbf27") + 
  geom_line(aes(y = daily_avg_temp), color = "#bfbc0f") + 
  geom_line(aes(y = max_temp), color = "#bf1111") + 
  geom_line(aes(y = daily_total)) + 
  ggtitle("Relation between the amount of daily crimes and daily average temparature of 2017") + 
  xlab("Date") + ylab("Daily average temparature") 
sp_2017 + facet_wrap(~month, ncol = 4, scales = "free") 
sp_2017

mid_2018 <- median(combination_infor_2018$daily_total)
sp_2018<-ggplot(combination_infor_2018, aes(x = date, y = daily_avg_temp)) + 
  #geom_point(aes(color = daily_total)) + 
  #scale_color_gradient2(midpoint = mid_2015, low = 'blue', mid = "grey", high = "black", space = "Lab") + 
  geom_line(aes(y = min_temp), color = "#0fbf27") + 
  geom_line(aes(y = daily_avg_temp), color = "#bfbc0f") + 
  geom_line(aes(y = max_temp), color = "#bf1111") + 
  geom_line(aes(y = daily_total)) + 
  ggtitle("Relation between the amount of daily crimes and daily average temparature of 2018") + 
  xlab("Date") + ylab("Daily average temparature") 
sp_2018 + facet_wrap(~month, ncol = 4, scales = "free") 
sp_2018

#######################
#######################
#correlation analysis
corrplot.mixed(cor(combination_infor_2015[, -c(1, 6, 7)]), order="hclust", tl.col="black")
corrplot.mixed(cor(combination_infor_2016[, -c(1, 6, 7)]), order="hclust", tl.col="black")
corrplot.mixed(cor(combination_infor_2016[, -c(1, 6, 7)]), order="hclust", tl.col="black")
corrplot.mixed(cor(combination_infor_2016[, -c(1, 6, 7)]), order="hclust", tl.col="black")

corrplot.mixed(cor(combination_information[, -c(1, 6, 7)]), order="hclust", tl.col="black")

######################
daily_geom_crime <- data.frame(cbind(total_incidents$incidents_ymd, total_incidents$year, total_incidents$incidents_month, 
                                     total_incidents$incidents_Lon, total_incidents$incidents_Lat, total_incidents$incidents_District, 
                                     total_incidents$incidents_Number), stringsAsFactors = FALSE)
names(daily_geom_crime) <- c("ymd", "year", "month", "lon", "lat", "district", "number")
#make lon and lot more comparable
#x = cos(lat) * cos(lon)
#y = cos(lat) * sin(lon)
daily_geom_crime$x <- cos(as.numeric(daily_geom_crime$lat)) * cos(as.numeric(daily_geom_crime$lon))
daily_geom_crime$y <- cos(as.numeric(daily_geom_crime$lat)) * sin(as.numeric(daily_geom_crime$lon))
daily_geom_crime$point <- sqrt(as.numeric(daily_geom_crime$x) ^ 2 + as.numeric(daily_geom_crime$y) ^ 2)
daily_geom_crime <- aggregate(daily_geom_crime$number, by = list(daily_geom_crime$ymd, daily_geom_crime$district,
                                                                 daily_geom_crime$point), FUN = length)
names(daily_geom_crime) <- c("date", "district", "point", "count")
daily_geom_crime$date <- as.Date(daily_geom_crime$date)
daily_geom_crime_climate <- merge(daily_geom_crime, daily_temp, by = "date")




