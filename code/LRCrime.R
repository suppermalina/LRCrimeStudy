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
library(factoextra)
library(tidyr)
library(data.table)
library(DAAG)
library(rpart)
library(C50)
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
incidents$month <- month(incidents$ymd)
incidents$year <- year(incidents$ymd)
incidents$wday <- wday(incidents$ymd)
incidents$hour <- hour(incidents$ymd)
hour_wday <- data.frame(cbind(incidents$hour, incidents$wday, incidents$OFFENSE_CODE), stringsAsFactors = FALSE)
names(hour_wday) <- c("hour", "wday", "code")
hour_wday$wday <- factor(hour_wday$wday, labels = c("MON", "TUE", "WED", "THR", "FRI", "SAT", "SUN"))
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

weekdayHour <- ddply(hour_wday, c("hour", "wday"), summarise, N = length(code))
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
#correlation between month, temp and crimes records
month_temp <- combination_information[, -c(1, 3, 4)]
month_temp_total <- combination_information[, -c(1, 3, 4, 6)]
month_temp$month
length_of_each_month <- aggregate(month_temp$month, by = list(month_temp$year, month_temp$month), FUN = length)
month_temp_sec <- aggregate(month_temp$daily_avg_temp, by = list(month_temp$month, month_temp$year), FUN = sum)
names(length_of_each_month) <- c("year", "month", "days")
names(month_temp_sec) <- c("month", "year", "total")
temp_sec <- merge(length_of_each_month, month_temp_sec, by = c("year", "month"))
temp_sec$monthly_avg_temp <- temp_sec$total / temp_sec$days
temp_sec <- temp_sec[, -c(3, 4)]

month_temp_crim_sec <- aggregate(month_temp$daily_total, by = list(month_temp$year, month_temp$month), FUN = sum)
names(month_temp_crim_sec) <- c("year", "month", "monthly_total")

month_temp_crime <- merge(month_temp_crim_sec, temp_sec, by = c("year", "month"))
month_temp_crime$year <- factor(month_temp_crime$year)
month_temp_crime$month <- factor(as.numeric(month_temp_crime$month), order = c(1 : 12), 
                         labels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

month_temp_crime_2015 <- month_temp_crime[month_temp_crime$year == "2015", ]
month_temp_crime_2016 <- month_temp_crime[month_temp_crime$year == "2016", ]
month_temp_crime_2017 <- month_temp_crime[month_temp_crime$year == "2017", ]
month_temp_crime_2018 <- month_temp_crime[month_temp_crime$year == "2018", ]

corrplot.mixed(cor(month_temp_crime_2015[, -c(1, 2)]), order="hclust", tl.col="black")
corrplot.mixed(cor(month_temp_crime_2016[, -c(1, 2)]), order="hclust", tl.col="black")
corrplot.mixed(cor(month_temp_crime_2017[, -c(1, 2)]), order="hclust", tl.col="black")
corrplot.mixed(cor(month_temp_crime_2018[, -c(1, 2)]), order="hclust", tl.col="black")
##

month_temp_total_tmp <- aggregate(month_temp_total$daily_avg_temp, by = list(month_temp_total$month), FUN = mean)
names(month_temp_total_tmp) <- c("month", "monthly_avg_tmp")
month_temp_total_crime <- aggregate(month_temp_total$daily_total, by = list(month_temp_total$month), FUN = sum)
names(month_temp_total_crime) <- c("month", "monthly_total")
total_month_crime <- merge(month_temp_total_tmp, month_temp_total_crime, by = c("month"))
corrplot.mixed(cor(total_month_crime[, c(2:3)]), order="hclust", tl.col="black")


#####################
#linear regression
#base on monthly data
train <- data.frame(rbind(month_temp_crime_2015, month_temp_crime_2016, month_temp_crime_2017), stringsAsFactors = FALSE)
test <- month_temp_crime_2018
model = lm(formula = monthly_total ~ monthly_avg_temp,  data=train)
summary(model)
par(mfrow=c(2,2))
plot(model)

prediction <- predict(model, test)
#####
#k fold
myData <- data.frame(rbind(month_temp_crime_2015[3:4], month_temp_crime_2016[3:4], 
                           month_temp_crime_2017[3:4], month_temp_crime_2018[3:4]), stringsAsFactors = FALSE)
names(myData) <- c("y", "x")
cv.lm(myData, fit, m=10) # 10 fold cross-validation


######################

#we have to re-label all descriptions. currently we have 11 descriptions.
#however, if we use all these 11 types to do machine learning, we will fall into
#the trap of overfit. So, we re-classfied all crimes into two types:
#property, violent
#property break down into breaking in, theft, etc
#violent includes rape, mudur, etc.
detail_types <- levels(as.factor(total_incidents$incidents_Description))
proper_list <- c("ALL OTHER LARCENY", "BREAK IN", "BURGLARY", "THEFT", "PROPERTY")
violent_list <- c("ASSAULT", "RAPE", "TERR", "BATTERY", "ROBBERY", "VIOLENT")
for (i in 1 : length(total_incidents$incidents_Description)) {
  if (total_incidents$incidents_Description[i] %in% proper_list) {
    print("PROPERTY")
    print(total_incidents$incidents_Description[i])
    total_incidents$incidents_Description[i] <- "PROPERTY"
  } else if (total_incidents$incidents_Description[i] %in% violent_list) {
    print("VIO")
    print(total_incidents$incidents_Description[i])
    total_incidents$incidents_Description[i] <- "VIOLENT"
  } else {
    print("RM")
    total_incidents <- total_incidents[-i, ]
  }
}
#write.csv(total_incidents, "/Users/mali/Documents/myGit/LRCrimeStudy/data/relabel_and_classfied_crime_Of_LR_2015-2018.csv")
total <- read.table("/Users/mali/Documents/myGit/LRCrimeStudy/data/relabel_and_classfied_crime_Of_LR_2015-2018.csv", head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F)

class(daily_geom_crime$month)
factor(total$incidents_Description)
daily_geom_crime <- data.frame(cbind(total$year, total$incidents_month, total$incidents_wday, 
                                     total$incidents_District, total$incidents_Description), stringsAsFactors = FALSE)
class(month_temp$month)
month_temp$month <- data.frame(factor(month_temp$month, levels = c(1:12), labels = c("JAN", "FEB", "MAR", "APR", 
                                                                               "MAY", "JUN", "JUL", "AUG", 
                                                                               "SEP", "OCT", "NOV", "DEC")), stringsAsFactors = FALSE)
names(daily_geom_crime) <- c("year", "month", "wday", "district", "description")
names(month_temp) <- c("daily_total", "daily_avg_temp", "year", "month")
tree_data <- merge(month_temp, daily_geom_crime, by = c("year", "month"))
tree_data <- tree_data[, -c(1, 3)]
factor(tree_data$description)
tree_data$description <- factor(tree_data$description, levels = c(1, 2), labels = c("no", "yes"))
table(tree_data$description)
