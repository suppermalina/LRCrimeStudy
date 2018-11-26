setwd("/Users/mali/Documents/myGit/LRCrimeStudy/code")
library(lubridate)
library(reshape)
library(stringr)

transfer2ymd <- function(input) {
  if (is.na(input)) {
    return(NA)
  }
  if (is.null(input)) {
    return(NA)
  }
  if (grepl("\\/", input)) {
    gsub("\\/", "\\-", input)
  }
  brkdate <- strsplit(input, "\\-")
  reformed_ymd <- paste(c("20", brkdate[[1]][3], "-", brkdate[[1]][1], "-", brkdate[[1]][2]), 
                        collapse = "")
  return(reformed_ymd)
}

tmp_2017_2018 <- read.table("/Users/mali/Documents/myGit/LRCrimeStudy/data/Little_Rock_Police_Department_Statistics_2017_to_Year_to_Date_2018.csv", 
                            head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F)

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
tmp_2017_2018$INCIDENT_DATE <- gsub("\\/", "\\-", tmp_2017_2018$INCIDENT_DATE)
tmp_2017_2018$ymd <- mdy_hms(tmp_2017_2018$INCIDENT_DATE)
tmp_2017_2018$month <- month(tmp_2017_2018$ymd, label = TRUE)
tmp_2017_2018$year <- year(tmp_2017_2018$ymd)
tmp_2017_2018$wday <- wday(tmp_2017_2018$ymd, label = TRUE)
tmp_2017_2018$hour <- hour(tmp_2017_2018$ymd)
tmp_2017_2018$ymd <- as.character(tmp_2017_2018$ymd)
for (i in 1 : length(tmp_2017_2018$ymd)) {
  if (grepl("[[:space:]][0-9].*", tmp_2017_2018$ymd[i])) {
    print(tmp_2017_2018$ymd[i])
    tmp_2017_2018$ymd[i] <- gsub("[[:space:]][0-9].*", "", tmp_2017_2018$ymd[i])
    print(tmp_2017_2018$ymd[i])
  }
}
tmp_2017 <- tmp_2017_2018[tmp_2017_2018$year == "2017", ]
tmp_2018 <- tmp_2017_2018[tmp_2017_2018$year == "2018", ]
write.csv(tmp_2017, "/Users/mali/Documents/myGit/LRCrimeStudy/data/Little_Rock_Police_Department_Statistics_2017.csv")
write.csv(tmp_2018, "/Users/mali/Documents/myGit/LRCrimeStudy/data/Little_Rock_Police_Department_Statistics_2018.csv")

#########################################################
#########################################################

tmp_2015 <- read.table("/Users/mali/Documents/myGit/LRCrimeStudy/data/Little_Rock_Police_Department_Statistics_2015.csv", 
                        head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F)
head(tmp_2015)
tmp_date_2015 <- data.frame(tmp_2015$Incident.Date, stringsAsFactors = FALSE)


#transform the format of date into the one used by 2017 and 2018
for (i in 1 : length(tmp_2015$Incident.Date)) {
  if (grepl("\\-", tmp_2015$Incident.Date[i])) {
    tmp <- gsub("\\-.*", "", tmp_2015$Incident.Date[i])
    print(tmp)
    tmp_2015$Incident.Date[i] <- tmp
  }
}

tmp_2015$ymd <- mdy(tmp_2015$Incident.Date)
for (i in 1 : length(tmp_2015$Incident.Date)) {
  if (!grepl("2015", tmp_2015$ymd[i])) {
    tmp_2015[-i, ]
  }
}
tmp_2015$month <- month(tmp_2015$ymd)
tmp_2015$wday <- weekdays(tmp_2015$ymd)
tmp_2015$Incident.Date <- NULL

for (i in 1 : length(tmp_2015$Incident.Address)) {
  if (grepl("\\n\\(.*", tmp_2015$Incident.Address[i])) {
    print(tmp_2015$Incident.Address[i])
    tmp_2015$Incident.Address[i] <- gsub("\\n\\(.*", ",", tmp_2015$Incident.Address[i])
    tmp_2015$Incident.Address[i] <- gsub("\\n", ",", tmp_2015$Incident.Address[i])
  } else {
    tmp_2015$Incident.Address[i] <- gsub("\\n", ",", tmp_2015$Incident.Address[i])
  }
}

#using the original address information fetching coordinate from google
register_google(key = "AIzaSyC4L_SU2ZLIqImTqhxJKUxycGq0oCSmrVE", account_type = "premium", day_limit = 100000)
coordinate_2015 <- geocode(tmp_2015$Incident.Address, output = "latlon" , source = "google")
length(coordinate_2015[,1])

#write.csv(coordinate_2015, "/Users/mali/Documents/myGit/LRCrimeStudy/data/coordinate_for_LRCrime_2015")
tmp_2015$LONGTITUDE <- coordinate_2015$lon
tmp_2015$LATITUDE <- coordinate_2015$lat

tmp_add <- strsplit(as.character(tmp_2015$Incident.Address), ",")
tmp_frame_15 <- do.call(rbind, tmp_add)
tmp_2015$ADDRESS <- tmp_frame_15[, 1]
tmp_2015$CITY <- tmp_frame_15[, 2]
tmp_2015$STATE <- tmp_frame_15[, 3]
for (i in 1 : length(tmp_2015$Offense.Description)) {
  if (grepl("or", tmp_2015$Offense.Description[i])) {
    tmp_2015$Offense.Description[i] <- gsub("or.*", "", tmp_2015$Offense.Description[i])
  }
  if (grepl("\\(",  tmp_2015$Offense.Description[i])) {
    tmp_2015$Offense.Description[i] <- gsub("\\(.*", "", tmp_2015$Offense.Description[i])
  }
  if (grepl("on", tmp_2015$Offense.Description[i])) {
    tmp_2015$Offense.Description[i] <- gsub("on.*", "", tmp_2015$Offense.Description[i])
  }
}
tmp_2015$Incident.Address <- NULL

for (i in 1 : length(tmp_2015$OFFENS_DESCRIPTION)) {
  if (grepl("on", tmp_2015$OFFENS_DESCRIPTION[i], ignore.case = TRUE)) {
    tmp_2015$OFFENS_DESCRIPTION[i] <- gsub("on.*", "", tmp_2015$OFFENS_DESCRIPTION[i], ignore.case = TRUE)
  }
  if (grepl("or", tmp_2015$OFFENS_DESCRIPTION[i], ignore.case = TRUE)) {
    tmp_2015$OFFENS_DESCRIPTION[i] <- gsub("or.*", "", tmp_2015$OFFENS_DESCRIPTION[i], ignore.case = TRUE)
  }
}

colnames(tmp_2015) <- c("INCIDENT_NUMBER", "LOCATION_DISTRICT", 
                        "OFFENS_DESCRIPTION", "ymd", 
                        "month", "wday","LONGTITUDE", "LATITUDE", "LOCATION_ADDRESS", "CITY", "STATE")
#write.csv(tmp_2015, "/Users/mali/Documents/myGit/LRCrimeStudy/data/purged_crime_data_for_2015")
#####################################
#####################################

tmp_2016 <- read.table("/Users/mali/Documents/myGit/LRCrimeStudy/data/Little_Rock_Police_Department_Statistics_2016.csv", 
                       head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F)

#formalizing time format
for (i in 1 : length(tmp_2016$IncidentDate)) {
  if (grepl("\\-", tmp_2016$IncidentDate[i])) {
    tmp <- gsub("\\-.*", "", tmp_2016$IncidentDate[i])
    print(tmp)
    tmp_2016$IncidentDate[i] <- tmp
  }
  if (grepl(" ", tmp_2016$IncidentDate[i])) {
    tmp <- gsub(" .*", "", tmp_2016$IncidentDate[i])
    print(tmp)
    tmp_2016$IncidentDate[i] <- tmp
  }
}
tmp_2016$ymd <- mdy(tmp_2016$IncidentDate)
tmp_2016$month <- month(tmp_2016$ymd)
tmp_2016$wday <- weekdays(tmp_2016$ymd)

for (i in 1 : length(tmp_2016$IncidentAddress)) {
  if (grepl("\\n\\(.*", tmp_2016$IncidentAddress[i])) {
    print(tmp_2016$IncidentAddress[i])
    tmp_2016$IncidentAddress[i] <- gsub("\\n\\(.*", ",", tmp_2016$IncidentAddress[i])
    tmp_2016$IncidentAddress[i] <- gsub("\\n", ",", tmp_2016$IncidentAddress[i])
  } else {
    tmp_2016$IncidentAddress[i] <- gsub("\\n", ",", tmp_2016$IncidentAddress[i])
  }
}

#using the original address information fetching coordinate from google
register_google(key = "AIzaSyC4L_SU2ZLIqImTqhxJKUxycGq0oCSmrVE", account_type = "premium", day_limit = 100000)
coordinate_2016 <- geocode(tmp_2016$IncidentAddress, output = "latlon" , source = "google")
#write.csv(coordinate_2016, "/Users/mali/Documents/myGit/LRCrimeStudy/data/coordinate_for_LRCrime_2016")

coordinate_2016 <- read.table("/Users/mali/Documents/myGit/LRCrimeStudy/data/coordinate_for_LRCrime_2016.csv", 
                              head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F)
tmp_2016$LONGTITUDE <- coordinate_2016$lon
tmp_2016$LATITUDE <- coordinate_2016$lat

tmp_add <- strsplit(as.character(tmp_2016$IncidentAddress), ",")
tmp_frame_16 <- do.call(rbind, tmp_add)
tmp_2016$ADDRESS <- tmp_frame_16[, 1]
tmp_2016$CITY <- tmp_frame_16[, 2]
tmp_2016$STATE <- tmp_frame_16[, 3]
for (i in 1 : length(tmp_2016$offenseDesc1)) {
  if (grepl("or", tmp_2016$offenseDesc1[i])) {
    tmp_2016$offenseDesc1[i] <- gsub("or.*", "", tmp_2016$offenseDesc1[i])
  }
  if (grepl("\\(",  tmp_2016$offenseDesc1[i])) {
    tmp_2016$offenseDesc1[i] <- gsub("\\(.*", "", tmp_2016$offenseDesc1[i])
  }
  if (grepl("on", tmp_2016$offenseDesc1[i])) {
    tmp_2016$offenseDesc1[i] <- gsub("on.*", "", tmp_2016$offenseDesc1[i])
  }
}
tmp_2016$IncidentAddress <- NULL
tmp_2016$IncidentDate <- NULL
for (i in 1 : length(tmp_2016$OFFENS_DESCRIPTION)) {
  if (grepl("on", tmp_2016$OFFENS_DESCRIPTION[i], ignore.case = TRUE)) {
    tmp_2016$OFFENS_DESCRIPTION[i] <- gsub("on.*", "", tmp_2016$OFFENS_DESCRIPTION[i], ignore.case = TRUE)
  }
  if (grepl("or", tmp_2016$OFFENS_DESCRIPTION[i], ignore.case = TRUE)) {
    tmp_2016$OFFENS_DESCRIPTION[i] <- gsub("or.*", "", tmp_2016$OFFENS_DESCRIPTION[i], ignore.case = TRUE)
  }
}
colnames(tmp_2016) <- c("INCIDENT_NUMBER", "LOCATION_DISTRICT", 
                        "OFFENS_DESCRIPTION", "ymd", 
                        "month", "wday","LONGTITUDE", "LATITUDE", "LOCATION_ADDRESS", "CITY", "STATE")
#write.csv(tmp_2016, "/Users/mali/Documents/myGit/LRCrimeStudy/data/purged_crime_data_for_2016.csv")

########################
########################
incidents_ymd <- data.frame(c(data_2015$ymd, data_2016$ymd, data_2017$ymd, data_2018$ymd))

incidents_month <- data.frame(c(data_2015$month, data_2016$month, data_2017$month, data_2018$month))
incidents_wdays <- data.frame(c(data_2015$wday, data_2016$wday, data_2017$wday, data_2018$wday))
incidents_Description <- data.frame(c(data_2015$OFFENS_DESCRIPTION, data_2016$OFFENS_DESCRIPTION, 
                                      data_2017$OFFENSE_DESCRIPTION, data_2018$OFFENSE_DESCRIPTION))
incidents_District <- data.frame(c(data_2015$LOCATION_DISTRICT, data_2016$LOCATION_DISTRICT, 
                                   data_2017$LOCATION_DISTRICT, data_2018$LOCATION_DISTRICT))
incidents_Address <- data.frame(c(data_2015$LOCATION_ADDRESS, data_2016$LOCATION_ADDRESS, 
                                  data_2017$LOCATION_ADDRESS, data_2018$LOCATION_ADDRESS))
incidents_City <- data.frame(c(data_2015$CITY, data_2016$CITY, data_2017$CITY, data_2018$CITY))
incidents_State <- data.frame(c(data_2015$STATE, data_2016$STATE, data_2017$STATE, data_2018$STATE))
incidents_Lon <- data.frame(c(data_2015$LONGTITUDE, data_2016$LONGTITUDE, data_2017$LONGITUDE,
                              data_2018$LONGITUDE))
incidents_Lat <- data.frame(c(data_2015$LATITUDE, data_2016$LATITUDE, data_2017$LATITUDE, 
                              data_2018$LATITUDE))
incidents_Number <- data.frame(c(data_2015$INCIDENT_NUMBER, data_2016$INCIDENT_NUMBER, 
                                 data_2017$INCIDENT_NUMBER, data_2018$INCIDENT_NUMBER))

total_incidents <- cbind.data.frame(incidents_ymd, incidents_month, incidents_wdays, 
                                    incidents_District, incidents_Address, incidents_City, incidents_State, 
                                    incidents_Lon, incidents_Lat,
                                    incidents_Number, incidents_Description)
colnames(total_incidents) <- c("incidents_ymd", "incidents_month", "incidents_wdays", 
                               "incidents_District", "incidents_Address", "incidents_City", "incidents_State", 
                               "incidents_Lon", "incidents_Lat",
                               "incidents_Number", "incidents_Description")
head(total_incidents)

for (i in 1 : length(total_incidents$incidents_Description)) {
  if (grepl("of", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- gsub("of", "", total_incidents$incidents_Description[i], ignore.case = TRUE)
  }
  if (grepl("theft", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- "THEFT"
  }
  if (grepl("robbery", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- "ROBBERY"
  }
  if (grepl("batter*", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- "BATTERY"
  }
  if (grepl("burglary", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- "BURGLARY"
  }
  if (grepl("assault", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- "ASSAULT"
  }
  if (grepl("shoplifting", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- "THEFT"
  }
  if (grepl("terr", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- "TERR"
  }
  if (grepl("Ars", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- "ARS"
  }
  if (grepl("POCKET", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- "THEFT"
  }
  if (grepl("breaking", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- "BREAK IN"
  }
  if (grepl("homicide", total_incidents$incidents_Description[i], ignore.case = TRUE)) {
    total_incidents$incidents_Description[i] <- "HOMICIDE"
  }
}
total_crimes <- total_incidents[total_incidents$incidents_Description != "HOMICIDE",]


####################
####################
tmp_ttl <- read.table("/Users/mali/Documents/myGit/LRCrimeStudy/data/Crime_records_Of_LR_2015-2018.csv", head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F)
tmp_ttl$incidents_wdays
tmp_ttl$incidents_wdays <- tmp_tmp$incidents_wdays
for (i in 1 : length(tmp_ttl$incidents_wdays)) {
  if (grepl("mon", tmp_ttl$incidents_wdays[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_wdays[i] <- "MONDAY"
  }
  if (grepl("tue", tmp_ttl$incidents_wdays[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_wdays[i] <- "TUESDAY"
  }
  if (grepl("wed", tmp_ttl$incidents_wdays[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_wdays[i] <- "WEDNESDAY"
  }
  if (grepl("thu", tmp_ttl$incidents_wdays[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_wdays[i] <- "THURSDAY"
  }
  if (grepl("fri", tmp_ttl$incidents_wdays[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_wdays[i] <- "FRIDAY"
  }
  if (grepl("sat", tmp_ttl$incidents_wdays[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_wdays[i] <- "SATURDAY"
  }
  if (grepl("sun", tmp_ttl$incidents_wdays[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_wdays[i] <- "SUNDAY"
  }
}
tmp_ttl$incidents_wdays <- factor(tmp_ttl$incidents_wdays, levels = c("MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", 
                                  "FRIDAY", "SATURDAY", "SUNDAY"))
for (i in 1 : length(tmp_ttl$incidents_month)) {
  print(tmp_ttl$incidents_month[i])
  if (tmp_ttl$incidents_month[i] == "1" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "JAN"
  }
  if (tmp_ttl$incidents_month[i] == "2" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "FEB"
  }
  if (tmp_ttl$incidents_month[i] == "3" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "MAR"
  }
  if (tmp_ttl$incidents_month[i] == "4" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "APR"
  }
  if (tmp_ttl$incidents_month[i] == "5" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "MAY"
  }
  if (tmp_ttl$incidents_month[i] == "6" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "JUN"
  }
  if (tmp_ttl$incidents_month[i] == "7" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "JUL"
  }
  if (tmp_ttl$incidents_month[i] == "8" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "AUG"
  }
  if (tmp_ttl$incidents_month[i] == "9" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "SEP"
  }
  if (tmp_ttl$incidents_month[i] == "10" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "OCT"
  }
  if (tmp_ttl$incidents_month[i] == "11" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "NOV"
  }
  if (tmp_ttl$incidents_month[i] == "12" & !is.na(tmp_ttl$incidents_month[i])) {
    tmp_ttl$incidents_month[i] <- "DEC"
  }
}

for (i in 1 : length(tmp_ttl$incidents_month)) {
  print(tmp_ttl$incidents_month[i])
  if (grepl("9", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "SEP"
  }
  if (grepl("jan", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "JAN"
  }
  if (grepl("feb", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "FEB"
  }
  if (grepl("mar", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "MAR"
  }
  if (grepl("apr", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "APR"
  }
  if (grepl("may", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "MAY"
  }
  if (grepl("jun", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "JUN"
  }
  if (grepl("jul", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "JUL"
  }
  if (grepl("aug", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "AUG"
  }
  if (grepl("Sep", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "SEP"
  }
  if (grepl("oct", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "OCT"
  }
  if (grepl("nov", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "NOV"
  }
  if (grepl("dec", tmp_ttl$incidents_month[i], ignore.case = TRUE)) {
    tmp_ttl$incidents_month[i] <- "DEC"
  }
}
tmp_ttl$incidents_month <- factor(tmp_ttl$incidents_month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL",
                                           "AUG", "SEP", "OCT", "NOV", "DEC"))
class(tmp_ttl$incidents_wdays)
tmp_ttl$year <- year(tmp_ttl$incidents_ymd)
for (i in 1 : length(tmp_ttl$year)) {
  if (!is.na(tmp_ttl$year[i]) & tmp_ttl$year[i] < 2015) {
    print(tmp_ttl$year[i])
    tmp_ttl <- tmp_ttl[-i, ]
  }
}

for (i in 1 : length(tmp_ttl$year)) {
  
  if (!is.na(tmp_ttl$year[i]) & tmp_ttl$year[i] > 2018) {
    print(tmp_ttl$year[i])
    tmp_ttl[-i, ]
    tmp_ttl <- tmp_ttl[-i, ]
    
  }
}
factor(tmp_ttl$year)

tmp_ttl$X.4 <- NULL
tmp_ttl$X.3 <- NULL
tmp_ttl$X.2 <- NULL
tmp_ttl$X.1 <- NULL
tmp_ttl$X <- NULL
#write.csv(tmp_ttl, "/Users/mali/Documents/myGit/LRCrimeStudy/data/Crime_records_Of_LR_2015-2018.csv")

########################################################
########################################################
climate <- read.table("/Users/mali/Documents/myGit/LRCrimeStudy/data/climate.csv", head=TRUE, sep=",", fill=TRUE, stringsAsFactors=F)
head(climate)
daily_climate <- data.frame(climate$DATE, climate$LATITUDE, 
                            climate$LONGITUDE, climate$DAILYMaximumDryBulbTemp, climate$DAILYMinimumDryBulbTemp, stringsAsFactors = F)
monthly_climate <- data.frame(climate$DATE, climate$LONGITUDE, 
                              climate$LATITUDE, climate$MonthlyMaximumTemp, climate$MonthlyMeanTemp, climate$MonthlyMaximumTemp, stringsAsFactors = F)
colnames(daily_climate) <- c("date", "lon", "lat", "dailyMax", "dailyMin")
colnames(monthly_climate) <- c("date", "lon", "lat", "monthlyMin", "monthlyMean", "monthlyMax")

#removing na
daily_climate <- daily_climate[!is.na(daily_climate$dailyMax), ]
daily_climate <- daily_climate[!is.na(daily_climate$dailyMin), ]
class(length(daily_climate$date))

for (i in 1 : length(daily_climate$date)) {
  print(daily_climate$date[i])
  daily_climate$date[i] <- gsub("\\/", "\\-", daily_climate$date[i])
  print(daily_climate$date[i])
  daily_climate$date[i] <- gsub(" .[0-9].*\\:.*", "", daily_climate$date[i])
  print(daily_climate$date[i])
  daily_climate$date[i] <- transfer2ymd(daily_climate$date[i])
}

for (indicator in 1 : length(daily_climate$date)) {
  i = indicator + 1
  while (i <= length(daily_climate$date) & (daily_climate$date[i] == daily_climate$date[indicator])) {
    daily_climate <- daily_climate[-i, ]
    i = i + 1
  }
  indicator = i
  
}
 
daily_climate$lat <- NULL
daily_climate$lon <- NULL
#write.csv(daily_climate, "/Users/mali/Documents/myGit/LRCrimeStudy/data/purged_daily_climate_Of_LR_2015-2018.csv")

################################
monthly_climate <- monthly_climate[!is.na(monthly_climate$monthlyMin), ]
monthly_climate <- monthly_climate[!is.na(monthly_climate$monthlyMax), ]
class(length(monthly_climate$date))

for (i in 1 : length(monthly_climate$date)) {
  print(monthly_climate$date[i])
  monthly_climate$date[i] <- gsub("\\/", "\\-", monthly_climate$date[i])
  print(daily_climate$date[i])
  monthly_climate$date[i] <- gsub(" .[0-9].*\\:.*", "", monthly_climate$date[i])
  print(monthly_climate$date[i])
  monthly_climate$date[i] <- transfer2ymd(monthly_climate$date[i])
}
monthly_climate$lat <- NULL
monthly_climate$lon <- NULL

monthly_climate$year <- year(monthly_climate$date)
monthly_climate$month <- month(monthly_climate$date)
monthly_climate$date <- NULL
factor(monthly_climate$month, labels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
#write.csv(monthly_climate, "/Users/mali/Documents/myGit/LRCrimeStudy/data/purged_monthly_climate_Of_LR_2015-2018.csv")

