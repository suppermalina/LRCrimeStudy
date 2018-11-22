if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force = TRUE)
library("ggmap")
citation("ggmap")

#The following three lines used to test if the ggmap has been installed properly
us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(map)

#Functions to work with date-times and time-spans: fast and user friendly parsing of date-time data, 
#extraction and updating of components of a date-time (years, months, days, hours, minutes, and seconds), 
#algebraic manipulation on date-time and time-span objects. 
#The 'lubridate' package has a consistent and memorable syntax that makes working with dates easy and fun.
#Parts of the 'CCTZ' source code, released under the Apache 2.0 License,
#are included in this package. See <https://github.com/google/cctz> for more details.
install.packages("lubridate")



