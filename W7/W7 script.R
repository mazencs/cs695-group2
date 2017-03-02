#********************************************
#         Create a Customer Profile
#********************************************
install.packages("readr")
install.packages("googleVis")
install.packages("plyr")
install.packages("stringr")
install.packages("stringi")
install.packages("magrittr")
install.packages("dplyr")

library(readr)
library("googleVis")
library("plyr")

# Load data
Trump <- read_csv("~/Trump.csv")

# Show the distribution of users by location (city) with Google Map
# !!! It takes time to show all the locations, so be patient !!!
Trump$city <- tolower(Trump$USER_CITY)
citySum = data.frame(table(Trump$city))

GeoStates <- gvisGeoChart(citySum, "Var1", "Freq",
                          options=list(region="US", 
                                       #  displayMode="regions", 
                                       displayMode="markers",
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)

# User gender distribution by city
# Get subset of the data to explore
subdf <- subset(Trump, city =='new york city' | city =='brooklyn')
genderCity <-data.frame(table(subdf[,c("city", "USER_GENDER")]))
# xtabs(Freq~city+USER_GENDER,genderCity)
tab = reshape(genderCity,direction="wide",timevar="USER_GENDER",idvar="city")
Column <- gvisColumnChart(tab)
plot(Column)


# User posting time by gender
Trump$days <- weekdays(as.POSIXlt(Trump$MESSAGE_POSTED_TIME))
dfrm <-data.frame(table(Trump[,c("USER_GENDER","days")]))
genderDays = reshape(dfrm,direction="wide",timevar="days",idvar="USER_GENDER")
Bar <- gvisBarChart(genderDays)
plot(Bar)
