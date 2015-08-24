#reading in Andrew Richardson's LAI data
LAI <- read.csv("~/Downloads/LAI%20for%20ELI.csv")
View(LAI)
summary(LAI)

#converting Year into character value
LAI$chYear<-as.character(LAI$Year)
summary(chYear)
levels(chYear) #doesn't work to give range

#formatting year.decimals as r-friendly dates
library(lubridate)
Date <- format(date_decimal(LAI$ID), "%m-%d-%Y")

#plots to see years at each site
###US-Ha1
plot(LAI$Year[LAI$Site=="US-Ha1"], LAI$Site[LAI$Site=="US-Ha1"])
###US-MMS
plot(LAI$Year[LAI$Site=="US-MMS"], LAI$Site[LAI$Site=="US-MMS"])
###US-UMB
(plot(LAI$Year[LAI$Site=="US-UMB"], LAI$Site[LAI$Site=="US-UMB"])
###US-WCr
plot(LAI$Year[LAI$Site=="US-WCr"], LAI$Site[LAI$Site=="US-WCr"])
 
#plots to see for which years LAI data is available
###US-Ha1
plot(LAI$Year[LAI$Site=="US-Ha1"],LAI$LAI2000_raw[LAI$Site=="US-Ha1"], xaxt='n')
  axis(1, at=1990:2010, labels=NULL)
plot(LAI$Year[LAI$Site=="US-Ha1"],LAI$LAI2000_LAIr[LAI$Site=="US-Ha1"], xaxt='n')
  axis(1, at=1990:2010, labels=NULL)
###US-MMS
plot(LAI$Year[LAI$Site=="US-MMS"],LAI$LAI2000_raw[LAI$Site=="US-MMS"], xaxt='n')
  axis(1, at=1990:2010, labels=NULL)
plot(LAI$Year[LAI$Site=="US-MMS"],LAI$LAI2000_LAIr[LAI$Site=="US-MMS"], xaxt='n')
  axis(1, at=1990:2010, labels=NULL)
###US-UMB
plot(LAI$Year[LAI$Site=="US-UMB"],LAI$LAI2000_raw[LAI$Site=="US-UMB"], xaxt='n')
  axis(1, at=1990:2010, labels=NULL)
plot(LAI$Year[LAI$Site=="US-UMB"],LAI$LAI2000_LAIr[LAI$Site=="US-UMB"], xaxt='n')
  axis(1, at=1990:2010, labels=NULL)
###US-WCr
plot(LAI$Year[LAI$Site=="US-WCr"],LAI$LAI2000_raw[LAI$Site=="US-WCr"])
  axis(1, at=1998:2004, labels=NULL) #error: need finite ylim values
plot(LAI$Year[LAI$Site=="US-WCr"],LAI$LAI2000_LAIr[LAI$Site=="US-WCr"], xaxt='n')
  axis(1, at=1998:2004, labels=NULL) #error: need finite ylim values
LAI$LAI2000_raw[LAI$Site=="US-WCr"] #all read "NA"; no LAI data available at this site?
