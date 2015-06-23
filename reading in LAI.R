##reading in Andrew Richardson's LAI data
LAI <- read.csv("~/Downloads/LAI%20for%20ELI.csv")
View(LAI)
summary(LAI)

#looking at date range for single site at a time
summary(LAI$ID[LAI$Site=="Ca-Oas"])
LAI$ID[LAI$Site]

#formatting year.decimals as r-friendly dates
library(lubridate)
Date <- format(date_decimal(LAI$ID), "%m-%d-%Y")

