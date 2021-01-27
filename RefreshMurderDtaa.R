library(rdrop2)
library(data.table)
library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)
library(RSocrata)

PI <- readRDS("C:/Users/sconroy/Documents/DallasPoliceData/PoliceIncidents1-9-21.RDS")
setDT(PI)

lastRefreshDate <- as.Date(max(PI$date1,na.rm = TRUE))

refreshString <- paste0("https://www.dallasopendata.com/Public-Safety/Police-Incidents/qv6i-rri7?$where=date1>'",
                        lastRefreshDate,"'")
refreshString
newPI <- read.socrata(refreshString)
setDT(newPI)

PI <- rbindlist(list(PI,newPI))

Murder <- PI[grepl("MURDER",offincident) | grepl("HOMICIDE",offincident) | grepl("MURDER",ucr_offense) | 
                 grepl("HOMICIDE",nibrs_crime_category) | grepl("MURDER",nibrs_crime),]


max(Murder$date1)

Murder[,rowid := 1:.N]

Murder[,Date := as.Date(substr(date1,1,10))]
setorder(Murder,Date)

Murder[,MonthDate := as.Date(paste0(format(Date,"%Y-%m"),"-01"))]
Murder[,WeekNum := strftime(Date, format = "%V")]
Murder <- merge(Murder,Murder[,head(.SD, 1L),.SDcols = "Date",by = c("servyr","WeekNum")],by = c("servyr","WeekNum"))
setnames(Murder,old = c("Date.x","Date.y"),new = c("Date","WeekDate"))

Murder[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
Murder[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
Murder[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
Murder[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
Murder[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
Murder[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
Murder[,Longitude := as.numeric(Longitude)]
Murder[,Latitude := as.numeric(Latitude)]
Murder[,Year := as.factor(servyr)]
Murder$servyr <- NULL
Murder <- Murder[!is.na(Latitude) & !is.na(Longitude) & !is.na(Date) & !is.na(Year),]
Murder[,NumPerYear := 1:.N,by = Year]
Murder[,c("LatLongStart","LatLongEnd","LatLong","LatLongComma") := NULL]

setnames(Murder,old = c("watch","offincident","comprace","compsex","compage","compethnicity","status","victimcond","beat"),
         c("Watch","Officer_Incident","Comp_Race","Comp_Sex","Comp_Age","Comp_Ethnicity","Status","Victim_Condition","Beat"),
         skip_absent = TRUE)


saveRDS(Murder,"C:/Users/sconroy/Documents/DallasPoliceData/Murder.RDS")
