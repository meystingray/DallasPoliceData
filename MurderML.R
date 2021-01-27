
library(data.table)

PI <- readRDS("C:/Users/sconroy/Documents/DallasPoliceData/PoliceIncidents1-9-21.RDS")
setDT(PI)

PI[,rowid := 1:.N]

PI[,Date := as.Date(substr(date1,1,10))]

PI <- PI[Date >= as.Date("2014-06-01"),]
setorder(PI,Date)

PI[,MonthDate := as.Date(paste0(format(Date,"%Y-%m"),"-01")),by = Date]

PI[,WeekNum := strftime(Date, format = "%V-%Y"),by = Date]
PI[,WeekDate := min(Date),by = WeekNum]


ggplot(PI,aes(x = WeekDate)) + geom_line(stat = 'count')


library(prophet)

PI[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
PI[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
PI[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
PI[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
PI[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
PI[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
PI[,Longitude := as.numeric(Longitude)]
PI[,Latitude := as.numeric(Latitude)]
PI[,Year := as.factor(servyr)]
PI$servyr <- NULL
PI <- PI[!is.na(Latitude) & !is.na(Longitude) & !is.na(Date) & !is.na(Year),]
PI[,NumPerYear := 1:.N,by = Year]
PI[,c("LatLongStart","LatLongEnd","LatLong","LatLongComma") := NULL]

PI[grepl("SUBSTANCE",offincident) | grepl("MARIJUANA",offincident) | grepl("DRUG",offincident),
   unique(offincident)]


unique(PI$offincident)
table(PI$drug)

PI[grepl("MURDER",offincident) | grepl("HOMICIDE",offincident) | grepl("MURDER",ucr_offense) | 
     grepl("HOMICIDE",nibrs_crime_category) | grepl("MURDER",nibrs_crime),NumMurdersPerMonth := .N,by = MonthDate]

PI[,NumMurdersPerMonth := mean(NumMurdersPerMonth,na.rm = TRUE),by = MonthDate]


PI[grepl("DRUG",offincident),NumDrugsPerMonth := .N,by = MonthDate]

PI[,NumDrugsPerMonth := mean(NumDrugsPerMonth,na.rm = TRUE),by = MonthDate]


PI[grepl("KIDNAP",offincident),NumKidnapPerMonth := .N,by = MonthDate]

PI[,NumKidnapPerMonth := mean(NumKidnapPerMonth,na.rm = TRUE),by = MonthDate]


PI[grepl("ACCIDENT",offincident),NumAccidentsPerMonth := .N,by = MonthDate]

PI[,NumAccidentsPerMonth := mean(NumAccidentsPerMonth,na.rm = TRUE),by = MonthDate]


PI[grepl("ASSAULT",offincident),NumAssaultsPerMonth := .N,by = MonthDate]

PI[,NumAssaultsPerMonth := mean(NumAssaultsPerMonth,na.rm = TRUE),by = MonthDate]


PI[grepl("THEFT",offincident),NumTheftsPerMonth := .N,by = MonthDate]

PI[,NumTheftsPerMonth := mean(NumTheftsPerMonth,na.rm = TRUE),by = MonthDate]

library(ggplot2)

combined <- PI[,head(.SD,1),
               .SDcols = c("NumMurdersPerMonth","NumAssaultsPerMonth","NumKidnapPerMonth",
                           "NumTheftsPerMonth","NumAccidentsPerMonth","NumDrugsPerMonth"),
               by = MonthDate]

combined <- combined[MonthDate < as.Date("2021-01-01")]
combined <- melt(combined,
                 measure.vars = c("NumMurdersPerMonth","NumAssaultsPerMonth","NumTheftsPerMonth",
                                  "NumAccidentsPerMonth","NumDrugsPerMonth","NumKidnapPerMonth"),
             id.vars = c("MonthDate"))

combined[variable == "NumMurdersPerMonth",variable := "# Murders Per Month"]
combined[variable == "NumAssaultsPerMonth",variable := "# Assaults Per Month"]
combined[variable == "NumTheftsPerMonth",variable := "# Thefts Per Month"]
combined[variable == "NumAccidentsPerMonth",variable := "# Vehicle Accidents Per Month"]
combined[variable == "NumDrugsPerMonth",variable := "# Drug Incidents Per Month"]
combined[variable == "NumKidnapPerMonth",variable := "# Kidnapping Incidents Per Month"]

setnames(combined,old = names(combined),new = c("Date","Crime","# Per Month"))

ggplot(combined,aes(x = Date)) + geom_point(aes(y = `# Per Month`,color = Crime),na.rm = TRUE) +
  geom_smooth(aes(y = `# Per Month`,color = Crime),method = "loess",formula = y ~ x,n = 50,na.rm = TRUE) + 
  facet_wrap(~Crime,scales = "free") + theme(legend.position = "none")

combined
