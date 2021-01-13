# Police Incidents, https://www.dallasopendata.com/Public-Safety/Police-Incidents/qv6i-rri7
PI <- read.socrata("https://www.dallasopendata.com/resource/qv6i-rri7.csv")
#PI <- readRDS("C:/Users/sconroy/Desktop/Debug/PoliceIncidents.RDS")
PI <- readRDS("C:/Users/sconroy/Documents/DallasPoliceData/PoliceIncidents1-9-21.RDS")

library(data.table)
library(ggplot2)
library(stringr)
library(RSocrata)
setDT(PI)


unique(PI$offincident)
unique(PI$comp)

# PI Badge Analysis
PI[,Date := as.Date(substr(date1,1,10))]
PI[,MonthDate := as.Date(paste0(format(Date,"%Y-%m"),"-01"))]

NumOfficers <- data.table(MonthDate = unique(PI$MonthDate))
for (m in NumOfficers$MonthDate) {
    
    NumOfficers[MonthDate == m,NumOfficers := 
                    length(unique(c(
                        PI[MonthDate == m,unique(ro1badge)],
                        PI[MonthDate == m,unique(ro2badge)],
                        PI[MonthDate == m,unique(assoffbadge)]
                    )))]
    
}
NumOfficers <- NumOfficers[MonthDate >= as.Date("2014-06-01") & MonthDate < as.Date("2021-01-01"),]

library(ggplot2)
ggplot(NumOfficers,aes(x = MonthDate,y = NumOfficers)) + geom_line() + geom_smooth(n = 50)


# Extract murder incidents by looking for "MURDER" or "HOMICIDE" in the officer's incident description.
PI[grepl("MANSLAUGHTER",offincident),unique(offincident)]
PI[Date >= as.Date("2020-01-01") & victimcond == "Deceased",unique(offincident)]
PI[victimcond == "Deceased",unique(offincident)]
Murder <- PI[grepl("MURDER",offincident) | grepl("HOMICIDE",offincident) | grepl("MURDER",ucr_offense) | 
                 grepl("HOMICIDE",nibrs_crime_category) | grepl("MURDER",nibrs_crime),]
Murder <- merge(x = Murder,y = NumOfficers,by.x = "MonthDate",by.y = "MonthDate")
#saveRDS(Murder,file = "C:/Users/sconroy/Documents/DallasPoliceData/Murder.RDS")
#Murder <- readRDS("C:/Users/sconroy/Desktop/Debug/Murder.RDS")


# Clean Up Data
Murder[,Date := as.Date(substr(date1,1,10))]
Murder[,WeekNum := strftime(Date, format = "%V")]
Murder <- merge(Murder,Murder[,head(.SD, 1L),.SDcols = "Date",by = c("servyr","WeekNum")],by = c("servyr","WeekNum"))
setnames(Murder,old = c("Date.x","Date.y"),new = c("Date","WeekDate"))
Murder[,MonthDate := as.Date(paste0(format(Date,"%Y-%m"),"-01"))]

Murder[,NumPerDay := .N,by = Date]
Murder[,NumPerWeek := .N,by = WeekDate]
Murder[,NumPerMonth := .N,by = MonthDate]

# Smooth Murder Rates for plotting
Murder[,SmoothNumPerDay := predict(smooth.spline(NumPerDay,df = 20))$y]
Murder[,SmoothNumPerWeek := predict(smooth.spline(NumPerWeek,df = 55))$y]
Murder[,SmoothNumPerMonth := predict(smooth.spline(NumPerMonth,df = 10))$y]

# Plot Num of Murders per Month & Day
ggplot(Murder) +
    geom_line(aes(x = MonthDate,y = SmoothNumPerMonth,color = "red"),size = 1) + 
    #geom_point(aes(x = Date,y = NumPerMonth,color = "Red")) + 
    geom_line(aes(x = MonthDate,y = NumOfficers/120,color = "blue"),size = 1) +
    ggtitle("Dallas Murder Rates since 2014") + ylab("# Murders") + 
    scale_colour_manual(name = '',values = c('blue'='blue','red'='red'),
                        labels = c('# Officers Making Arrests in DPD Per Month','Dallas Murders Per Month')) + 
    theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + 
    scale_y_continuous(name = "Num Murders Per Month", 
        sec.axis = sec_axis(~ .*180, name = "Num Officers")
    ) +
    theme(axis.text.y.left = element_text(colour = "red"),
          axis.text.y.right = element_text(colour = "blue"),
        axis.title.y.left = element_text(color = "red"),
        axis.title.y.right = element_text(color = "blue"))


ggplot(Murder) +
    geom_point(aes(x = WeekDate,y = NumPerWeek)) + 
    geom_line(aes(x = WeekDate,y = SmoothNumPerWeek),color = "red") +
    ggtitle("Murder Rates per Week") + ylab("# Murders")

library(ggplot2)
ggplot(Murder[Date >= as.Date("2020-06-01"),]) +
    geom_point(aes(x = Date,y = NumPerDay)) + 
    geom_line(aes(x = WeekDate,y = SmoothNumPerDay),color = "red") +
    ggtitle("Murder Rates per Day") + ylab("# Murders")

Murders2020 <- Murder[Date >= as.Date("2020-01-01"),]
Murder[Date >= as.Date("2020-01-01"),.(WeekDate,WeekNum,NumPerWeek,SmoothNumPerWeek)]

# Plot 2020 Only Murder Rate per Month
ggplot(Murder[Date > as.Date("2020-01-01"),],aes(x = MonthDate)) + geom_bar() +
    ylab("# Murders per Month") + ggtitle("2020 Murders Per Month") +
    geom_text(aes(x = MonthDate,y = NumPerMonth,label = NumPerMonth),vjust = -0.25)


# Plot by Incident Type
table(Murder$offincident)
Murder[offincident == "CAPITAL MURDER BY TERROR THREAT/OTHER FELONY",offincident := "CAPITAL MURDER BY TERROR THREAT"]
ggplot(Murder[!(offincident %in% c("CAPITAL MURDER WHILE REMUNERATION","CRIMINAL NEGLIGENT HOMICIDE (DISTRACTED DRIVING"))]) +
    geom_bar(aes(x = MonthDate)) + facet_wrap(~ offincident)

    
# Victim Rate per Race
Murder[,NumPerRace := .N,by = comprace]
Murder[,PercentPerRace := round(100*.N/nrow(Murder),digits = 0),by = comprace]
Murder[,PercentPerRace := paste0(PercentPerRace,"%")]
ggplot(Murder) + geom_bar(aes(x = comprace)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    xlab("") + ylab("# per Victim Race") + 
    geom_text(aes(x = comprace,y = NumPerRace,label = PercentPerRace),vjust = -0.25) +
    ggtitle("Victim Count by Race")


# Victim Rate per Watch
# Time Reference: https://www.dallaspolice.net/joindpd/Pages/SalaryBenefits.aspx
Murder[,NumPerWatch := .N,by = watch]
Murder[watch == 1,Shift := "11pm - 7am"]
Murder[watch == 2,Shift := "7am - 3pm"]
Murder[watch == 3,Shift := "3pm - 11pm"]
Murder$Shift <- factor(Murder$Shift,levels = c("11pm - 7am", "7am - 3pm", "3pm - 11pm"))
ggplot(Murder,aes(x = Shift)) + geom_bar() + 
    xlab("") + ylab("# per Shift") + geom_text(aes(x = Shift,y = NumPerWatch,label = NumPerWatch),vjust = -0.25) +
    ggtitle("Murder Rate per Shift")

# Num Incidents Per Signal
Murder[,NumPerSignal := .N,by = signal]
ggplot(Murder[NumPerSignal > 5,],aes(x = reorder(signal, -NumPerSignal))) + geom_bar() + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
    xlab("") + ylab("# per Signal") + geom_text(aes(x = signal,y = NumPerSignal,label = NumPerSignal),vjust = -0.25) +
    ggtitle("Murder Rate per Signal")


# Shootings Per Year
Murder[signal == "19 - SHOOTING",NumShootingsPerYear := .N,by = servyr]
ggplot(Murder[signal == "19 - SHOOTING",],aes(x = servyr)) + geom_bar() +  
    xlab("") + ylab("# per year") + geom_text(aes(x = servyr,y = NumShootingsPerYear,label = NumShootingsPerYear),
                                              vjust = -0.25) +
    ggtitle("Num Shootings Per Year")

# Shootings Per Area
Murder[,NumPerAttack := .N,by = objattack]
ggplot(Murder[NumPerAttack > 5,],aes(x = reorder(objattack, -NumPerAttack))) + geom_bar() + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
    xlab("") + ylab("# per Attack") + geom_text(aes(x = objattack,y = NumPerAttack,label = NumPerAttack),vjust = -0.25) +
    ggtitle("Murder Rate per Location")

unique(Murder$beat)
unique(Murder$sector)
unique(Murder$division)
unique(Murder$district)
unique(Murder$taag)
unique(Murder$community)
unique(Murder$compsex)
unique(Murder$compage)
unique(Murder$comprace)

# Age distribution of victims per year
ggplot(Murder,aes(x = compage)) + geom_bar() + facet_wrap(~ servyr)
ggplot(Murder,aes(x = compage)) + geom_histogram(bins = 10) + facet_wrap(~ servyr)

# Sex distrbution
Murder[,NumPerYear := .N,by = c("servyr")]
Murder[,NumPerSexPerYear := .N,by = c("compsex","servyr")]
Murder[,PercentPerSexPerYear := .N/NumPerYear,by = c("compsex","servyr")]
Murder$PercentPerSexPerYear <- paste0(format(100*Murder$PercentPerSexPerYear,digits = 0),"%")
ggplot(Murder,aes(x = compsex)) + geom_bar() + facet_wrap(~ servyr) + 
    geom_text(aes(x = compsex,y = NumPerSexPerYear,
                  label = PercentPerSexPerYear),
                  vjust = -0.25) +
    ggtitle("Murder Rate per Sex")


# Victim Rates Per Year Per Communities
ggplot(Murder[comprace == "Black" & compsex == "Male" & compage < 30,],aes(x = servyr)) + geom_bar() + ggtitle("Murder Rate Per Year: Black Males Under 30")
ggplot(Murder[comprace == "Black" & compsex == "Male" & compage >= 30,],aes(x = servyr)) + geom_bar() + ggtitle("Murder Rate Per Year: Black Males Over 30")
ggplot(Murder[comprace == "Black" & compsex == "Female",],aes(x = servyr)) + geom_bar() + ggtitle("Murder Rate Per Year: Black Females")
ggplot(Murder[comprace == "White" & compsex == "Male" & compage < 30,],aes(x = servyr)) + geom_bar() + ggtitle("Murder Rate Per Year: White Males Under 30")
ggplot(Murder[comprace == "White" & compsex == "Male" & compage >= 30,],aes(x = servyr)) + geom_bar() + ggtitle("Murder Rate Per Year: White Males Over 30")
ggplot(Murder[comprace == "Hispanic or Latino" & compsex == "Male" & compage < 30,],aes(x = servyr)) + geom_bar() + ggtitle("Murder Rate Per Year: Hispanic Males Under 30")
ggplot(Murder[comprace == "Hispanic or Latino" & compsex == "Male" & compage >= 30,],aes(x = servyr)) + geom_bar() + ggtitle("Murder Rate Per Year: Hispanic Males Over 30")

#We see a significant increase in White and Hispanic Males Over 30 and a Significant Decrease in While Males Under 30. 

# 
unique(Murder$nibrs_crime)
table(Murder$victiminjurydesc)
table(Murder$gang)
table(Murder$drug)
unique(Murder$geocoded_column)



# Extract Lat / Long frm geocoded column
Murder[,rowid := 1:.N]
Murder[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
Murder[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
Murder[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
Murder[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
Murder[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
Murder[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]

Murder[,.(LatLongStart,LatLongEnd,LatLong,Latitude,Longitude)]

# Shootings by Year on Map
#ggplot(Murder) + geom_point(aes(x = Longitude,y = Latitude,color = signal)) + facet_wrap(~ servyr) + ylab(NULL)
#ggplot(Murder[signal == "19 - SHOOTING",]) + geom_point(aes(x = Longitude,y = Latitude)) + facet_wrap(~ servyr) + ylab(NULL)

library(ggmap)
apiKey <- fread("./APIkey.key")
apiKey <- names(apiKey)
register_google(key = apiKey)

DallasMap <- get_map(location = "Dallas", zoom = 11, source = "google")
ggmap(DallasMap)

Murder$Longitude <- as.numeric(Murder$Longitude)
Murder$Latitude <- as.numeric(Murder$Latitude)

ggmap(DallasMap,extent = "normal") + 
    geom_point(data = Murder, aes(x = Longitude, y = Latitude), size = 1) + facet_wrap(~ servyr)



