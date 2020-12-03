# Police Incidents, https://www.dallasopendata.com/Public-Safety/Police-Incidents/qv6i-rri7
PI <- read.socrata("https://www.dallasopendata.com/resource/qv6i-rri7.csv")
#PI <- readRDS("C:/Users/sconroy/Desktop/Debug/PoliceIncidents.RDS")

library(data.table)
library(ggplot2)
library(stringr)

setDT(PI)

# Extract murder incidents by looking for "MURDER" or "HOMICIDE" in the officer's incident description.
Murder <- PI[grepl("MURDER",offincident) | grepl("HOMICIDE",offincident),]
#saveRDS(Murder,file = "C:/Users/sconroy/Desktop/Debug/Murder.RDS")

Murder[,Date := as.Date(substr(date1,1,10))]

# Clean Up Data 
Murder[,MonthDate := as.Date(paste0(format(Date,"%Y-%m"),"-01"))]
setorder(Murder,Date)

Murder[,NumPerDay := .N,by = Date]
Murder[,NumPerMonth := .N,by = MonthDate]

# Smooth Murder Rates for plotting
Murder[,SmoothNumPerDay := predict(smooth.spline(NumPerDay,df = 20))$y]
Murder[,SmoothNumPerMonth := predict(smooth.spline(NumPerMonth,df = 10))$y]

# Plot Num of Murders per Month & Day
ggplot(Murder) +
    geom_line(aes(x = MonthDate,y = SmoothNumPerMonth,color = "red")) + 
    geom_point(data = Murder,aes(x = Date,y = NumPerMonth)) + 
    geom_line(aes(x = Date,y = SmoothNumPerDay,color = "blue")) +
    ggtitle("Murder Rates") + ylab("# Murders") + 
    scale_colour_manual(name = '',values = c('blue'='blue','red'='red'),
                        labels = c('Per Day','Per Month'))

# Plot by Incident Type
table(Murder$offincident)
ggplot(Murder[!(offincident %in% c("CAPITAL MURDER WHILE REMUNERATION","CRIMINAL NEGLIGENT HOMICIDE (DISTRACTED DRIVING"))],
       aes(x = MonthDate,y = SmoothNumPerMonth)) + geom_line() + geom_point() + facet_wrap(~ offincident)

# Plot 2020 Only Murder Rate per Month
ggplot(Murder[Date > as.Date("2020-01-01"),],aes(x = MonthDate)) + geom_bar() +
    ylab("# Murders per Month") + ggtitle("2020 Murders Per Month") +
    geom_text(aes(x = MonthDate,y = NumPerMonth,label = NumPerMonth),vjust = -0.25)
    
# Victim Rate per Race
Murder[,NumPerRace := .N,by = comprace]
ggplot(Murder,aes(x = comprace)) + geom_bar() + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    xlab("") + ylab("# per Victim Race") + geom_text(aes(x = comprace,y = NumPerRace,label = NumPerRace),vjust = -0.25) +
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


# Increases Per Year Per Communities
Murder[,NumPerRace := .N,by = comprace]
ggplot(Murder[comprace == "Black" & compsex == "Male" & compage < 30,],aes(x = servyr)) + geom_bar()
ggplot(Murder[comprace == "Black" & compsex == "Male" & compage > 30,],aes(x = servyr)) + geom_bar()
ggplot(Murder[comprace == "Black" & compsex == "Female",],aes(x = servyr)) + geom_bar()
ggplot(Murder[comprace == "White" & compsex == "Male" & compage < 30,],aes(x = servyr)) + geom_bar()
ggplot(Murder[comprace == "Hispanic or Latino" & compsex == "Male" & compage < 30,],aes(x = servyr)) + geom_bar()
ggplot(Murder[comprace == "Hispanic or Latino" & compsex == "Male" & compage > 30,],aes(x = servyr)) + geom_bar()

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

DallasMap <- get_map(location = "Dallas", zoom = 11, source = "google")
ggmap(DallasMap)

Murder$Longitude <- as.numeric(Murder$Longitude)
Murder$Latitude <- as.numeric(Murder$Latitude)

ggmap(DallasMap,extent = "normal") + 
    geom_point(data = Murder, aes(x = Longitude, y = Latitude), size = 1) + facet_wrap(~ servyr)



