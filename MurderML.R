pkg <- c("data.table","ggplot2","stringr","dplyr","gganimate","RSocrata","ggmap")
suppressWarnings(suppressPackageStartupMessages(
    invisible(lapply(pkg, function(x) require(x, character.only = T, quietly = T)))
))

#PI <- read.socrata("https://www.dallasopendata.com/resource/qv6i-rri7.csv")
#saveRDS(PI,file = "C:/Users/sconroy/Desktop/Debug/PoliceIncidents12-17-20.RDS")
#setDT(PI)

#Murder <- PI[grepl("MURDER",offincident) | grepl("HOMICIDE",offincident),]
Murder <- readRDS("C:/Users/sconroy/Desktop/Debug/Murder.RDS")


# Extract Lat / Long frm geocoded column
Murder[,rowid := 1:.N]
Murder[,Date := as.Date(substr(date1,1,10))]
Murder[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
Murder[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
Murder[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
Murder[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
Murder[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
Murder[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
Murder[,Longitude := as.numeric(Longitude)]
Murder[,Latitude := as.numeric(Latitude)]
Murder[,Year := as.factor(servyr)]


lats <- c(min(Murder$Latitude,na.rm = TRUE),max(Murder$Latitude,na.rm = TRUE))
lons <- c(min(Murder$Longitude,na.rm = TRUE),max(Murder$Longitude,na.rm = TRUE))
bb <- make_bbox(lon=lons,lat=lats,f=0.05)

# Get map of Dallas from Google API
DallasMapZoom11 <- get_map(location = bb, zoom = 11, maptype = "roadmap",source = "google",messaging = FALSE)
#DallasMapZoom12 <- get_map(location = "Dallas", zoom = 12, source = "google")


# All Shootings since 2014
ggmap(DallasMapZoom11,extent = "device") + 
    geom_point(data = Murder, aes(x = Longitude, y = Latitude,color = as.factor(watch)),size = 2) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggtitle("Dallas Murders since 2014")

