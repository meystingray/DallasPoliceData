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
#Murder[,.(LatLongStart,LatLongEnd,LatLong,Latitude,Longitude)]
MurderWithLatLong <- Murder[!is.na(Latitude) & !is.na(Longitude) & !is.na(Date) & !is.na(servyr),
                            .(Date,Longitude,Latitude,Year,servyr)]
Murders2020 <- MurderWithLatLong[Date > as.Date("2020-01-01"),.(Date,Latitude,Longitude)]

# Shootings by Year on Map
#ggplot(Murder) + geom_point(aes(x = Longitude,y = Latitude,color = signal)) + facet_wrap(~ servyr) + ylab(NULL)
#ggplot(Murder[signal == "19 - SHOOTING",]) + geom_point(aes(x = Longitude,y = Latitude)) + facet_wrap(~ servyr) + ylab(NULL)


# Load our API key for Google
apiKey <- fread("C:/Users/sconroy/Documents/DallasPoliceData/APIkey.key")
apiKey <- names(apiKey)
register_google(key = apiKey)

lats <- c(min(MurderWithLatLong$Latitude),max(MurderWithLatLong$Latitude))
lons <- c(min(MurderWithLatLong$Longitude),max(MurderWithLatLong$Longitude))
bb <- make_bbox(lon=lons,lat=lats,f=0.05)


# Get map of Dallas from Google API
DallasMapZoom10 <- get_map(location = "Dallas", zoom = 10, maptype = "roadmap",source = "google",messaging = FALSE)
DallasMapZoom11 <- get_map(location = bb, zoom = 11, maptype = "roadmap",source = "google",messaging = FALSE)
DallasMapZoom12 <- get_map(location = "Dallas", zoom = 12, source = "google")

saveRDS(DallasMapZoom10,file = "C:/Users/sconroy/Documents/meystingray.github.io/Shiny/Maps/DallasZoom10.RDS")
saveRDS(DallasMapZoom11,file = "C:/Users/sconroy/Documents/meystingray.github.io/Shiny/Maps/DallasZoom11.RDS")
saveRDS(DallasMapZoom12,file = "C:/Users/sconroy/Documents/meystingray.github.io/Shiny/Maps/DallasZoom12.RDS")


cda <- get_map(bb,maptype = "roadmap",source = "google")

# All Shootings since 2014
ggmap(DallasMapZoom11,extent = "device") + 
    geom_point(data = MurderWithLatLong, aes(x = Longitude, y = Latitude,fill = Year),
               shape = 21, size = 2, stroke = 1,color = "blue") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggtitle("Dallas Murders since 2014") + 
    scale_fill_manual(values = c("black", "dark gray", "blue", "purple", "green","orange","red"))



# Let's take a look at the shootings on a map for each year.
ggmap(DallasMapZoom11,extent = "normal") + 
    geom_point(data = MurderWithLatLong, aes(x = Longitude, y = Latitude), size = 1,color = "brown") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    facet_wrap(. ~ servyr) + ggtitle("Dallas Murders by Year")


 
lats <- c(min(Murders2020$Latitude),max(Murders2020$Latitude))
lons <- c(min(Murders2020$Longitude),max(Murders2020$Longitude))
bb <- make_bbox(lon=lons,lat=lats,f=0.05)
cda <- get_map(bb,zoom = 10,maptype = "roadmap",source = "google")
p <- ggmap(cda) + theme_bw() +
    geom_point(data = Murder, aes(x = Longitude, y = Latitude), 
               shape = 21, colour = "black", fill = "yellow", size = 3, stroke = 2) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
    
plot(p)

# Let's take a look at the shootings just for 2020.
suppressWarnings(
    ggmap(DallasMapZoom10,extent = "device",maprange = FALSE) +
        geom_point(data = Murders2020, aes(x = Longitude, y = Latitude), 
                   shape = 21, colour = "black", fill = "yellow", size = 3, stroke = 2)
)

# Try to do Cluster Analysis
KM <- kmeans(Murders2020[!is.na(Latitude) & !is.na(Longitude),.(Latitude,Longitude)],centers = 10)
#KM$centers

# Plot the clusters
suppressWarnings(
    ggmap(DallasMapZoom11,extent = "device") + 
        geom_point(data = MurderWithLatLong[Date > as.Date("2020-01-01"),.(Latitude,Longitude)],
                   aes(x = Longitude, y = Latitude), 
                   colour = "black", fill = "black", size = 2) + 
        geom_point(data = as.data.frame(KM$centers), aes(x = Longitude, y = Latitude), 
                   shape = 21, colour = "red", fill = "yellow", size = 30, stroke = 2,alpha = 0.3)
    
)

p <- ggmap(DallasMapZoom11,extent = "device") +
    geom_point(data = MurderWithLatLong, aes(x = Longitude, y = Latitude,group = seq_along(servyr)), 
               size = 2,shape = 21,color = "black",fill = "red",stroke = 1) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

anim <- p + transition_states(servyr,
                              transition_length = 1,
                              state_length = 2)# + ease_aes()


anim <- anim + ggtitle('Now showing {closest_state}') #,subtitle = 'Frame {frame} of {nframes}')

animate(anim,renderer = gifski_renderer(), height = 600, width = 600)