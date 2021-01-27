install.packages("RSocrata")
install.packages("gstat")
library("RSocrata")
library("gstat")
library("sp")
library("spdep")
library("rgdal")
library(data.table)

AllData <- RSocrata::ls.socrata("https://www.dallasopendata.com")
AllData <- AllData[,c("title","landingPage","modified","identifier","description","keyword","theme")]
setDT(AllData)
q <- AllData[grepl("Police",title),]


PoliceStations <- read.socrata(AllData[title == "Dallas Police Stations",identifier])

# Police Arrests, https://www.dallasopendata.com/Public-Safety/Police-Arrests/sdr7-6v3j
PA <- read.socrata("https://www.dallasopendata.com/resource/sdr7-6v3j.csv")

# Police Active Calls, https://www.dallasopendata.com/Public-Safety/Dallas-Police-Active-Calls/9fxf-t2tr
PAC <- read.socrata("https://www.dallasopendata.com/resource/9fxf-t2tr.json")

# Police Incidents, https://www.dallasopendata.com/Public-Safety/Police-Incidents/qv6i-rri7
PI <- read.socrata(AllData[title == "Police Incidents",identifier])

# Police Bulk Data, https://www.dallasopendata.com/Public-Safety/Police-Bulk-Data/ftja-9jxd
PBD <- read.socrata("https://www.dallasopendata.com/resource/ftja-9jxd.csv")


load("C:/Users/sconroy/Desktop/Debug/PoliceData.RData")

saveRDS(PA,file = "C:/Users/sconroy/Desktop/Debug/PoliceArrests.RDS")
saveRDS(PI,file = "C:/Users/sconroy/Desktop/Debug/PoliceIncidents.RDS")

nrow(PA)
nrow(PI)
nrow(PAC)
nrow(PBD)


names(PA)
names(PI)
plot(PI$x_coordinate,PI$y_cordinate)
names(PAC)
names(PBD)

setDT(PI)
ret <- c("LONG","LAT")
PI[!is.na(x_coordinate) & !is.na(y_cordinate),
   (ret) := convertXYtoLatLong(.SD),.SDcols = c("x_coordinate","y_cordinate")]

PI[,.(LONG,LAT)]
convertXYtoLatLong <- function(XYpoints,zone = 14) {
    
    #save(list = ls(),file = "D:/ConvertXYtoLatLong.RData")
    
    names(XYpoints) <- c("X","Y")
    coordinates(XYpoints) <- ~ X + Y
    sputm <- SpatialPoints(XYpoints, proj4string = CRS(paste0("+proj=utm +zone=", zone, "+datum=WGS84 +units=us-ft")))
    spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
    spgeo <- as.data.table(spgeo)
    setnames(spgeo,old = c("X","Y"),new = c("LONGITUDE","LATITUDE"))
    return(spgeo)
    
}

PoliceStations$location
setDT(PoliceStations)
PoliceStations[,rowid := 1:.N]
PoliceStations[,LatLongStart := regexpr("(",location,fixed = TRUE)[1] + 1,by = rowid]
PoliceStations[,LatLongEnd := regexpr(")",location,fixed = TRUE)[1] - 1,by = rowid]
PoliceStations[,LatLong := substr(location,start = LatLongStart,stop = LatLongEnd)]
PoliceStations[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
PoliceStations[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
PoliceStations[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
PoliceStations[,Longitude := as.numeric(Longitude)]
PoliceStations[,Latitude := as.numeric(Latitude)]
PoliceStations[,c("LatLongStart","LatLongEnd","LatLong","LatLongComma") := NULL]
PoliceStations$rowid <- NULL

saveRDS(PoliceStations,"PoliceStations.RDS")
getwd()
