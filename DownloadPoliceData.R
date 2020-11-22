install.packages("RSocrata")
library("RSocrata")
library("gstat")
library("sp")
library("spdep")
library("rgdal")

# Police Arrests, https://www.dallasopendata.com/Public-Safety/Police-Arrests/sdr7-6v3j
PA <- read.socrata("https://www.dallasopendata.com/resource/sdr7-6v3j.csv")

# Police Active Calls, https://www.dallasopendata.com/Public-Safety/Dallas-Police-Active-Calls/9fxf-t2tr
PAC <- read.socrata("https://www.dallasopendata.com/resource/9fxf-t2tr.json")

# Police Incidents, https://www.dallasopendata.com/Public-Safety/Police-Incidents/qv6i-rri7
PI <- read.socrata("https://www.dallasopendata.com/resource/qv6i-rri7.csv")

# Police Bulk Data, https://www.dallasopendata.com/Public-Safety/Police-Bulk-Data/ftja-9jxd
PBD <- read.socrata("https://www.dallasopendata.com/resource/ftja-9jxd.csv")


save(list = ls(),file = "C:/Users/sconroy/Desktop/Debug/PoliceData.RData")
saveRDS(PA,file = "C:/Users/sconroy/Desktop/Debug/PoliceArrests.RDS")
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
