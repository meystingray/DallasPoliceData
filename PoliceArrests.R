PA <- readRDS("C:/Users/sconroy/Desktop/Debug/PoliceArrests.RDS")
library(data.table)
setDT(PA)

names(PA)

unique(PA$araction)

table(PA$drugrelated)
table(PA$drugtype)
table(PA$araction)
table(PA$clothingworn)
table(PA$occupation)
table(PA$drugrelated)
PA[,OccupationCount := .N,by = occupation]
PA[,EmployerCount := .N,by = employer]

setorder(PA,-OccupationCount)
setorder(PA,-EmployerCount)
q <- PA[,head(.SD,1),.SDcols = c("OccupationCount","drugrelated","employer"),by = occupation]
q <- PA[,head(.SD,1),.SDcols = c("EmployerCount"),by = employer]
q <- q[drugrelated == "Yes",]
plot(PA[,.N,by = ararrestdate],type = "l")
setorder(PA,ararrestdate)
lines(x = PA$ararrestdate,y = PA$SmoothedAPD,col = "red")

table(PA$tatoo)
PA$arrestyr

PA[,ArrestsPerDay := .N,by = ararrestdate] 
table(PA$ArrestsPerDay)
PA[,SmoothedAPD := predict(smooth.spline(x = ararrestdate,y = ArrestsPerDay))]
PA[ararrestdate == min(ararrestdate,na.rm = TRUE),.(ararrestdate,arrestyr)]
PA <- PA[arrestyr != 2002,]
