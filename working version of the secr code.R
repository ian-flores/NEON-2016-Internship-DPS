


completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

mark <- function(plot="", name="", year=""){
tos <- subset(tos, plotID==plot)
tos$plotID <- NULL
tos$decimalLatitude <- as.numeric(tos$decimalLatitude)
tos$decimalLongitude  <- as.numeric(tos$decimalLongitude)
tos2 <- cbind(tos$decimalLongitude, tos$decimalLatitude)
tos1 <- latlong2grid(tos2)
tos1 <- tos1*1000
tos3 <- cbind(tos$trapalphanum, tos1)
colnames(tos3) <- c("Detector", "y", "x")
write.table(tos3, "TOSC.txt", row.names = F, quote=F, col.names = F)

data <- subset(data, plotID==plot)
data <- subset(data, scientificName == name)
data$year <- as.character(year(as.Date(data$date)))#, "%m/%d/%Y")))
data <- subset(data, year == year)
data<- completeFun(data = data, desiredCols = "date")

event.num <- factor(data$eventID, labels=1:length(levels(as.factor(data$eventID))))
event.num <- as.numeric(event.num)
event.num
data$eventID <- factor(data$eventID, labels=1:length(levels(as.factor(data$eventID))))
data$eventID <- as.numeric(data$eventID)
date.num <- numeric(length(event.num))

for(i in event.num) {
  date.sub <- data$date[which(event.num == i)]
  date.num[which(event.num == i)] <- factor(date.sub, labels=1:length(levels(as.factor(date.sub))))
}

Session <- event.num
ID <- data$individualID
Ocassion <- date.num
Detector <- data$trapalphanum

data <- cbind(Session, ID, Ocassion, Detector)
data <- data[complete.cases(data),]
write.table(data, "DATA.txt", row.names=F, quote=F, col.names = F)

STER <- read.capthist("DATA.txt", "TOSC.txt", fmt = "trapID", detector = "multi")

tuti <- NULL
for (i in 1:length(STER)) {
  tuti[i]<- summary(STER[[i]])$counts$Total[1]
  rtu <- which.max(tuti)
}
v <- secr.fit(STER[[rtu]], model=list(D~1, g0~1, sigma~1), buffer=100)
plot(v)
a <- predict(v)
return(a)

}