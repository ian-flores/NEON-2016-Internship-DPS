data <- list.files(path = "data/mamdata", full.names = T) 

data <- do.call(rbind,lapply(data,read.csv, na.strings=c("","NA", -1, 0)))

data1 <- subset(data, select = -c(nlcdClass, geodeticDatum, coordinateUncertainty, 
                                  elevationUncertainty, identificationQualifier, 
                                  identificationStatus, 
                                  reproductiveCondition, replacedTag, testes, nipples, 
                                  pregnancyStatus, vagina, bloodSampleID,
                                  bloodSampleMethod, fecalSampleID, fecalSampleCondition, 
                                  earSampleID, hairSampleID, whiskerSampleID, 
                                  voucherSampleID, measuredBy, recordedBy, remarks, 
                                  consistencyTagIDSexQF, consistencyTagIDLifeStageQF,
                                  consistencyTagIDTaxonIDQF, orderTagIDLifeStageQF, 
                                  orderTagIDRecaptureQF, fate, taxonID, plotID, uid, taxonRank))

life <- as.factor(data1$lifeStage)
life <- relevel(life, "subadult")
life <- relevel(life, "juvenile")
options(stringsAsFactors=F)
tos <- read.csv("TOSpointSpatialData.csv", header=T)
tos <- subset(tos, applicableModules=="mam")
plotID <- tos$plotID
decimalLatitude <- as.numeric(tos$decimalLatitude)
decimalLongitude <- as.numeric(tos$decimalLongitude)
trapalphanum <- tos$pointID
tos <- data.frame(cbind(trapalphanum, plotID, decimalLongitude, decimalLatitude))
data1 <- cbind(data1, life)
data <- merge(tos, data1, all=T)

tos <- subset(tos, plotID=="CPER_011")
tos$plotID <- NULL
tos$decimalLatitude <- as.numeric(tos$decimalLatitude)
tos$decimalLongitude  <- as.numeric(tos$decimalLongitude)
tos2 <- cbind(tos$decimalLongitude, tos$decimalLatitude)
tos1 <- latlong2grid(tos2)
tos1 <- tos1*1000
tos3 <- cbind(tos$trapalphanum, tos1)
colnames(tos3) <- c("Detector", "y", "x")
write.table(tos3, "TOSCPER011.txt", row.names = F, quote=F, col.names = F)




data <- subset(data, plotID=="CPER_011")
data <- subset(data, scientificName =="Perognathus flavus")

library(lubridate)
data$year <- as.character(year(as.Date(data$date, "%m/%d/%Y")))

data <- subset(data, plotID == "CPER_011")
data <- subset(data, year == "2014")

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

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
write.table(data, "DATACPER011.txt", row.names=F, quote=F, col.names = F)


library("secr")
CPER <- read.capthist("DATACPER011.txt", "TOSCPER011.txt", fmt = "trapID", detector = "multi")
CPER[[2]] <- NULL
summary(CPER)
plot(CPER)
CPER

v <- secr.fit(CPER[[6]], model = list(D~1, g0~1, sigma~1), buffer=100)
v
