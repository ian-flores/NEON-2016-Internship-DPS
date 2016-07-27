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
                                  orderTagIDRecaptureQF, fate, taxonID, trapCoordinate,plotID, uid, taxonRank))

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
tos <- data.frame(cbind(plotID,decimalLatitude,decimalLongitude, trapalphanum))
data1 <- cbind(data1, life)
data <- merge(tos, data1, all=T)
data1 <- NULL
data$lifeStage <- NULL

library(lubridate)
data$year <- as.character(year(as.Date(data$date, "%m/%d/%Y")))

data <- subset(data, plotID == "CPER_011")
data <- subset(data, year== "2014")

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

data<- completeFun(data = data, desiredCols = "date")


event.num <- factor(data$eventID, labels=1:length(levels(as.factor(data$eventID))))
event.num <- as.numeric(event.num)
event.num

for(i in levels(as.factor(data$eventID))) {
  date.sub <- data$date[which(data$eventID== i)]
  date.num <- factor(date.sub, labels=1:length(levels(as.factor(date.sub))))
}

data$eventID <- factor(data$eventID, labels=1:length(levels(as.factor(data$eventID))))
data$eventID <- as.numeric(data$eventID)
date.num <- numeric(length(event.num))

for(i in event.num) {
  date.sub <- data$date[which(event.num == i)]
  date.num[which(event.num == i)] <- factor(date.sub, labels=1:length(levels(as.factor(date.sub))))
  }

jugo <- data %>% group_by(eventID, date) %>% summarise()
