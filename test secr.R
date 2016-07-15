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
tos <- data.frame(cbind(trapalphanum, plotID,decimalLongitude,decimalLatitude))
data1 <- cbind(data1, life)
data <- merge(tos, data1, all=T)

tos <- subset(tos, plotID=="CPER_011")
tos$plotID <- NULL
tos$decimalLatitude <- as.numeric(tos$decimalLatitude)
tos$decimalLongitude  <- as.numeric(tos$decimalLongitude)
colnames(tos) <- c("Detector", "y", "x")

write.table(tos, "TOSCPER011.txt", row.names = F, quote=F, col.names = F)
trapss <- read.traps(file = "./TOSCPER011.txt", detector="multi")


data <- subset(data, plotID=="CPER_011")
data <- subset(data, scientificName =="Perognathus flavus")

data$date<- (as.Date(data$date, "%m/%d/%Y"))

Session <- data$eventID
ID <- data$individualID
Ocassion <- data$date
Detector <- data$trapalphanum

data <- cbind(Session, ID, Ocassion, Detector)
data <- data[complete.cases(data),]
write.table(data, "DATACPER011.txt", row.names=F, quote=F, col.names = F)


library("secr")
CPER <- make.capthist(captures =   "DATACPER011.txt", traps  = "TOSCPER011.txt", fmt = "trapID")

plot(CPER)

v <- secr.fit(CPER, model = list(D ~ session, g0~session, sigma ~ 1), buffer=20000)

