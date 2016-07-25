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
tos <- cbind(trapalphanum, plotID, decimalLongitude, decimalLatitude)
data1 <- cbind(data1, life)
data <- merge(tos, data1, all=T)

tos <- subset(tos, plotID=="CPER_011")
tos$plotID <- NULL
tos$decimalLatitude <- as.numeric(tos$decimalLatitude)
tos$decimalLongitude  <- as.numeric(tos$decimalLongitude)
tos <- data.frame(cbind(trapalphanum, decimalLongitude, decimalLatitude))
colnames(tos) <- c("Detector", "y", "x")

write.table(tos, "TOSCPER011.txt", row.names = F, quote=F, col.names = F)


data <- subset(data, plotID=="CPER_011")
data <- subset(data, scientificName =="Perognathus flavus")

data$date<- (as.Date(data$date, "%m/%d/%Y"))

Session <- event.num
ID <- data$individualID
Ocassion <- date.num
Detector <- data$trapalphanum

data <- cbind(Session, ID, Ocassion, Detector)
data <- data[complete.cases(data),]
write.table(data, "DATACPER011.txt", row.names=F, quote=F, col.names = F)


library("secr")
CPER <- read.capthist("DATACPER011.txt", "TOSCPER011.txt", fmt = "trapID")

plot(CPER)

v <- secr.fit(CPER, model = list(D ~ session))

