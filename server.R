#Server 
 
#check to see if libraries need to be installed
libs=c("shiny","DT","dplyr","ggplot2","readr","reshape2","RColorBrewer",
       "rgdal","gridExtra","sp", "lubridate", "spatstat", "SpatialEpi", "RSelenium", "maps")
x=sapply(libs,function(x)if(
  !require(x,character.only = T)) 
  install.packages(x));rm(x,libs)
library("ggmap")

options(shiny.fullstacktrace = TRUE)

# This creates the dataset, all clean, without the unneeded variables & 
# extracts the spatial information in each file. 
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
                                 orderTagIDRecaptureQF, fate, taxonID, trapCoordinate,
                                 plotID, uid, taxonRank))

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
data$year <- NULL
data$year <- year(as.Date(data$date, "%m/%d/%Y"))


shinyServer(
  function(input, output, session) {
    observe({
      test <<- paste0("http://www.neonscience.org/science-design/field-sites")
    })
    #iframe for a map of neon sites
    output$neonmap <- renderUI({
      my_map <- tags$iframe(src=test, 
                            height=850, 
                            width=1850)
      print(my_map)
      my_map
    })
    
    #function to calculate the sample sizes in each of the boxplots
    fun_length <- function(x){
      return(data.frame(y=median(x),label= paste0("N=", length(x))))
    }

    
    plotss <- reactive({
      x <- switch (input$indvar, "Weight" = data$weight, "Total Length" = data$totalLength,
                   "Hindfoot Length" = data$hindfootLength, "Ear Length" = data$earLength,
                   "Tail Length" = data$tailLength,
                   "Sex" = data$sex, "Life Stage" = data$life, "Site" = data$siteID
      )
        #input$indvar,
         #          input$indvar <- ind()
          #           )
                   
      y <- switch(input$depvar,
                  "Weight" = data$weight, "Total Length" = data$totalLength,
                  "Hindfoot Length" = data$hindfootLength, "Ear Length" = data$earLength,
                  "Tail Length" = data$tailLength)
        
      z <- switch(input$thirdvar, 
                  "None" =  NULL, 
                  "Sex" = data$sex, 
                  "NEON Domain" = data$domainID,
                  "Field Site" = data$siteID, 
                  "Species" = data$scientificName, 
                  "Life Stage" = data$life, 
                  "Year" = as.factor(year(as.Date(data$date, "%m/%d/%Y"))))
       
      if(input$thirdvar == "None"){
        if(class(x) == "numeric"){
          p <- ggplot(aes(x=x, y=y), data=data) + geom_point() + 
            geom_smooth(method = "lm") + 
            xlab(input$indvar) + ylab(input$depvar) + theme_bw() + 
            ggtitle(paste("Relationship between", input$indvar, "and", input$depvar))
        } else{if(class(x) == "character"){
          p <- ggplot(aes(x=x, y=y), data=data) + geom_boxplot()+ 
            xlab(input$indvar) + ylab(input$depvar)+ theme_bw()+ 
            ggtitle(paste("Relationship between", input$indvar, "and", input$depvar))+
            stat_summary(fun.data = fun_length, geom="text", vjust = +1, 
                         size=4, position = position_dodge(0.9))
        }  else{if(class(x) == "factor"){
          p <- ggplot(aes(x=x, y=y), data=data) + geom_boxplot()+ 
            xlab(input$indvar) + ylab(input$depvar)+ theme_bw()+ 
            ggtitle(paste("Relationship between", input$indvar, "and", input$depvar))+
            stat_summary(fun.data = fun_length, geom="text", vjust = +1, 
                         size=4, position = position_dodge(0.9))
        } } }
      } else{ 
        if(class(x) == "numeric"){
          p <- ggplot(aes(x=x, y=y, colour=z), data=data) + geom_point() + 
            geom_smooth(method = "lm") + 
            xlab(input$indvar) + ylab(input$depvar) + theme_bw() + 
            ggtitle(paste("Relationship between", input$indvar, "and", input$depvar))
        } else{if(class(x) == "character"){
          p <- ggplot(aes(x=x, y=y, colour=z), data=data) + geom_boxplot()+ 
            xlab(input$indvar) + ylab(input$depvar)+ theme_bw()+ 
            ggtitle(paste("Relationship between", input$indvar, "and", input$depvar))+
            stat_summary(fun.data = fun_length, geom="text", vjust = +1, 
                         size=4, position = position_dodge(0.9))
        }  else{if(class(x) == "factor"){
          p <- ggplot(aes(x=x, y=y, colour=z), data=data) + geom_boxplot()+ 
            xlab(input$indvar) + ylab(input$depvar)+ theme_bw()+ 
            ggtitle(paste("Relationship between", input$indvar, "and", input$depvar))+
            stat_summary(fun.data = fun_length, geom="text", vjust = +1, 
                         size=4, position = position_dodge(0.9))
        } } }
      }
      
      
    })
    
    output$scatterPlot <- renderPlot({
      print(plotss())
    })
    
    ###### Data Table ######
    
    #Display a Data Table with all the data loaded into the application
    output$mammals <- renderDataTable({
      datatable(data, options = list(
        searching=T, pageLength = 10, lengthMenu =c(5,10,15,20,25,50,100))
        #, 
        #colnames = c("Latitude", "Longitude", "Plot ID", "Trap", "Domain", "Site", 
         #            "Elevation", "Capture Date", "EventID","Individual ID","Scientific Name",
          #           "Sex", "Recapture", "HF Length", 
           #          "Ear Length","Tail Length", "Total Length", "Weight", "Life Stage")
    )})

    
    
    #######   CONTOUR MAP  #######
    
    # Display all the NEON Domains and displays them as options in a dropdown menu
    output$domains <- renderUI({
      selectInput("domainss", "Domain", 
                  c(as.character(data$domainID), "pick one"), selected = "pick one")
    })
    
    # Display all the NEON sites, that are located inside the prievously choosen domain, 
    # and displays them in a dropdown menu
    output$sites <- renderUI({
      options(StringsAsFactors=T)
      if(is.null(input$domainss) || input$domainss == "pick one"){return()
      } else selectInput("sitess", "Field Site", 
                         c(as.character(data$siteID[which(data$domainID == input$domainss)]), "pick one"), 
                         selected= "pick one")
    }) 
    
    # Displays the different mammal grids, that are located in the previously choosen site, 
    # and displays them in a dropdown menu
    output$plotID <- renderUI({
      options(StringsAsFactors=T)
      if (is.null(input$sitess) || input$sitess == "pick one"){return() 
      } else selectInput("plots", "Plot ID", 
                         c(data$plotID[which(data$siteID == input$sitess)], "pick one"),
                         selected = "pick one")
    })
    
    # Displays all the species in the choosen mammal grid.
    output$species <- renderUI({
      options(stringsAsFactors=T)
      if (is.null(input$plots) || input$plots == "pick one"){return() 
      } else selectizeInput("species", "Species", 
                            c(as.character(data$scientificName[which(data$plotID == input$plots)])), 
                            multiple = F, options = list(maxItems=1))
    })
    
    # The function for the contour map, so it calculates the contour, and looks for the backgorund 
    # map either in the folders inside the app, or in Google Maps, through Google's API
    
    mapas <- function(name = " ", plot = "CPER_011"){
      
      data <- subset(data, scientificName %in% (name))
      data <- subset(data, plotID == plot)
      tos <- subset(tos, plotID == plot)
      lati <- as.numeric(tos$decimalLatitude)
      long <- as.numeric(tos$decimalLongitude)
      tos <- cbind(tos, lati, long)
      options(stringsAsFactors=T)
      lat <- as.numeric(data$decimalLatitude)
      lng <- as.numeric(data$decimalLongitude)
      data <- cbind(data, lat, lng)
      mlat <- mean(as.numeric(tos$decimalLatitude))
      mlng <- mean(as.numeric(tos$decimalLongitude))
      data <- melt(data)
      data <- cbind(lat,lng, data)
      f <- paste0("./maps/", plot, ".rda")
      if (file.exists(f)){
        load(f)
        p <- ggmap(dropit) + 
          stat_density2d(data= data,
                         aes(x=lng, y=lat, alpha = 0.75, fill=(..level..)),
                         bins=10, geom="polygon", show.legend=F) +
          geom_point(data=tos, aes(x=long, y=lati), color='green') +
          xlab("Longitude") + ylab("Latitude")+
          ggtitle(paste(name)) +  
          scale_fill_gradient(low = "blue", high = "red") + theme(legend.position="none")
      } else {
        dropit <- get_map(location = c(lon = mlng, lat= mlat),
                          maptype="satellite", zoom=19, force = F)
        save(dropit, file = f)
        p <- ggmap(dropit) + 
          stat_density2d(data= data,
                         aes(x=lng, y=lat, alpha = 0.75, fill=(..level..)),
                         bins=10, geom="polygon", show.legend=F) +
          geom_point(data=tos, aes(x=long, y=lati), color='green') +
          xlab("Longitude") + ylab("Latitude")+
          ggtitle(paste(name)) +  
          scale_fill_gradient(low = "blue", high = "red") + theme(legend.position="none")}
      
      return(p)
      
    }
    
    # Displays the map, based on the user choices
    mapss <- reactive({
      # Substitutes the error message that would display because of the lack of options made by the user
      validate(
        need(input$species != "", "Please complete all the selections")
      )
      h <- mapas(name = input$species, plot = input$plots)
      return(h)
    })
    
    
    # Eliminates annoying messages
    options(shiny.deprecation.messages=FALSE)
    
    #Renders the contour to the application
    output$mymaps1 <- renderPlot({
      mapss()
    })
    
    ##### K est ####
    
    # A function that compiles and formats the data, to be able
    #to calculate the Ripley's K for the choosen species 
    ripley <- function(name="", plot=""){
      data <- subset(data, plotID == plot)
      data <- subset(data, scientificName == name)
      lat <- as.numeric(data$decimalLatitude)
      lng <- as.numeric(data$decimalLongitude)
      data <- cbind(lng, lat)
      
      # Converts the coordinates, into relative distances in a two dimensional space
      data <- latlong2grid(data)
      # We multiply the relative distances, to get them to the actual scale of the plots 
      data <- data*1000
      data <- unique(data)
      tos <- subset(tos, plotID == plot)
      lati <- as.numeric(tos$decimalLatitude)
      long <- as.numeric(tos$decimalLongitude)
      tos <- cbind(long, lati)
      tos <- latlong2grid(tos)
      tos <- tos*1000
      x.range <- range(tos$x)
      y.range <- range(tos$y)
      
      #Converts all the spatial data into a Point-Pattern Planar (PPP) Data Frame 
      data.ppp <- ppp(x= data$x, y= data$y, x.range, y.range) 
      c <- plot(envelope(data.ppp, Kest), main=name, 
                ylab="K(r)", xlab="Radius (m)")
      return(c)
    }
    
    
    spat <- reactive({
      validate(
        need(input$species != "", "Please complete all the selections")
      )
      l <- ripley(name = input$species, plot=input$plots)
      return(l)
    })
    output$kest <- renderPlot({
      spat()
    })
    #####MODEL#####
    # completeFun <- function(data, desiredCols) {
    #   completeVec <- complete.cases(data[, desiredCols])
    #   return(data[completeVec, ])
    # }
    # 
    # mark <- function(plot="", name=""){
    #   tos <- subset(tos, plotID == plot)
    #   tos$plotID <- NULL
    #   tos$decimalLatitude <- as.numeric(tos$decimalLatitude)
    #   tos$decimalLongitude  <- as.numeric(tos$decimalLongitude)
    #   tos2 <- cbind(tos$decimalLongitude, tos$decimalLatitude)
    #   tos1 <- latlong2grid(tos2)
    #   tos1 <- tos1*1000
    #   tos3 <- cbind(tos$trapalphanum, tos1)
    #   colnames(tos3) <- c("Detector", "y", "x")
    #   write.table(tos3, "TOSC.txt", row.names = F, quote=F, col.names = F)
    #   
    #   data <- subset(data, plotID == plot)
    #   data <- subset(data, scientificName == name)
    #   #data$year <- as.character(year(as.Date(data$date)))#, "%m/%d/%Y")))
    #   #data <- subset(data, year == '2015')
    #   data<- completeFun(data = data, desiredCols = "date")
    #   
    #   event.num <- factor(data$eventID, labels=1:length(levels(as.factor(data$eventID))))
    #   event.num <- as.numeric(event.num)
    #   event.num
    #   data$eventID <- factor(data$eventID, labels=1:length(levels(as.factor(data$eventID))))
    #   data$eventID <- as.numeric(data$eventID)
    #   date.num <- numeric(length(event.num))
    #   
    #   for(i in event.num) {
    #     date.sub <- data$date[which(event.num == i)]
    #     date.num[which(event.num == i)] <- factor(date.sub, labels=1:length(levels(as.factor(date.sub))))
    #   }
    #   
    #   Session <- event.num
    #   ID <- data$individualID
    #   Ocassion <- date.num
    #   Detector <- data$trapalphanum
    #   
    #   data <- cbind(Session, ID, Ocassion, Detector)
    #   data <- data[complete.cases(data),]
    #   write.table(data, "DATA.txt", row.names=F, quote=F, col.names = F)
    #   
    #   STER <- read.capthist("DATA.txt", "TOSC.txt", fmt = "trapID", detector = "multi")
    #   
    #   tuti <- NULL
    #   for (i in 1:length(STER)) {
    #     tuti[i] <- summary(STER[[i]])$counts$Total[1]
    #     rtu <- which.max(tuti)
    #   }
    #   v <- secr.fit(STER[[rtu]], model = list(D~1, g0~1, sigma~1), buffer=100)
    #   plot(v)
    #   a <- predict(v)
    #   return(a)
    # }
    # 
    # capture <- reactive({
    #   xolo <- mark(plot = input$plots , name = input$species)
    #   return(xolo)
    # })
    # output$mrc1 <- renderPlot({
    #   capture()
    # })
    #### Animation ####    
    
  })