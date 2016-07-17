#Server 

library(DT)
library(dplyr)
library(readr)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(rgdal)
library(gridExtra)
library(sp)
library(reshape2)
library(lubridate)
library(plotly)
library(animation)

options(shiny.fullstacktrace = TRUE)

                                                #### Mammals ####
data <- list.files(path = "data/mamdata", full.names = T) 

data <- do.call(rbind,lapply(data,read.csv, na.strings=c("","NA", -1, 0)))

data1 <- subset(data, select = -c(nlcdClass, geodeticDatum, coordinateUncertainty, 
                                 elevationUncertainty, individualID, identificationQualifier, 
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
tos <- data.frame(cbind(plotID,decimalLatitude,decimalLongitude, trapalphanum))
data1 <- cbind(data1, life)
data <- merge(tos, data1, all=T)

shinyServer(
  function(input, output) {
    observe({
      test <<- paste0("http://www.neonscience.org/science-design/field-sites")
    })
    output$neonmap <- renderUI({
      my_map <- tags$iframe(src=test, 
                            height=850, 
                            width=1850)
      print(my_map)
      my_map
    })
    
    fun_length <- function(x){
      return(data.frame(y=median(x),label= paste0("N=", length(x))))
    }
    plotss <- reactive({
      x <- switch (input$indvar, 
                   "Weight" = data$weight, "Total Length" = data$totalLength, 
                   "Hindfoot Length" = data$hindfootLength, "Ear Length" = data$earLength, 
                   "Tail Length" = data$tailLength, 
                   "Sex" = data$sex, "Life Stage" = data$life, "Site" = data$siteID)
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
    
    output$mammals <- renderDataTable({
      datatable(data, options = list(
        searching=T, pageLength = 10, lengthMenu =c(5,10,15,20,25,50,100)), 
        colnames = c("Individual", "Latitude", "Longitude", "Plot ID", "Trap", "Domain", "Site", 
                     "Trap", 
                     "Elevation", 
                     "Capture Date", "Scientific Name", "Sex", "Recapture", "Life Stage", 
                     "HF Length", 
                     "Ear Length","Tail Length", "Total Length", "Weight")
        )})
    
    
    #######   CONTOUR MAP  #######
    
    output$domains <- renderUI({
      selectInput("domainss", "Domain", 
                  c(as.character(data$domainID), "pick one"), selected = "pick one")
    })
    
    output$sites <- renderUI({
      options(StringsAsFactors=T)
      if(is.null(input$domainss) || input$domainss == "pick one"){return()
      } else selectInput("sitess", "Field Site", 
                     c(as.character(data$siteID[which(data$domainID == input$domainss)]), "pick one"), 
                     selected= "pick one")
    }) 
 
    output$plotID <- renderUI({
      options(StringsAsFactors=T)
      if (is.null(input$sitess) || input$sitess == "pick one"){return() 
         } else selectInput("plots", "Plot ID", 
                     c(data$plotID[which(data$siteID == input$sitess)], "pick one"),
                     selected = "pick one")
    })

    output$species <- renderUI({
      options(stringsAsFactors=T)
      if (is.null(input$plots) || input$plots == "pick one"){return() 
      } else selectizeInput("species", "Species", 
                            c(as.character(data$scientificName[which(data$plotID == input$plots)])), 
                            multiple = T, options = list(maxItems=3))
    })
    
    output$traps <- renderUI({
      if (is.null(input$species) || input$species == "pick one"){return()
        } else radioButtons("traps",
                            "Decide if to show all traps or just where animals where caught",
                            c("All Traps" = "all", "Only Caught" = "caught"))
    })
    
    # output$slid <- renderUI({
    #   if (is.null(input$species) || input$species == "pick one"){return()
    #   } else sliderInput("slide", 
    #                      "Date Animation", 
    #                      min = min(data$date), max= max(data$date))
    # })
      
    filename <- reactive({
      paste0("./maps/", input$plotID)
    })
    
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
                         aes(x=lng, y=lat, alpha = 0.75, fill=..level..),
                         bins=10, geom="polygon", show.legend=F) +
          geom_point(data=tos, aes(x=long, y=lati), color='green') +
          xlab("Longitude") + ylab("Latitude")+
          ggtitle(paste(name)) +  
          scale_fill_gradient(low = "blue", high = "red")
         } else {
      dropit <- get_map(location = c(lon = mlng, lat= mlat),
                        maptype="satellite", zoom=19, force = F)
      save(dropit, file = f)
      p <- ggmap(dropit) + 
        stat_density2d(data= data,
                       aes(x=lng, y=lat, alpha = 0.75, fill=..level..),
                       bins=10, geom="polygon", show.legend=F) +
        geom_point(data=tos, aes(x=long, y=lati), color='green') +
        xlab("Longitude") + ylab("Latitude")+
        ggtitle(paste(name)) +  
        scale_fill_gradient(low = "blue", high = "red")}
      
      return(p)
      
    }
    
    mapss <- reactive({
      h <- mapas(name = input$species, plot = input$plots)
      return(h)
    })
    
    
    options(shiny.deprecation.messages=FALSE)
    
    output$mymaps1 <- renderPlot({
      mapss()
    })
    
    output$mymaps2 <- renderPlot({
      mapss()
    })
    
    output$mymaps3 <- renderPlot({
      mapss()
    })
  
#### Animation ####    
    
    })