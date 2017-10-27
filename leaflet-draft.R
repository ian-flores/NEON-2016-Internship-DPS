library(leaflet) 
library(devtools)
install_github("ropenscilabs/nneo")
library(nneo)
library(randomcoloR)
library(rgdal)
#install.packages("jsonlite")
neon_sites <- nneo_sites()
df = data.frame(Lat=neon_sites$siteLatitude, 
                Long=neon_sites$siteLongitude)
neon_domains <- readOGR("data/spatial-data/NEON_Domains.shp", 
                        layer = "NEON_Domains", verbose=F)

tyo <- randomColor(count=22)

leaflet(neon_domains) %>%
    addPolygons(color=tyo, stroke=F, 
                fillOpacity = 0.5, 
                smoothFactor = 0.5, group = "Domains")%>%
    addProviderTiles("Esri.WorldImagery") %>%
    addMarkers(df$Long, df$Lat, popup= ~as.character(neon_sites$siteDescription), group="Stations")%>%
    # Layers control
    addLayersControl(
      overlayGroups = c("Stations", "Domains"),
      options = layersControlOptions(collapsed = FALSE))

