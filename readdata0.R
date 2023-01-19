#LOAD relevant libraries
library(tidyverse)


#sf reads shapefiles into R

library(sf)

#library(ggmap)


library(leaflet)
library(ggplot2)
library(ggthemes)
library(ggsn)
library(geojsonio)
library(tmap)


#load ea shapefile

ea <- st_read("C:\\Users\\Godwi\\Desktop\\Murithi\\2023\\Geopsy\\Projects\\Aceli\\Data\\Shapefiles\\ea\\ea.shp")
iw <- st_read("C:\\Users\\Godwi\\Desktop\\Murithi\\2023\\Geopsy\\Projects\\Aceli\\Data\\Shapefiles\\ea\\iw.shp")


#Load the shapefiles  the workspace

Ug <- st_read("C:\\Users\\Godwi\\Desktop\\Murithi\\2023\\Geopsy\\Projects\\Aceli\\Data\\Shapefiles\\geoBoundaries-UGA-ADM0-all\\geoBoundaries-UGA-ADM0.shp")
KE <- st_read("C:\\Users\\Godwi\\Desktop\\Murithi\\2023\\Geopsy\\Projects\\Aceli\\Data\\Shapefiles\\ken_adm_iebc_20191031_shp\\ken_admbnda_adm0_iebc_20191031.shp")
RW <- st_read("C:\\Users\\Godwi\\Desktop\\Murithi\\2023\\Geopsy\\Projects\\Aceli\\Data\\Shapefiles\\rwa_adm_2006_nisr_wgs1984_20181002_shp\\rwa_adm0_2006_NISR_WGS1984_20181002.shp")
TZ <- st_read("C:\\Users\\Godwi\\Desktop\\Murithi\\2023\\Geopsy\\Projects\\Aceli\\Data\\Shapefiles\\tza_admbnda_adm0_20181019\\tza_admbnda_adm0_20181019.shp")

#read the data


data <- read_csv("C:\\Users\\Godwi\\Desktop\\Murithi\\2023\\Geopsy\\Projects\\Aceli\\Data\\FA_SMEs.csv")
data2 <- read_csv("C:\\Users\\Godwi\\Desktop\\Murithi\\2023\\Geopsy\\Projects\\Aceli\\Data\\TA_SMEs.csv")

#convert the data to a dataframe

addr <- as.data.frame(data)

addr0 <- as.data.frame(data2)


#Convert dataframe to sf object


fasmes <- st_as_sf(addr, coords = c("Longitude", "Latitude"))
tasmes <- st_as_sf(addr0, coords= c("Longitude", "Latitude"))

#Assign a projection to the sf object


fasmes0 <- st_set_crs(fasmes, 4326)
tasmes0 <- st_set_crs(tasmes, 4326)
Ug0 <- st_set_crs(Ug, 4326)
KE0 <- st_set_crs(KE, 4326)
TZ0 <- st_set_crs(TZ, 4326)
RW0 <- st_set_crs(RW, 4326)
ea0 <- st_set_crs(ea, 4326)
iw0 <- st_set_crs(iw, 4326)

#plot the shapefile
ggplot() +
  geom_sf(data = ea0, size = 0.01, aes(fill="#cccccc"), color = "white")+
  geom_sf(data =  Ug0, size = 0.01, aes(fill = "transparent"), color = "black")+
  geom_sf(data =  KE0, size = 0.01, aes(fill = "transparent"), color = "black")+
  geom_sf(data =  TZ0, size = 0.01, aes(fill = "transparent"), color = "black")+
  geom_sf(data =  RW0, size = 0.01, aes(fill = "transparent"), color = "black")+
  geom_sf(data = iw0, size = 0.01, aes(fill = "#1ca3ce"), color = "#1ca3ce") +
  geom_sf(data = fasmes0, size = 1.2, aes(color = "black"), shape = 21, stroke = 1, fill = "red") +
  geom_sf(data = tasmes0, size = 1.2, aes(color = "black"), shape = 21, stroke = 1, fill = "green") +
  scale_fill_identity(name = '', guide = 'legend',labels = c("Water Bodies", "Country boundary", "District boundary")) +
  scale_colour_manual(name = 'Legend', 
                      values =c('red'='red','green'='green'), labels = c('SMEs receiving Loans','SMEs receiving TA'))+
  scalebar(data = ea0, dist = 200, dist_unit = "km", st.size = 3, location = "bottomleft", model = "WGS84", transform = TRUE)+
  north(ea0, symbol = 1, location = "topleft", anchor = NULL)+
  theme(labels = element_blank())+

  geom_sf_label(data = KE0, label = "Kenya",size = 3, box.padding = 0) +
  geom_sf_label(data =  Ug0, label = "Uganda",size = 3) +
  geom_sf_label(data = RW0, label = "Rwanda",size = 3) +
  geom_sf_label(data = TZ0, label = "Tanzania",size = 3) +
  
  theme_light()+
  theme(legend.position = "left")+


  ggtitle("Distribution of SMEs receiving ACELI supported Loans")+
  coord_sf()

#Plots the data using leaflets to check
m <- leaflet(addr) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng=addr$Longitude, lat=addr$Latitude, clusterOptions = markerClusterOptions())
m  # Print the map
