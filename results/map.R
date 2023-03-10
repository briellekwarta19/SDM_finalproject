#Load libraries:
library(tidyverse)
library(raster) 
library(terra) 
library(spData) 
library(readr) 
library(readxl) 
library(tmap)
library(tmaptools)
library(sf)
library(leaflet)
library(maps)
library(spData)
library(magick)
library(grid)
library(viridis)
library(RColorBrewer)

#loading shp files and plotting
columbia_sf <- read_sf(here::here("spatialdata", "Columbia50.shp"))
colnames(columbia_sf )

plot(columbia_sf[3], max.plot = 10) #plotting temperature data


#### Leaflet map by segment ####
#transform data to different coordinate system
columbia_sf <- st_transform(columbia_sf, "WGS84")

#A map of a subset of the JDR noting the river fork for each segment
leaflet() %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%  
  #addProviderTiles(providers$CartoDB.Positron) %>% # Add a nice, neutral background
  addPolygons(data = columbia_sf , # Add polygons 
              weight = 2, # Increase polygon outline
              opacity = 1, # Make the polygon outline not see-through at all  
              dashArray = "3", # Make the polygon outlines dashed 
              fillOpacity = 0.7, # Make the polygons somewhat see-through
              highlightOptions = highlightOptions( # Defines pop-up labels  
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE), 
              label = paste("<strong>Segment</strong>: ", # Make this part of the label bolded
                            columbia_sf $Segment) %>% 
                lapply(htmltools::HTML), # We need this bit to properly apply the code creating bold words and line breaks
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) 




#### Alt 1 ####
#1 = Downstream, 10 observations
#2 = Downstream, 5 observations
#3 = Downstream, No observations
#4 = Random, 10 observations 
#5 = Random, 5 observations
#6 = Random, No observations

file_name <- here::here("results", "map_true_finalstates.csv")
true_finalstates_mapdat <- read.csv(file_name)[-1]

true_finalstates_mapdat1 <- true_finalstates_mapdat %>% filter(dr == "down_10obs")
true_finalstates_mapdat1 <- true_finalstates_mapdat1  %>% arrange(desc(segment))

columbia_states_sf_1 <- cbind(columbia_sf, true_finalstates_mapdat1$dr, true_finalstates_mapdat1$state)
colnames(columbia_states_sf_1)[4:5] <- c("dr", "State")

#pal <- colorBin("YlOrRd", domain = columbia_states_sf_1$State, bins = 6)

# leaflet() %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>% 
#   #addProviderTiles(provider = "CartoDB.DarkMatterNoLabels") %>% # Add a nice, neutral background
#   #addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
#   addPolygons(data = columbia_states_sf_1, # Add polygons 
#               color = ~pal(columbia_states_sf_1$State), # Color the segment polygons
#               weight = 5, # Increase polygon outline
#               opacity = 1, # Make the polygon outline not see-through at all  
#               dashArray = "3", # Make the polygon outlines dashed 
#               fillOpacity = 0.7, # Make the polygons somewhat see-through
#               highlightOptions = highlightOptions( # Defines pop-up labels  
#                 weight = 5,
#                 color = "#666",
#                 dashArray = "",
#                 fillOpacity = 4,#0.7,
#                 bringToFront = TRUE), 
#               label = paste("<strong>Segment</strong>: ", # Make this part of the label bolded
#                             columbia_sf $Segment, "<br>",
#                             "<strong>Segment</strong>: ", # Make this part of the label bolded
#                             columbia_sf $State) %>% 
#                 lapply(htmltools::HTML), # We need this bit to properly apply the code creating bold words and line breaks
#               labelOptions = labelOptions(
#                 style = list("font-weight" = "normal", padding = "3px 8px"),
#                 textsize = "15px",
#                 direction = "auto")) %>% 
#   addLegend(data = columbia_states_sf_1,
#             pal = pal,
#             values = ~State, 
#             opacity = 0.7, 
#             title = "Final Abundance",
#             position = "bottomright")

color_val <- c("blue", "yellow", "orange" , "#D73027", "#67001F")

breaks <- c(1.2, 1.4, 1.6, 1.8, 2.0)


breaks_label <- c("1.2 - 1.4", "1.4 - 1.6", "1.6 - 1.8",
                  "1.8-2", "2")


df_pal <- data.frame(Color_Value = color_val,
                     Color_Label = breaks_label,
                     stringsAsFactors = F)


scales::show_col(color_val)

columbia_states_sf_1$Color <- NA

for(i in 1:50){
  if(columbia_states_sf_1$State[i] >= breaks[1] & columbia_states_sf_1$State[i] < breaks[2]){
    columbia_states_sf_1$Color[i] <- color_val[1]
  }
  if(columbia_states_sf_1$State[i] >= breaks[2] & columbia_states_sf_1$State[i] < breaks[3]){
    columbia_states_sf_1$Color[i] <- color_val[2]
  }
  if(columbia_states_sf_1$State[i] >= breaks[3] & columbia_states_sf_1$State[i] < breaks[4]){
    columbia_states_sf_1$Color[i] <- color_val[3]
  }
  if(columbia_states_sf_1$State[i] >= breaks[4] & columbia_states_sf_1$State[i] < breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[4]
  }
  if(columbia_states_sf_1$State[i] == breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[5]
  }
  
}


leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = columbia_states_sf_1,
              weight=5,opacity = 1.0,color = ~columbia_states_sf_1$Color,
              fillOpacity = 0.9, smoothFactor = 0.5,
              fillColor = ~columbia_states_sf_1$Color,
              label = ~State) %>%
  addLegend(
    "topright",
    colors = df_pal$Color_Value,
    labels = df_pal$Color_Label, 
    opacity = 0.9
  )




#### Alt 2 ####
#1 = Downstream, 10 observations
#2 = Downstream, 5 observations
#3 = Downstream, No observations
#4 = Random, 10 observations 
#5 = Random, 5 observations
#6 = Random, No observations

file_name <- here::here("results", "map_true_finalstates.csv")
true_finalstates_mapdat <- read.csv(file_name)[-1]

true_finalstates_mapdat1 <- true_finalstates_mapdat %>% filter(dr == "down_5obs")
true_finalstates_mapdat1 <- true_finalstates_mapdat1  %>% arrange(desc(segment))

columbia_states_sf_1 <- cbind(columbia_sf, true_finalstates_mapdat1$dr, true_finalstates_mapdat1$state)
colnames(columbia_states_sf_1)[4:5] <- c("dr", "State")

columbia_states_sf_1$Color <- NA

for(i in 1:50){
  if(columbia_states_sf_1$State[i] >= breaks[1] & columbia_states_sf_1$State[i] < breaks[2]){
    columbia_states_sf_1$Color[i] <- color_val[1]
  }
  if(columbia_states_sf_1$State[i] >= breaks[2] & columbia_states_sf_1$State[i] < breaks[3]){
    columbia_states_sf_1$Color[i] <- color_val[2]
  }
  if(columbia_states_sf_1$State[i] >= breaks[3] & columbia_states_sf_1$State[i] < breaks[4]){
    columbia_states_sf_1$Color[i] <- color_val[3]
  }
  if(columbia_states_sf_1$State[i] >= breaks[4] & columbia_states_sf_1$State[i] < breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[4]
  }
  if(columbia_states_sf_1$State[i] == breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[5]
  }
  
}


leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = columbia_states_sf_1,
              weight=5,opacity = 1.0,color = ~columbia_states_sf_1$Color,
              fillOpacity = 0.9, smoothFactor = 0.5,
              fillColor = ~columbia_states_sf_1$Color,
              label = ~State) %>%
  addLegend(
    "topright",
    colors = df_pal$Color_Value,
    labels = df_pal$Color_Label, 
    opacity = 0.9
  )



#### Alt 3 ####
#1 = Downstream, 10 observations
#2 = Downstream, 5 observations
#3 = Downstream, No observations
#4 = Random, 10 observations 
#5 = Random, 5 observations
#6 = Random, No observations

file_name <- here::here("results", "map_true_finalstates.csv")
true_finalstates_mapdat <- read.csv(file_name)[-1]

true_finalstates_mapdat1 <- true_finalstates_mapdat %>% filter(dr == "down_no_obs")
true_finalstates_mapdat1 <- true_finalstates_mapdat1  %>% arrange(desc(segment))

columbia_states_sf_1 <- cbind(columbia_sf, true_finalstates_mapdat1$dr, true_finalstates_mapdat1$state)
colnames(columbia_states_sf_1)[4:5] <- c("dr", "State")

columbia_states_sf_1$Color <- NA

for(i in 1:50){
  if(columbia_states_sf_1$State[i] >= breaks[1] & columbia_states_sf_1$State[i] < breaks[2]){
    columbia_states_sf_1$Color[i] <- color_val[1]
  }
  if(columbia_states_sf_1$State[i] >= breaks[2] & columbia_states_sf_1$State[i] < breaks[3]){
    columbia_states_sf_1$Color[i] <- color_val[2]
  }
  if(columbia_states_sf_1$State[i] >= breaks[3] & columbia_states_sf_1$State[i] < breaks[4]){
    columbia_states_sf_1$Color[i] <- color_val[3]
  }
  if(columbia_states_sf_1$State[i] >= breaks[4] & columbia_states_sf_1$State[i] < breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[4]
  }
  if(columbia_states_sf_1$State[i] == breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[5]
  }
  
}


leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = columbia_states_sf_1,
              weight=5,opacity = 1.0,color = ~columbia_states_sf_1$Color,
              fillOpacity = 0.9, smoothFactor = 0.5,
              fillColor = ~columbia_states_sf_1$Color,
              label = ~State) %>%
  addLegend(
    "topright",
    colors = df_pal$Color_Value,
    labels = df_pal$Color_Label, 
    opacity = 0.9
  )



#### Alt 4 ####
#1 = Downstream, 10 observations
#2 = Downstream, 5 observations
#3 = Downstream, No observations
#4 = Random, 10 observations 
#5 = Random, 5 observations
#6 = Random, No observations

file_name <- here::here("results", "map_true_finalstates.csv")
true_finalstates_mapdat <- read.csv(file_name)[-1]

true_finalstates_mapdat1 <- true_finalstates_mapdat %>% filter(dr == "rand_10obs")
true_finalstates_mapdat1 <- true_finalstates_mapdat1  %>% arrange(desc(segment))

columbia_states_sf_1 <- cbind(columbia_sf, true_finalstates_mapdat1$dr, true_finalstates_mapdat1$state)
colnames(columbia_states_sf_1)[4:5] <- c("dr", "State")

columbia_states_sf_1$Color <- NA

for(i in 1:50){
  if(columbia_states_sf_1$State[i] >= breaks[1] & columbia_states_sf_1$State[i] < breaks[2]){
    columbia_states_sf_1$Color[i] <- color_val[1]
  }
  if(columbia_states_sf_1$State[i] >= breaks[2] & columbia_states_sf_1$State[i] < breaks[3]){
    columbia_states_sf_1$Color[i] <- color_val[2]
  }
  if(columbia_states_sf_1$State[i] >= breaks[3] & columbia_states_sf_1$State[i] < breaks[4]){
    columbia_states_sf_1$Color[i] <- color_val[3]
  }
  if(columbia_states_sf_1$State[i] >= breaks[4] & columbia_states_sf_1$State[i] < breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[4]
  }
  if(columbia_states_sf_1$State[i] == breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[5]
  }
  
}


leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = columbia_states_sf_1,
              weight=5,opacity = 1.0,color = ~columbia_states_sf_1$Color,
              fillOpacity = 0.9, smoothFactor = 0.5,
              fillColor = ~columbia_states_sf_1$Color,
              label = ~State) %>%
  addLegend(
    "topright",
    colors = df_pal$Color_Value,
    labels = df_pal$Color_Label, 
    opacity = 0.9
  )


#### Alt 5 ####
#1 = Downstream, 10 observations
#2 = Downstream, 5 observations
#3 = Downstream, No observations
#4 = Random, 10 observations 
#5 = Random, 5 observations
#6 = Random, No observations

file_name <- here::here("results", "map_true_finalstates.csv")
true_finalstates_mapdat <- read.csv(file_name)[-1]

true_finalstates_mapdat1 <- true_finalstates_mapdat %>% filter(dr == "rand_5obs")
true_finalstates_mapdat1 <- true_finalstates_mapdat1  %>% arrange(desc(segment))

columbia_states_sf_1 <- cbind(columbia_sf, true_finalstates_mapdat1$dr, true_finalstates_mapdat1$state)
colnames(columbia_states_sf_1)[4:5] <- c("dr", "State")


columbia_states_sf_1$Color <- NA

for(i in 1:50){
  if(columbia_states_sf_1$State[i] >= breaks[1] & columbia_states_sf_1$State[i] < breaks[2]){
    columbia_states_sf_1$Color[i] <- color_val[1]
  }
  if(columbia_states_sf_1$State[i] >= breaks[2] & columbia_states_sf_1$State[i] < breaks[3]){
    columbia_states_sf_1$Color[i] <- color_val[2]
  }
  if(columbia_states_sf_1$State[i] >= breaks[3] & columbia_states_sf_1$State[i] < breaks[4]){
    columbia_states_sf_1$Color[i] <- color_val[3]
  }
  if(columbia_states_sf_1$State[i] >= breaks[4] & columbia_states_sf_1$State[i] < breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[4]
  }
  if(columbia_states_sf_1$State[i] == breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[5]
  }
  
}


leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = columbia_states_sf_1,
              weight=5,opacity = 1.0,color = ~columbia_states_sf_1$Color,
              fillOpacity = 0.9, smoothFactor = 0.5,
              fillColor = ~columbia_states_sf_1$Color,
              label = ~State) %>%
  addLegend(
    "topright",
    colors = df_pal$Color_Value,
    labels = df_pal$Color_Label, 
    opacity = 0.9
  )



#### Alt 6 ####
#1 = Downstream, 10 observations
#2 = Downstream, 5 observations
#3 = Downstream, No observations
#4 = Random, 10 observations 
#5 = Random, 5 observations
#6 = Random, No observations

file_name <- here::here("results", "map_true_finalstates.csv")
true_finalstates_mapdat <- read.csv(file_name)[-1]

true_finalstates_mapdat1 <- true_finalstates_mapdat %>% filter(dr == "rand_no_obs")
true_finalstates_mapdat1 <- true_finalstates_mapdat1  %>% arrange(desc(segment))

columbia_states_sf_1 <- cbind(columbia_sf, true_finalstates_mapdat1$dr, true_finalstates_mapdat1$state)
colnames(columbia_states_sf_1)[4:5] <- c("dr", "State")


columbia_states_sf_1$Color <- NA


for(i in 1:50){
  if(columbia_states_sf_1$State[i] >= breaks[1] & columbia_states_sf_1$State[i] < breaks[2]){
    columbia_states_sf_1$Color[i] <- color_val[1]
  }
  if(columbia_states_sf_1$State[i] >= breaks[2] & columbia_states_sf_1$State[i] < breaks[3]){
    columbia_states_sf_1$Color[i] <- color_val[2]
  }
  if(columbia_states_sf_1$State[i] >= breaks[3] & columbia_states_sf_1$State[i] < breaks[4]){
    columbia_states_sf_1$Color[i] <- color_val[3]
  }
  if(columbia_states_sf_1$State[i] >= breaks[4] & columbia_states_sf_1$State[i] < breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[4]
  }
  if(columbia_states_sf_1$State[i] == breaks[5]){
    columbia_states_sf_1$Color[i] <- color_val[5]
  }
  
}

leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = columbia_states_sf_1,
              weight=5,opacity = 1.0,color = ~columbia_states_sf_1$Color,
              fillOpacity = 0.9, smoothFactor = 0.5,
              fillColor = ~columbia_states_sf_1$Color,
              label = ~State) %>%
  addLegend(
    "topright",
    colors = df_pal$Color_Value,
    labels = df_pal$Color_Label, 
    opacity = 0.9
  )



