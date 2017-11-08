# make infographic
#load in packages
library(sets)
library(ggplot2)
library(dplyr)
library(leaflet)
library(grid)
library(gridExtra)
library(extrafont)

map_style_url <- 'https://api.mapbox.com/styles/v1/burrbank/cj9o88ylz45v22rt36ggcv0e9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYnVycmJhbmsiLCJhIjoiY2o5bzdxbGFnNWRkOTJ3bXE2MjZxaHI0NyJ9.E0z0htY-RD4apyrQO3XuOQ'
color1 <- "#33d12e" # green
color2 <- "#e3ea0b" # yellow
color3 <- "#b307f7" # purple

# Configure Theme
my_theme <- function() {
  theme(
    
    plot.background = element_rect(fill = '#e2e2e3', colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#EAEAEA"),
    axis.text = element_text(colour = "#252A34", face = "bold", family = "Ubuntu"), #change font
    plot.title = element_text(colour = "#252A34", face = "bold", size = 18, vjust = 1, family = "Ubuntu"), #change font
    axis.title = element_text(colour = "#252A34", face = "bold", size = 13, family = "Ubuntu"), #change font
    panel.grid.major.x = element_line(colour = "#FF2E63"), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Ubuntu", face = "bold", colour = "#08D9D6"),
    strip.background = element_rect(fill = "#252A34"),
    axis.ticks = element_line(colour = "#FF2E63")
  )
}
#load data as df
df <- read.csv('data/traffic_accidents.csv')
coord <- data.frame(lng=df$X, lat=df$Y)


#cluster the data points to get an aproximation of intersections
k <- kmeans(coord, 250, nstart = 50)
coord <- mutate(coord, cluster = k$cluster )

# top5 = coord dataframe > grouped by the cluster of each point > counting the number of points in each cluster > 
#   sorted from largest cluster to smallest > only returning the top5
top5 <- coord %>% group_by(cluster) %>% summarise(total = n()) %>% arrange(-total) %>% head(5)

in_top5 <- coord$cluster %in% top5$cluster
top5_coord <- coord[in_top5,]

color_gen <- function(top5_coord){
  sapply(top5_coord$cluster, function(cluster){
    if(cluster == top5$cluster[1]){
      "red"
    } else if(cluster == top5$cluster[2]){
      "orange"
    } else if(cluster == top5$cluster[3]){
      "green"
    } else if(cluster == top5$cluster[4]){
      "purple"
    } else {
      "blue"
    }
  })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = color_gen(top5_coord)
)

#create map
map <- leaflet(data = top5_coord, options = leafletOptions(zoomControl=FALSE, attributionControl=FALSE)) %>%
  setView(lng=-66.6431, lat=45.9636, zoom=13) %>%
  addAwesomeMarkers(~lng, ~lat, icon=icons, label=~as.character(cluster)) %>%
  addTiles(map_style_url)

map


