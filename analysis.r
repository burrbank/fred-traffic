# make infographic
#load in packages
library(ggplot2)
library(dplyr)
library(leaflet)
library(grid)
library(gridExtra)
library(extrafont)
library(mapview)
library(png)
library(useful)

map_style_url <- 'https://api.mapbox.com/styles/v1/burrbank/cj9o88ylz45v22rt36ggcv0e9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYnVycmJhbmsiLCJhIjoiY2o5bzdxbGFnNWRkOTJ3bXE2MjZxaHI0NyJ9.E0z0htY-RD4apyrQO3XuOQ'

gray <- '#e2e2e3'
dark_gray <- "#252A34"
neon_red <- "#FF2E63"
neon_blue <- "#08D9D6"

background <- "#3a4559"

# Configure Theme
my_theme <- function() {
  theme(
    
    plot.background = element_rect(fill = background, colour = background),
    panel.background = element_rect(fill = dark_gray),
    axis.text = element_text(colour = neon_blue, face = "bold", family = "Ubuntu"), #change font
    plot.title = element_text(colour = neon_blue, face = "bold", size = 18, vjust = 1, family = "Ubuntu"), #change font
    axis.title = element_text(colour = neon_blue, face = "bold", size = 13, family = "Ubuntu"), #change font
    panel.grid.major.x = element_line(colour = neon_blue), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = neon_blue),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Ubuntu", face = "bold", colour = neon_blue),
    strip.background = element_rect(fill = dark_gray),
    axis.ticks = element_line(colour = neon_blue),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, colour = neon_blue)
  )
}
#load data as df
df <- read.csv('data/traffic_accidents.csv')
intersections <- readLines('data/intersection.txt')
df <- df %>% mutate(inter_id = as.factor(intersections)) %>% rename(lng = X, lat =Y)

top5 <- df %>% group_by(inter_id) %>% summarise(collisions=n()) %>% arrange(-collisions) %>% head(5)
in_top5 <- df$inter_id %in% top5$inter_id

df_in_top5 <- df[in_top5,] %>% mutate(inter_id = droplevels(inter_id))
#adjust factors so that 1 is the largest group
df_in_top5$inter_id = factor(df_in_top5$inter_id, top5$inter_id)

top5$inter_id = droplevels(top5$inter_id)
color_gen <- function(df_in_top5){
   sapply(df_in_top5$inter_id, function(inter_id){
     if(inter_id == top5$inter_id[1]){
       "red"
     } else if(inter_id == top5$inter_id[2]){
       "orange"
     } else if(inter_id == top5$inter_id[3]){
       "green"
     } else if(inter_id == top5$inter_id[4]){
       "purple"
     } else {
       "blue"
     }
   })
}
#colors corrisponding to marker actuall color
red1st <- "#C2372E"
orange2nd <- "#D2893C"
green3rd <- "#729E44"
purple4th <- "#B15EA6"
blue5th <- "#479ABB"
scale <- c(red1st, orange2nd, green3rd, purple4th, blue5th)
 
 
icons <- awesomeIcons(
  icon = 'android-radio-button-on',
  iconColor = 'black',
  library = 'ion',
  markerColor = color_gen(df_in_top5)
)
center <- group_by(df_in_top5, inter_id) %>% summarise(x = mean(lng), y = mean(lat))
x <- mean(center$x)
y <- mean(center$y)

#create map
map <- leaflet(data = df_in_top5, options = leafletOptions(zoomControl=FALSE, attributionControl=FALSE)) %>%
  setView(lng=x, lat=y, zoom=17) %>%
  addAwesomeMarkers(~lng, ~lat, icon=icons) %>%
  addTiles(map_style_url)

mapshot(map, file='map.png',vheight=3000, vwidth=6000)


type_data <- df_in_top5 %>% group_by(Type, inter_id) %>% summarise(total = n()) %>% left_join(top5, by="inter_id") %>% mutate(average=total/collisions)

p1 <- ggplot(data = df_in_top5, aes(x=Type, fill=inter_id))
p1 <- p1 + geom_bar() + scale_fill_manual(values = scale) + my_theme() + facet_grid(.~inter_id) + theme(legend.position = "none")


#pdf device
pdf('poster.pdf', width=20, height=30, title="Traffic visualization", bg = background )
#create layout
#for map
grid.newpage()
pushViewport(viewport(layout = grid.layout(6,3),width=unit(20, "inches"), height=unit(30, "inches")))
print(p1, vp = vplayout(1, 1:2))
grid.raster(readPNG("map.png"), vp = vplayout(2:3, 1:3))
dev.off()
