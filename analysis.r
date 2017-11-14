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
library(grDevices)


map_style_url <- 'https://api.mapbox.com/styles/v1/burrbank/cj9o88ylz45v22rt36ggcv0e9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYnVycmJhbmsiLCJhIjoiY2o5bzdxbGFnNWRkOTJ3bXE2MjZxaHI0NyJ9.E0z0htY-RD4apyrQO3XuOQ'

font = "Ubuntu Condensed"

gray <- '#e2e2e3'
dark_gray <- "#252A34"
neon_red <- "#FF2E63"
neon_blue <- "#08D9D6"

background <- "#3a4559"
light_background <- "#4a5770"

# Configure Theme
my_theme <- function() {
  theme(
    
    plot.background = element_rect(fill = background, colour = background),
    panel.background = element_rect(fill = dark_gray),
    axis.text = element_text(colour = neon_blue, face = "bold", family = font), #change font
    plot.title = element_text(colour = neon_blue, face = "bold", size = 18, vjust = 1, family = font), #change font
    axis.title = element_text(colour = neon_blue, face = "bold", size = 13, family = font), #change font
    panel.grid.major.x = element_line(colour = neon_blue), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = neon_blue),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = font, face = "bold", colour = neon_blue),
    strip.background = element_rect(fill = dark_gray),
    axis.ticks = element_line(colour = neon_blue),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, colour = neon_blue, size = 13),
    axis.text.y = element_text(colour = neon_blue, size = 13)
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
       "blue"
     } else {
       "purple"
     }
   })
}
#colors corrisponding to marker actuall color
red1st <- "#C2372E"
orange2nd <- "#D2893C"
green3rd <- "#729E44"
blue4th <- "#479ABB"
purple5th <- "#B15EA6"
scale <- c(red1st, orange2nd, green3rd, blue4th, purple5th)
 
 
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

#mapshot(map, file='map.png',vheight=3000, vwidth=6000)


type_data <- df_in_top5 %>% group_by(Type, inter_id) %>% summarise(total = n()) %>% 
  left_join(top5, by="inter_id") %>% mutate(average=total/collisions) %>% arrange(-collisions)
type_data$inter_id <- factor(type_data$inter_id, levels=top5$inter_id)


p1 <- ggplot(data = type_data, aes(x=Type, y=average, fill=inter_id))
p1 <- p1 + geom_bar(stat = 'identity') + scale_fill_manual(values = scale) +
        my_theme() + facet_grid(.~inter_id) + theme(legend.position = "none", axis.title = element_blank()) +
          scale_y_continuous(labels = scales::percent)

df_in_top5$Date <- as.Date(as.character(df_in_top5$Date))
p2 <- ggplot(data=df_in_top5, aes(x=Date, fill = inter_id, col= inter_id))
p2 <- p2 + geom_area(stat="bin", binwidth = 140) + my_theme() + scale_fill_manual(values = scale) + theme(legend.position = "none", axis.title.y = element_blank()) + scale_x_date(date_breaks = '6 months', date_labels = "%b %Y") + facet_grid(.~inter_id)


p3 <- ggplot(data=df_in_top5, aes(x = inter_id, fill = inter_id)) + geom_bar() + my_theme() + 
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 45, vjust = 1), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank()) + 
    scale_fill_manual(values = scale)

#pdf device
pdf('poster.pdf', width=20, height=30, title="Traffic visualization", bg = background )
#create layout
#for map
grid.newpage()
pushViewport(viewport(layout = grid.layout(6,3),width=unit(20, "inches"), height=unit(30, "inches")))
print(p1, vp = vplayout(4, 1:3)) #by type plot
print(p2, vp = vplayout(6, 1:3)) #area plot


grid.raster(readPNG("map.png"), vp = vplayout(2:3, 1:3))
#grid.rect(vp = vplayout(1, 1:2), gp = gpar(fill=light_background, col="NA"))
grid.text("High Collision Intersections in Fredericton", 
          x = unit(0.5, "npc"), y = unit(0.9, "npc"),
          gp = gpar(fontfamily=font,
                    fontsize=40,
                    fontface="bold.italic",
                    col=neon_blue),
          vp = vplayout(1, 1:2)
)
grid.text("Data visualization with R (leaflet and ggplot2)",
          x = unit(0.5, "npc"), y = unit(0.8, "npc"),
          gp = gpar(fontfamily=font,
                    fontsize=25,
                    fontface="italic",
                    col=neon_blue),
          vp = vplayout(1, 1:2)
)
grid.text(paste(
          "Author:",
          "Github:",
          "Data Source:",
          "Github Repo:", sep = "\n"),  vjust = 1, hjust = 0, 
          x = unit(0.05, "npc"), y = unit(0.5, "npc"),
          gp = gpar(fontfamily=font,
                    fontsize=25,
                    fontface="bold",
                    col=neon_red),
          vp = vplayout(1, 1:2)
)
grid.text(paste(
          "Ryan Porter",
          "https://github.com/burrbank",
          "https://opendata.arcgis.com/datasets/b4547c95f12e47b3b2942c64329088f5_0.csv",
          "https://github.com/burrbank/fred-traffic", sep = "\n"),  vjust = 1, hjust = 0, 
          x = unit(0.2, "npc"), y = unit(0.5, "npc"),
          gp = gpar(fontfamily=font,
                    fontsize=25,
                    fontface="bold",
                    col=neon_blue),
          vp = vplayout(1, 1:2)
)
grid.rect(gp = gpar(fill=dark_gray, col="NA"), vp = vplayout(1, 3))

grid.text(paste(
  "[below] This is a map of fredericton marked with each",
  "collision from the 5 highest collision intersections",
  "in the city (this includes nearby collisions)", sep = "\n"),  vjust = 1, hjust = 0, 
  x = unit(0.1, "npc"), y = unit(0.3, "npc"),
  gp = gpar(fontfamily=font,
            fontsize=20,
            fontface="bold",
            col=neon_blue),
  vp = vplayout(1, 3)
)

grid.text(paste(
  "[left] A histogram showing the total collisions",
  "from each intersection. Color key is the same for",
  "each visual on this poster", sep = "\n"),  vjust = 1, hjust = 0, 
  x = unit(0, "npc"), y = unit(0.9, "npc"),
  gp = gpar(fontfamily=font,
            fontsize=24,
            fontface="bold",
            col=neon_blue),
  vp = vplayout(5, 2)
)


grid.text(paste(
  "[above] This chart gives the percentage (per",
  "intersection) of each type of collision.",
  "This highlights possible problems with the",
  "intersection", sep = "\n"),  vjust = 1, hjust = 0, 
  x = unit(0, "npc"), y = unit(0.9, "npc"),
  gp = gpar(fontfamily=font,
            fontsize=24,
            fontface="bold",
            col=neon_blue),
  vp = vplayout(5, 3)
)

grid.text(paste(
  "[below] This area chart shows the total colisions",
  "over the collection time for each intersection.", sep = "\n"),  vjust = 1, hjust = 0, 
  x = unit(0, "npc"), y = unit(0.55, "npc"),
  gp = gpar(fontfamily=font,
            fontsize=24,
            fontface="bold",
            col=neon_blue),
  vp = vplayout(5, 2)
)

pushViewport(viewport(layout = grid.layout(1, 3), layout.pos.col = 1:2, layout.pos.row = 5))
print(p3, vp = vplayout(1, 1))
dev.off()
