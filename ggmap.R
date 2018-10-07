# devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)

# Load Data
##############################
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')
master = read.csv('data/all_schools_2015.csv')

# Test Map
##############################

register_google(key = "AIzaSyC4ABT6F3JznHUr-uCTAFs4R3lgDYNul_k")
nyc_map = get_map(location = c(lon = -73.94316, lat = 40.7465), 
                  zoom = 11, 
                  maptype = "terrain",
                  source = "google")
nycMap = ggmap(nyc_map, extent = "panel", legend = "bottomright")
nycMap

schools = geom_point(aes(x = lon, y = lat, color = as.numeric(master$Mean.Scale.Score)), 
                     data = master, 
                     size = 1,
                     alpha = 1,
                     shape = as.factor(master$charter)) 

nycMap + schools + 
  scale_color_gradient(name = "Mean Scale Score", low="red", high="blue") +
  scale_shape_discrete(name = "School Type", solid=T, labels = c("Charter", "Public"))

