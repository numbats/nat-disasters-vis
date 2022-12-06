library(osmdata)
library(tidyverse)

## To recreate this plot 
# * What data do we need?
# * How do we get it into R?
# * Once its in R what does it look like?
# We can use open street maps

## What is Open Street Maps ?
?osmdata

## Check out all the stuff we can get from OSM data
?available_features

# city details
city_name = "Forbes, Australia"
city_coords = data.frame(long = 148.0076, lat = -33.3844) # Forbes  

# Bounding box for the city
city_bb <- getbb(place_name = city_name)
city_bb

# get all street data
city_highways <- city_bb %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

summary(city_highways)

# *Luxury:* Forbes is small, you may not want to do this for New York

# plot streets
ggplot() +
  geom_sf(data = city_highways$osm_lines,
          inherit.aes = FALSE,
          color = "gray") +
  geom_point(data = city_coords, aes(x = long, y = lat)) +
  theme_bw()

# can split up the street type 

main_streets <- city_bb %>% 
  opq() %>% #timeout = 120
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets = city_bb %>% 
  opq() %>% #timeout = 120
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets <- city_bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

street_plot <- ggplot() +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE, size = 0.2) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE, size = 0.5) +
  geom_sf(data = main_streets$osm_lines,
          inherit.aes = FALSE, size = 0.75) +
  geom_point(data = city_coords, aes(x = long, y = lat)) +
  theme_bw()

street_plot

## Something isn't right ?? 

# Any ideas??? 
  
## Debugging 
  
# * Maybe there are no highways in this data set   
# * Maybe I have made a typo in my code   
# * Maybe I've used the query incorrectly   
# * Maybe highways exceed the bounding box aren't downloaded
# * Maybe highways are a different OSM data type to lines

summary(main_streets)
# Main road didn't work :( 

## Value terms 
city_highways$osm_lines$highway %>% unique()
# Used the wrong value query 
# There is no primary 

# Fixed it
main_streets <- city_bb %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("trunk", "trunk_link")) %>% 
  osmdata_sf() 

street_plot <- ggplot() +
  geom_sf(data = city_highways$osm_lines,
          inherit.aes = FALSE, size = 0.1) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE, size = 0.2) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE, size = 0.3) +
  geom_sf(data = main_streets$osm_lines,
          inherit.aes = FALSE, size = 0.5) +
  geom_point(data = city_coords, aes(x = long, y = lat)) +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Forbes Road Network") +
  theme_minimal()

street_plot

# Find a closed road and filter down to that road data
closed_road_name = "Bedgerabong" 
street_set = city_highways$osm_lines
roads_detected = str_detect(as.vector(street_set$name), pattern = closed_road_name)
closed_road_ind = which(roads_detected == TRUE)
closed_roads = street_set[closed_road_ind, ]
View(selected_roads)

# Plot the closed road
street_plot + 
  geom_sf(data = closed_roads, col = "red") 

## What if only part of that road is closed ?? 
# want sections
street_plot + 
  geom_sf(data = selected_roads, aes(col = osm_id), size = 1.5) 

## Need a meaningful colour scheme
closed_roads <- closed_roads %>% 
  mutate(closures = "Closed") %>%
  mutate(closures = if_else(osm_id == "192072838", "Water over road", closures))

street_plot + 
  geom_sf(data = selected_roads, aes(col = closures), size = 1.5) + 
  scale_color_manual(name = "Closures", values = c("red", "blue")) +
  theme(legend.position = "bottom")

# How might we like to extend this visualisation?

