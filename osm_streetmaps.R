## ----------------------------------------------------------------------
##
## Script name: osm_streetmaps.R
##
## Purpose of script: Create streetmaps based on Openstreetmap (OSM) data
##
## Author: Moritz Rösch
##
## Date Created: 2021-12-20
##
## Copyright (c) Moritz Rösch, 2021
## Email: moritz.roesch@stud-mail.uni-wuerzburg.de
##
## ----------------------------------------------------------------------
##
## Structure: - definition of area of interest
##            - access and download osm data
##            - add external vector data (optional)
##            - plotting of data in final map
##
## Notes: The script is based on this great tutorial:
##        https://ggplot2tutor.com/tutorials/streetmaps
##
##          The definition of the AOI and its extent can be done in multiple
##        ways and has to be tested based on the desired output. Option 1
##        mostly returns a bounding box which covers a larger area than the
##        city. Option 2 & 3 require creation of extent vector file or
##        knowledge of coordinates.
##
##          Depending on the AOI other OSM keys and values need to be used to
##        create nice maps. The greater the AOI the less detailed a map needs
##        to be. Therefore, only the values representing greater roads, rivers,
##        need to be selected. All the available OSM features (key & value) can
##        be seen here: https://wiki.openstreetmap.org/wiki/Map_features
##   
##          Creation of city name or area text is possible with ggplot. However
##        the most appealing results can be achieved by simply importing .pdf-map
##        into Word or Powerpoint and create a text field with transparent background.
##
## ----------------------------------------------------------------------


# -- Packages ----------------------------------------------------------------

library(osmdata) # access and download of osm data
library(sf)
library(tidyverse)


# -- Area of interest (AOI) --------------------------------------------------

# Definition of AOI and extraction of bounding box to restrict OSM data
# download to extent of AOI. Multiple options to generate bounding box.

# Option 1 - Search function of OSM
aoi_bb <- getbb("Würzburg Germany") # returns bounding box for area based on the OSM location search

# Option 2 - Definition of lat/lon coordinates
aoi_bb <- matrix(c(9.871628, 49.710684, # xmin, ymin
                   10.01443, 49.84546), # xmax, ymax
                 nrow = 2,
                 byrow = FALSE,
                 dimnames = list(c("x", "y"),
                                 c("min", "max")))

# Option 3 - Extracting extent of vectordata
aoi <- st_read("data/wue_extent.gpkg")
aoi_bb <- matrix(st_bbox(aoi),
                 nrow = 2,
                 byrow = FALSE,
                 dimnames = list(c("x", "y"),
                                 c("min", "max")))


# -- Accessing and downloading OSM data --------------------------------------

# OSM data features (e.g. streets) are stored as key value pairs. The key
# "highway" contains multiple different values. Thes values represent different
# kind of "highways", e.g., streets that connect cities (value = "primary")
# or streets in residential areas (value = "residential"). A full reference
# list of all available OSM keys and values and their definition can be seen here:
# https://wiki.openstreetmap.org/wiki/Map_features

# Display all values for a key feature
available_tags("highway")

# For the map creation the following key-value pairs proved useful:
# Key: highway
#   Value:  - motorway (Equivalent to freeways, Autobahn, etc.)
#           - trunk (most important roads that aren´t motorways)
#           - primary (roads that link larger towns)
#           - secondary (linking towns)
#           - tertiary (linking smaller towns and villages)
#           - unclassified (least important roads in country, but still with purpose)
#           - residential (roads to access properties and housing)
#           - living streets (speed restricted roads in residential areas)
#           - service (roads with access to e.g., industrial parks)
#           - pedestrian (roads mainly for pedestrians in city centers)
#           - track (agriculture or forestry roads)
#           - footway (designated footpaths next to streets)
# Key: railway
available_tags("railway")
#   Value:  - rail (train rails)
#           - tram (tram rails)
# Key: natural
available_tags("natural")
#   Value:  - water (every inland water body including lakes, rivers, canals)
# Key: waterway
available_tags("waterway")
#   Value:  - river (displays rivers as a line feature)
#           - stream (smaller than rivers, also as line feature)

# Download osm features

# Big streets
big_streets <- aoi_bb %>% # pass the created bounding box
  opq() %>% #builds an overpass (OSM API) query
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "trunk",
                            "secondary", "tertiary")) %>% #select desired values for key
  osmdata_sf() # returns the OSM overpass query into sf format

big_streets
# To access the geometry features use $
big_streets$osm_lines
ggplot(big_streets$osm_lines) + geom_sf()

# Small streets
small_streets <- aoi_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified", "pedestrian",
                            "service", "footway")) %>%
  osmdata_sf()

# Railway
railway <- aoi_bb %>%
  opq()%>%
  add_osm_feature(key = "railway", value = c("rail", "tram")) %>%
  osmdata_sf()

# Water (River)
water <- aoi_bb %>%
  opq()%>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf()


# -- Add external vector data (optional) -------------------------------------

# Feel free to add additional vector data to embellish your map. A great
# example would be your house or some place of interest in your area.

wue_places <- st_read("data/wue_places.gpkg")


# -- Plot map ----------------------------------------------------------------

# Plotting of extracted OSM features with ggplot. Size and transparency of
# sigle feature sets has to be altered based on the selected AOI.

street_map <- ggplot() +
  geom_sf(data = water$osm_polygons, # osm_polygons are used because outlines of river should be displayed
          inherit.aes = FALSE,
          fill = NA,
          color = "black",
          size = .4,
          alpha = 1) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .08,
          alpha = 0.8) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = 1) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = 1) +
  geom_sf(data = filter(wue_places, NAME == "Residenz"), # optional plotting of external point data
          color = "red",
          size = 0.8,
          alpha = 0.8) +
  coord_sf(xlim = aoi_bb[1,], #takes x coordinates from bbox
           ylim = aoi_bb[2,], #takes y coordinates from bbox
           expand = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_rect(colour = "black", fill=NA, size=2), # creates a frame around map
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
street_map

# Save map as PDF
ggsave(filename="output/Würzburg_example.pdf",
       width = 210, 
       height = 297, # this fits a vertical DINA4 page, for horizontal switch width and height values
       units = "mm")

# LABEL creation or referencing to Word!
# Depending on how you want to display your labels (e.g. city name, Coordinates)
# you can export streetmap without any labels and just add them in Word or
# PowerPoint. The examples in the repo were created with with Word by adding
# a gradually transparent text field (top is 100% transparent, bottom is 0% transparent).
# Within this you can create the city name and coordinates. A dividing line
# also looks nice.