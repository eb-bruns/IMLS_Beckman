### Author: Emily Beckman Bruns  ###  Date: 3/25/2022                                |

### DESCRIPTION:
  # This script creates a basic interactive HTML map using the 'leaflet'
  #   package in R.

### INPUTS:
  # Any files that can be viewed spatially: points (CSV with lat-long),
  #   polygons (shapefiles), images (rasters)
  # This script uses a shapefile with points from an oak conservation
  #   project in Mesoamerica

### OUTPUTS:
  # HTML file. You view the map online at a temp address that cannot be
  #   viewed outside the desktop where you ran the script. You can share
  #   the resulting HTML file and other users can download it and open
  #   to view (opens in browser)


######################
### LOAD LIBRARIES ###
######################

# this little code chunk is from Shannon M Still; thanks, Shannon!
  my.packages <- c("leaflet","rgdal")

                #   "RColorBrewer","data.table","tidyverse","mapview","googledrive",,"Polychrome"
  # un-comment the next line to install current versions
  #install.packages(my.packages)
  lapply(my.packages, require, character.only=TRUE)
    rm(my.packages)

#############################
### SET WORKING DIRECTORY ###
#############################

# point to folder where you have all spatial files
main_dir <- "/Users/emily/Desktop/*work/"

#################
### LOAD DATA ###
#################

### FIRST DEFINE PROJECTION / COORDINATE REFERENCE SYSTEM (CRS)

## define initial projection of points (usually WGS84)
  # warning message isn't a problem
wgs.proj <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +no_defs +towgs84=0,0,0")

### READ IN YOUR SHAPEFILE

# read in shapefile with data you'd like to display
pt_data <- readOGR(file.path(main_dir,"Robles_Experimento_VF/Robles_Experimento_VF.shp"))
  # look at variables in the dataset
str(pt_data@data)

# reproject to make sure it has the right CRS
pt_data <- spTransform(pt_data,wgs.proj)

##################
### CREATE MAP ###
##################

title <- "<b>Quercus insignis</b>"
map_note <- "Click points to see details"

# create your map
map <- leaflet() %>%
  ## Base layer
      # see different base layer options here:
      #   https://leaflet-extras.github.io/leaflet-providers/preview/
  addProviderTiles(providers$CartoDB.Voyager) %>%
	## Title
	addControl(title, position = "topright") %>%
  ## Points
	addCircleMarkers(
    data = pt_data,
    lng = ~X, lat = ~Y,
      # you can use a hex code to get an exact color
      # can look up hex colors here: https://htmlcolorcodes.com/color-picker/
      # e.g., you replace "purple" below with "#93D353" to get lime green
    color = "purple", radius = 5, fillOpacity = 0.7, stroke = F,
      # text that appears when you click a point
    popup = ~paste0("Finca: ",Finca,"<br>",
                    "Planta: ",Planta,"<br>",
                    "Z: ",Z)) %>%
	## Add scale bar
	addScaleBar(position = "bottomright",
		options = scaleBarOptions(maxWidth = 150)) %>%
  ## Add mini map to show context
  addMiniMap(
    tiles = providers$Esri.WorldGrayCanvas,
    toggleDisplay = TRUE, position = "bottomright") %>%
	## Add note
	addControl(map_note, position = "topright")

# view your map
map

# save map
htmlwidgets::saveWidget(map, file.path(main_dir,"Karina_leaflet_map.html"))























## DON'T RUN:

##################
### SOME NOTES THAT MAY BE HELPFUL IF ADDING OTHER LAYERS IN THE FUTURE
##################

### READ IN OTHER LAYERS YOU'D LIKE TO ADD (optional)

## Global country boundaries
	#		UIA World Countries Boundaries, UNIGIS Geospatial Education Resources, via ArcGIS Hub
	# 	Dowload here: https://hub.arcgis.com/datasets/252471276c9941729543be8789e06e12_0
world_countries <- readOGR(file.path(main_dir,"UIA_World_Countries_Boundaries/World_Countries__Generalized_.shp"))
# if desired, you can filter to only target countries; speeds things up
  # look at countries
    # reference IOS codes here: https://www.nationsonline.org/oneworld/country_code_list.htm
sort(unique(world_countries@data$ISO))
  # select countries of interest
target_iso <- c("CR","PA") # Costa Rica and Panama
target_countries <- world_countries[world_countries@data$ISO %in% target_iso,]

## Ecoregions
	# 	Terrestrial Ecoregions of the World, via WWF (Olson et al., 2001)
	#		Download here: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
ecoregions <- readOGR(file.path(main_dir,"official/wwf_terr_ecos.shp"))
# project the ecoregions for mapping
ecoregions.wgs <- spTransform(ecoregions,wgs.proj)
# you can also clip the ecoregions to just be target countries (maps faster)
ecoregions.wgs <- ecoregions.wgs[!is.na(ecoregions.wgs@data$REALM),]
ecoregions_clip.wgs <- ecoregions.wgs[ecoregions.wgs@data$REALM=="NT",]
# create color palette for mapping ecoregions
eco_pal_colors <- createPalette(length(unique(ecoregions_clip.wgs@data$ECO_ID)),
	seedcolors = c("#ba3c3c","#ba7d3c","#baab3c","#3ca7ba","#3c6aba","#573cba","#943cba","#ba3ca1","#ba3c55"),
	range = c(5,42), target = "normal", M=50000)
swatch(eco_pal_colors)
eco_pal_colors <- as.vector(eco_pal_colors)
eco_pal <- colorFactor(eco_pal_colors,ecoregions_clip.wgs@data$ECO_ID)

## You can look for and add any other shapefile layers you'd like

### ADD LAYERS TO MAP (order matters)

	## Ecoregions (optional!)
	addPolygons(
		data = ecoregions_clip.wgs,
      # can label them or comment out next line
    label = ~ECO_NAME,
		fillColor = ~eco_pal(ecoregions_clip.wgs@data$ECO_ID),
		fillOpacity = 0.9, color = "#757575", weight = 1.5, opacity = 0.8) %>%
	## Country outlines (optional!)
	addPolygons(
		data = target_countries,
      # can label them or comment out next line
    label = ~COUNTRY,
		fillColor = "transparent", weight = 1.5, opacity = 0.5, color = "black") %>%
