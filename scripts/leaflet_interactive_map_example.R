### Author: Emily Beckman  ###  Date: 1/7/2020                                |

### DESCRIPTION:
  # This script creates interactive HTML maps using the 'leaflet' package
  #   in R. With a basic understanding of leaflet functions, the scripts for
  #   these simple maps can be easily adjusted for different uses.

### INPUTS:
  # Any files that can be viewed spatially: points (CSV with lat-long),
  #   polygons (shapefiles), images (rasters)
  # This script uses distribution points (CSV with lat-long columns) from
  #   the Conservation Gap Analysis of Native U.S. Oaks, 2019:
  #   "all_occ_compiled_unq_all_exsitu_short.csv", which can be downloaded
  #   here, with appropriate access permissions:
  #   https://drive.google.com/open?id=1hVJIdO49eZg_ggJnsXeLFkGGnxMWl-Oy

### OUTPUTS:
  # HTML file. You view the map online at a temp address that cannot be
  #   viewed outside the desktop where you ran the script. File needs to be
  #   hosted online for others to view the map also.


## test edit

#################
### LIBRARIES ###
#################

# this little code chunk is from Shannon M Still! thanks, Shannon!
  my.packages <- c("leaflet","RColorBrewer","tidyverse","data.table",
                   "mapview","googledrive","rgdal")
  # un-comment the next line to install current versions
  #install.packages(my.packages)
  lapply(my.packages, require, character.only=TRUE)
    rm(my.packages)

##############
### SCRIPT ###
##############

# set working directory to location where you have your spatial files
main_dir <- "/Users/emily/Desktop/work/"

#################################
# 1. Map with toggles for species
#################################

# you can view the final product of the following code block here:
#   https://leopardshark.com/esb/oak_test_map.html

# download CSV of oak distribution points (you need permission) and place
#   the file in the working directory you specified above:
#   https://drive.google.com/open?id=1hVJIdO49eZg_ggJnsXeLFkGGnxMWl-Oy

# read in CSV of distribution points
df <- read.csv("all_occ_compiled_unq_all_exsitu_short.csv",as.is=T)
  str(df) # take a look at the data structure

# reorder data so the ex situ points are at the top (easier to view in map)
df <- setorder(df, -dataset)

# create list of unique species names; this is the grouping used to toggle
sp <- unique(df$species)

  # example for creating a grouping variable for number intervals, rather than
  #   factors like species name
  #quakes <- quakes %>%
  #  mutate(groups = case_when(
  #    stations < 30 ~ 1,
  #    stations < 50 ~ 2,
  #    TRUE ~ 3
  #  ))

# create color scheme
# take a look at options for color pre-made schemes, if needed, by running:
#   display.brewer.all()
# this scheme is for categorical (factor) variables
palette_fix <- colorFactor(palette = c("#2cb42c","purple"),
                       levels = c("exsitu","insitu"))
# this scheme is for continuous (numeric) variables
#palette_cont <- colorNumeric(palette = "RdBu",
#                             domain = c(1:20))

# write text that will be map's title
title <- "In situ distribution and ex situ source localities for native U.S.
  oaks of conservation concern"

# create function to plot the map
map_layers <- function(){
  map <- leaflet() %>%
  # add base map; you can add as many separate provider tiles as you wish
  # see all provider tiles (base maps) by running "names(providers)"
  addProviderTiles(
    "CartoDB.PositronNoLabels",
    # you can change the "maxZoom" option depending on how far you want
    #   viewers to be able to zoom (addresses privacy issues)
    options = providerTileOptions(maxZoom = 10))
  # loop through all groups and add each toggle layer one at a time
  for (i in 1:length(sp)) {
    map <- map %>%
    addCircleMarkers(
      data = df %>%
        # change 'species' to the name of the column you groupped
        filter(species == sp[i]),
        group = sp[i],
        lng = ~longitude, lat = ~latitude,
        radius = 4,
        # change 'dataset' to the name of the column you want to define
        #   the color of the points
        color = ~palette_fix(dataset),
        fillOpacity = 1,
        stroke = F
    )
  }
  # add layer control
  map %>%
  # add title
  addControl(title, position = "topright") %>%
  # add legend
  addLegend(
    labels = c("Yes","No"),
    colors = c("#2cb42c","purple"),
    title = "Represented ex situ?",
    position = "topright",
    opacity = 0.75) %>%
  # add layer control panel
  addLayersControl(
    overlayGroups = sp,
    options = layersControlOptions(collapsed = F)) %>%
  hideGroup(sp[2:length(sp)]) # hide all groups except the 1st one
}

# view map
map_layers()

# save map
map_final <- map_layers()
htmlwidgets::saveWidget(map_final, file = "oak_test_map.html")

####################################################
# 2. Original exmaple used to create toggle oak map
####################################################

# load data
data("quakes")

# create a grouping variable
quakes <- quakes %>%
  mutate(groups = case_when(
    stations < 30 ~ 1,
    stations < 50 ~ 2,
    TRUE ~ 3
  ))

# function to plot a map with layer selection
map_layers <- function(){
  # number of groups
  k <- n_distinct(quakes$groups)
  # base map
  map <- leaflet() %>%
    addProviderTiles(
      "CartoDB.PositronNoLabels",
      options = providerTileOptions(maxZoom = 9))
  # loop through all groups and add a layer one at a time
  for (i in 1:k) {
    map <- map %>%
      addCircleMarkers(
        data = quakes %>% filter(groups == i),
          group = as.character(i),
          lng = ~long, lat = ~lat, radius = 1
      )
  }
  # create layer control
  map %>%
    addLayersControl(
      overlayGroups = c(1:k),
      options = layersControlOptions(collapsed = F)) %>%
    hideGroup(as.character(c(2:k))) # hide all groups except the 1st one
}

# view map
map_layers()

##########################################
# 3. Personal test of map without toggles
##########################################

# read in CSV of distribution points
#df <- read.csv("all_occ_compiled_unq_all_exsitu_AJOENSIS.csv")

# create a map
map <- df %>%
  leaflet() %>%
  # you can add as many separate provider tiles as you wish
    # see different ProviderTile (background) options here:
    #   https://leaflet-extras.github.io/leaflet-providers/preview/
  addProviderTiles(
    "CartoDB.PositronNoLabels",
    options = providerTileOptions(maxZoom = 9),
    group = "Base map") %>%
  # the "popup" is the text that appears when you click a point
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    popup = ~paste0("<b>","<i>",species,"</b>","</i>","<br/>",dataset),
    color = ~palette_fix(dataset), #"#2cb42c"
    radius = 3,
    group = "Q. ajoensis"
  ) %>%
  # this creates a little box where you can click layers on and off
  addLayersControl(
    baseGroups = c("Base map"),
    overlayGroups = c("Q. ajoensis"),
    options = layersControlOptions(collapsed = F)
  ) %>%
  # something isn't quite right here
  addLegend(
    labels = c("Ex situ","In situ"),
    colors = ~palette_fix(dataset),
    title = "Legend",
    position = "topright",
    opacity = 0.75)
# view your map!
map

#################################
# 4. Map with toggles for genera
#################################

# you can view the final product of the following code block hhere:
#   https://leopardshark.com/esb/IMLS_exsitu_prelim_map.html

# read in CSV of distribution points
#df <- read.csv("exsitu_prelim_compiled_MortonIMLS_toPlot.csv",as.is=T)
  str(df) # take a look at the data structure

# create list of unique genus names; this is the grouping used to toggle
ge <- unique(df$genus)

# create color palette
# get some hex color codes here: https://www.rapidtables.com/web/color/RGB_Color.html
palette_fix <- colorFactor(palette = c("#D2691E","#7B68EE","#3CB371","#00008B"),
                       levels = c("Malus","Quercus","Tilia","Ulmus"))

# write text that will be map's title
title <- "Target taxa in four priority genera: Wild source localities of ex situ accessions
  provided by collections in our survey"

# create function to plot the map
map_layers <- function(){
  map <- leaflet() %>%
  # add base map; you can add as many separate provider tiles as you wish
  # see all provider tiles (base maps) by running "names(providers)"
  addProviderTiles(
    "CartoDB.PositronNoLabels",
    # you can change the "maxZoom" option depending on how far you want
    #   viewers to be able to zoom (addresses privacy issues)
    options = providerTileOptions(maxZoom = 10))
  # loop through all groups and add each toggle layer one at a time
  for (i in 1:length(ge)) {
    map <- map %>%
    addCircleMarkers(
      data = df %>%
        # change 'species' to the name of the column you groupped
        filter(genus == ge[i]),
        group = ge[i],
        lng = ~long_dd, lat = ~lat_dd,
        radius = 4,
        color = ~palette_fix(genus),
        fillOpacity = 0.75,
        stroke = F,
        popup = ~paste0("<b>","<i>",taxon_full_name_acc,"</b>","</i>","<br/>",inst_short),
    )
  }
  # add layer control
  map %>%
  # add title
  addControl(title, position = "topright") %>%
  # add legend
  addLegend(
    labels = c("Malus","Quercus","Tilia","Ulmus"),
    colors = c("#D2691E","#7B68EE","#3CB371","#00008B"),
    #title = "Target genera",
    position = "topright",
    opacity = 0.5) %>%
  # add layer control panel
  addLayersControl(
    overlayGroups = ge,
    options = layersControlOptions(collapsed = F))
  #hideGroup(ge[2:length(ge)]) # hide all groups except the 1st one
}

# view map
map_layers()

# save map
map_final <- map_layers()
htmlwidgets::saveWidget(map_final, file = "IMLS_exsitu_test_map.html")
