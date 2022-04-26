################################################################################

## proected_area_coverage.R
### Author: Emily Beckman Bruns
### Date: 03/21/2022

### DESCRIPTION:
# This script ____________________

### DATA IN:
### BOUNDARIES
## Global country boundaries
#		UIA World Countries Boundaries, UNIGIS Geospatial Education Resources, via
#		ArcGIS Hub Shapefile
#		https://hub.arcgis.com/datasets/252471276c9941729543be8789e06e12_0
### OCCURRENCE POINTS
## In situ occurrence points (latitude and longitude in decimal degrees)
# 	Can use the output from 3-1_refine_occurrence_points.R
# 	https://github.com/MortonArb-CollectionsValue/OccurrencePoints/tree/master/scripts
# 	Each file has data for only one species and is named "Genus_species.csv"
# 	You can read in data for mult. species in one file but need to edit the
#		code to split after reading in
## Ex situ wild localities (latitude and longitude in decimal degrees)
# 	Can use the output from 3-1_refine_occurrence_points.R, which has a
#		"database" column that has "Ex_situ" to distinguish the ex situ records
#		from the rest of the in situ records
### ECOREGIONS
## Global Ecoregions
# 	Terrestrial Ecoregions of the World, via WWF (Olson et al., 2001)
#		https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
## U.S. Ecoregions
#		EPA Level IV Ecoregions
#   https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4_state_boundaries.zip
### PROTECTED AREAS (PAs)
## Global PAs
# 	World Database on Protected Areas (WDPA)
#		https://www.iucn.org/theme/protected-areas/our-work/world-database-protected-areas
## U.S. PAs
#		Protected Areas Database of the United States (PAD-US)
#   https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download

### DATA OUT:
## _______________________

################################################################################
# Load libraries
################################################################################

## [code chunk from Shannon M. Still]
my.packages <- c("leaflet","raster","ConR", #"rnaturalearth","ggplot2",
                 "dplyr","sf","spThin","sp","lwgeom",
                 "rgdal","rgeos") #these will be retired by end of 2023
#install.packages(my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
sessionInfo()
rm(my.packages)

################################################################################
# Set working directory
################################################################################

# either set manually:
#setwd("Desktop/*work/PA-test")
#main_dir <- "/Volumes/GoogleDrive-103729429307302508433/Shared drives/IMLS MFA/occurrence_points"

# or use 0-1_set_workingdirectory.R script from OccurrencePoints repo:
#source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")

################################################################################
# Load functions
################################################################################

# clip points by boundary so only in target area
# (helpful if focusing on one country/region)
clip.by.boundary <- function(pts,pt_proj,boundary){
  # select coordinate columns
  latlong <- pts %>% select(decimalLongitude,decimalLatitude)
  # turn occurrence point data into a SpatialPointsDataFrame
  spatial_pts <- SpatialPointsDataFrame(latlong, pts, proj4string = pt_proj)
  # clip by boundary created earlier
  spatial_pts <- spatial_pts[boundary, ]
  # keep just the data (not the spatial info you added)
  pts_new <- spatial_pts@data
  return(pts_new)
}

# create buffers around points, using specified projection
create.buffers <- function(df,radius,pt_proj,buff_proj,boundary){
  # select coordinate columns
  latlong <- df %>% select(decimalLongitude,decimalLatitude)
  # turn occurrence point data into a SpatialPointsDataFrame
  sp_df <- SpatialPointsDataFrame(latlong, df, proj4string = pt_proj)
  # reproject SpatialPointsDataFrame to specified projection
  proj_df <- spTransform(sp_df,buff_proj)
  # place buffer around each point
  buffers <- buffer(proj_df,width=radius,dissolve=T)
  # clip buffers by boundary (e.g., state, country)
  buffers_clip <- raster::intersect(buffers,boundary)
  # return buffer polygons
  return(buffers_clip)
}

# calculate percent of occurrence points in global protected areas (WDPA)
#		first thin points, then see which are in PAs, then calculate percent
pts.in.pa <- function(occ.pts,lat_col,long_col,sp_col,thin.km,my.proj,pa.list){
  # first thin points; thin.km gives the distance between thinned pts
  thin_pts <-
    spThin::thin(
      loc.data=occ.pts,
      lat.col=lat_col,
      long.col=long_col,
      spec.col=sp_col,
      thin.par=thin.km, #kilometers b/w pts
      reps=1, #right now we are just doing once, but should prob do more?
      locs.thinned.list.return=T,
      write.files=F,
      write.log.file=F)
  thinned_pts <- thin_pts[[1]]
  # count number of points after thinning
  num_thinned <- nrow(thinned_pts)
  # make thinned points a spatial object
  thinned_spatial <- SpatialPointsDataFrame(thinned_pts,thinned_pts,proj4string=my.proj)
  # see which points are in protected areas polygons
  #		WDPA comes in three parts; I've left it like that to
  #		hopefully make this calculation a little faster
  pt_in_pa_list <- lapply(pa.list, function(x) thinned_spatial[x,])
  # create one object with all pts found in PAs
  pt_in_pa <- do.call(rbind, pt_in_pa_list)
  # count number of points in PAs
  in_pa <- nrow(pt_in_pa@data)
  # create dataframe of values to return
  pt_in_pa_stats <- data.frame(
    num_pt_unthinned = nrow(occ.pts),
    km_thin_value = thin.km,
    num_pt_thinned = num_thinned,
    num_pt_in_pa = in_pa,
    per_pt_in_pa = round(((in_pa/num_thinned)*100),digits=2))
  return(pt_in_pa_stats)
}

# calculate area of intersection of 50km buffers around occurrence points and 
#   global protected areas (WDPA)
buffer.in.pa <- function(occ.pts,radius,pt_proj,buff_proj,pa.list,boundary){
  #
  ## create 50km buffers around in situ points (aea proj) & calculate area 
  ##   fast using sp method, slow using sf - not sure exactly why
  # sp method (fast):
  buff_50_sp <- create.buffers(occ.pts,radius,pt_proj,buff_proj,boundary)
  buff_50_sp <- raster::aggregate(buff_50_sp)
  # calculate area
  #buff_area_sp <- buff_50_sp@polygons[[1]]@area/1000000
  # sf method (slow):
  #insitu_sf <- st_as_sf(x = occ.pts,                         
  #  coords = c("decimalLongitude", "decimalLatitude"),
  #  crs = pt_proj)
  #buff_50_sf <- st_union(st_buffer(insitu_sf,radius))
  # can just convert sp version to sf
  buff_50_sf <- st_as_sf(buff_50_sp)
  # calculate area
  buff_area_sf <- as.numeric(st_area(buff_50_sf)/1000000)
  # see difference between buffer areas calculated using sp and sf
  #buff_diff_num <- (buff_area_sp - buff_area_sf)
  #buff_diff_per <- buff_diff_num/buff_area_sp*100
  #
  ## loop through PA layers (there are 3) and find intersection b/w
  ##  PAs and buffer around occurrence points
  pa_buff_intersect_lst <- list()
  for(i in 1:length(pa.list)){
    ## sp (spatial polygons) method --> SLOW
    #pa_layer_aea <- spTransform(pa.list[[1]],buff_proj)
    #pa_intersect_id <- sp::over(buff_50_sp,pa_layer_aea)
    #pa_intersect_sp <- gIntersection(buff_50_sp,pa.list[[1]],checkValidity=2L)
    ## sf (simple features) method --> faster
    # select PA features that overlap with buffer layer
    pa_intersect_id <- st_intersects(buff_50_sf,pa.list[[i]]) %>% unlist()
    pa_intersect <- st_union(pa.list[[i]][pa_intersect_id,])
    # intersect the buffer and PA layers to get overlap
    buffer_in_pa <- st_union(st_intersection(buff_50_sf,pa_intersect))
    # add results to list 
    pa_buff_intersect_lst[[i]] <- buffer_in_pa
  }
  #
  ## combine all PA intersection layers & calc percent coverage
  pa_buff_intersect <- Reduce(st_union,pa_buff_intersect_lst)
    # visualize
    #plot(buff_50_sf); plot(pa_buff_intersect,add=T)
  #	compare the buffer area to the buff_pa_inter area to get % coverage
  pa_intersect_area <- as.numeric(st_area(pa_buff_intersect)/1000000)
  pa_intersect_per <- pa_intersect_area/buff_area_sf*100
  #
  ## create dataframe of values to return
  buff_in_pa_stats <- data.frame(
    area_buff = round(buff_area_sf,digits=2),
    #area_buff_sp = buff_area_sp,
    #compare_buff_num = buff_diff_num,
    #compare_buff_per = buff_diff_per,
    area_pa_in_buff = round(pa_intersect_area,digits=2),
    per_pa_in_buff = round(pa_intersect_per,digits=2))
  return(buff_in_pa_stats)
}
  
  
################################################################################
################################################################################
# Choose target species
################################################################################

# read in target taxa list
taxon_list <- read.csv("target_species_with_syn.csv",
                       header = T, na.strings = c("","NA"),colClasses = "character")
head(taxon_list)

# OPTIONAL, depending on your workflow:
#		read in manual edits to target species maps
pt_edits <- read.csv("manual_point_edits.csv",
                     header = T, na.strings = c("","NA"),colClasses = "character")
head(pt_edits)

# OPTIONAL: select target species
target_sp <- taxon_list %>% filter(grepl("^MAP",map_flag))
target_sp <- sort(gsub(" ","_",target_sp$species_name_acc))
length(target_sp) #30

# OPTIONAL: load native dist information for each species
native_dist <- read.csv("target_taxa_with_native_dist.csv",
                        header = T, na.strings = c("","NA"), colClasses = "character")
native_dist <- native_dist %>% dplyr::select(species_name_acc,rl_native_dist,
                                             gts_native_dist)

################################################################################
# Read in polygon data
################################################################################

## define projections
  # WGS84, used for point calculations
wgs.proj <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84
	+no_defs +towgs84=0,0,0")
  # Albers Equal Area, used for area calculations (meters)
aea.proj <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110
	+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")

## read in shapefile of global protected areas
## CITATION: UNEP-WCMC and IUCN (2022), Protected Planet: The World Database on Protected Areas (WDPA) [Online], February 2022, Cambridge, UK: UNEP-WCMC and IUCN. Available at: www.protectedplanet.net.
# As of January 2022, the WDPA includes data on 269,457 protected areas of
#		which 96% were polygons and 4% points.
# since the shapefile is large, the source has it split into three;
#		we will load each
# need to be unzipped the first time
#unzip(zipfile="WDPA_WDOECM_Mar2022_Public_all_shp/WDPA_WDOECM_Mar2022_Public_all_shp_0.zip",
#      exdir="WDPA_WDOECM_Mar2022_Public_all_shp_0")
#unzip(zipfile="WDPA_WDOECM_Mar2022_Public_all_shp/WDPA_WDOECM_Mar2022_Public_all_shp_1.zip",
#      exdir="WDPA_WDOECM_Mar2022_Public_all_shp_1")
#unzip(zipfile="WDPA_WDOECM_Mar2022_Public_all_shp/WDPA_WDOECM_Mar2022_Public_all_shp_2.zip",
#      exdir="WDPA_WDOECM_Mar2022_Public_all_shp_2")
# two ways to read in shapefiles...

## 1) read in using raster package (spatial polygons; for point-in-poly analysis)
  # WGS84 projection
pa0 <- readOGR("WDPA_WDOECM_Mar2022_Public_all_shp_0/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  # project to WGS proj that we've defined, so its exact
  pa0 <- spTransform(pa0,wgs.proj)
pa1 <- readOGR("WDPA_WDOECM_Mar2022_Public_all_shp_1/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  pa1 <- spTransform(pa1,wgs.proj)
pa2 <- readOGR("WDPA_WDOECM_Mar2022_Public_all_shp_2/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  pa2 <- spTransform(pa2,wgs.proj)
  # create list of layers for use later
pa_layers_sp_wgs <- list(pa0,pa1,pa2)
rm(pa0,pa1,pa2)
  #pa <- Reduce(rbind,c(pa0,pa1,pa2)) # takes too long, probably better to keep separate anyways for speed

## 2) read in using sf package (simple polygons; for polygon intersection analysis)
  # AEA projection
pa0 <- st_read("WDPA_WDOECM_Mar2022_Public_all_shp_0/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  # project to equal earth (meter measurements) for area calculations later
  pa0 <- pa0 %>% st_transform(crs = aea.proj)
pa1 <- st_read("WDPA_WDOECM_Mar2022_Public_all_shp_1/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  pa1 <- pa1 %>% st_transform(crs = aea.proj)
pa2 <- st_read("WDPA_WDOECM_Mar2022_Public_all_shp_2/WDPA_WDOECM_Mar2022_Public_all_shp-polygons.shp")
  pa2 <- pa2 %>% st_transform(crs = aea.proj)
# create list of layers for use later
pa_layers_sf_aea <- list(pa0,pa1,pa2)
rm(pa0,pa1,pa2)

# read in shapefile of global ecoregions
#ecoregions <- readOGR(file.path(main_dir,"inputs","gis_data",
#	"official","wwf_terr_ecos.shp"))

# read in shapefile of U.S. EPA Level III Ecoregions
#ecol3 <- readOGR(file.path(main_dir,"inputs","gis_data",
#	"us_eco_l3/us_eco_l3.shp"))

# read in shapefile of U.S. EPA Level IV Ecoregions
  # spatial polygons  
ecol4_sp <- readOGR("us_eco_l4_state_boundaries/us_eco_l4.shp")
ecol4_sp <- spTransform(ecol4_sp,aea.proj)
  # simple features
#ecol4_sf <- st_read("us_eco_l4_state_boundaries/us_eco_l4.shp")
#ecol4_sf <- ecol4_sf %>% st_transform(crs = aea.proj)

# read in shapefile of country boundaries
world_countries <- readOGR("UIA_World_Countries_Boundaries-shp/World_Countries__Generalized_.shp")
#world_countries.aea <- spTransform(world_countries,aea.proj)
# optional; if you are subsetting points to country(ies) of interest
target_iso <- c("US")
target_countries <- world_countries[world_countries@data$ISO %in% target_iso,]
# create polygon for clipping points later
target_countries.wgs <- spTransform(target_countries,wgs.proj)
boundary.wgs <- aggregate(target_countries.wgs,dissolve = TRUE)
# remove objects we don't use later
rm(target_iso,target_countries,target_countries.wgs,world_countries)

################################################################################
## Calculate geographic and ecological coverage of ex situ collections
################################################################################

### START SUMMARY TABLE

# we add each target species as we go along
#summary_tbl <- data.frame(
#  species = "start",
#  EOO = "start",
#  dist_filter = "start",
#    #pt-in-pa
#  num_pt_unthinned = "start",
#  km_thin_value = "start",
#  num_pt_thinned = "start",
#  num_pt_in_pa = "start",
#  per_pt_in_pa = "start",
#    #buff-in-pa
#  area_buff = "start",
#  area_pa_in_buff = "start",
#  per_pa_in_buff = "start",
#  stringsAsFactors=F)

### CYCLE THROUGH TARGET SPECIES TO CALCULATE EX SITU COVERAGE

for(sp in 1:length(target_sp)){

## can also test with one species first
#sp <- 5

  # print progress
  cat("\n"); print(paste0("Starting ", target_sp[sp], " at ", Sys.time()))
  print("Filtering occurrence points")
  
  ### READ IN AND PREP POINT DATA
  
  ## read in occurrence points (includes ex situ)
  insitu_raw <- read.csv(file.path("spp_edited_points",
                                   paste0(target_sp[sp],".csv")),
                         na.strings=c("","NA"), stringsAsFactors = F)
  nrow(insitu_raw)
  spp.rl.dist <- native_dist[which(native_dist$species_name_acc == gsub("_"," ",target_sp[sp])),]
  ## filter as desired
  insitu <- insitu_raw %>%
    filter(database == "Ex_situ" |
             (.cen & .inst & .con & .outl &
                ### for gap analysis of nine genera dataset
                .bonapnative & .yr1950 & .yrna &
                #.urb & .yr1950 & .yr1980 & .yrna &
                #(.gtsnative | is.na(.gtsnative)) &
                #(.rlnative  | is.na(.rlnative)) &
                #(.rlintroduced | is.na(.rlintroduced)) &
                basisOfRecord != "FOSSIL_SPECIMEN" & basisOfRecord != "LIVING_SPECIMEN" &
                ### for gap analysis of nine genera dataset
                basisOfRecord != "H?" &
                establishmentMeans != "INTRODUCED" & establishmentMeans != "MANAGED" &
                establishmentMeans != "INVASIVE"))
  if(!is.na(spp.rl.dist$rl_native_dist)){
    insitu <- insitu %>%
      filter(.rlnative | is.na(.rlnative))
    dist_filter_val <- "RL"
  } else if(!is.na(spp.rl.dist$gts_native_dist)){
    insitu <- insitu %>%
      filter(.gtsnative | is.na(.gtsnative))
    dist_filter_val <- "GTS"
  } else {
    dist_filter_val <- "N/A"
  }
  nrow(insitu)
  
  ## check document with manual point edits to see if anything needs to be added back or removed
  manual.edit <- pt_edits[which(pt_edits$species_name_acc == gsub("_"," ",target_sp[sp])),]
  # bounding box
  if(!is.na(manual.edit$bounding_box)){
    bounds <- unlist(strsplit(manual.edit$bounding_box,"; "))
    for(i in 1:length(bounds)){
      within <- unlist(strsplit(bounds[i],", "))
      insitu <- insitu %>% filter(!(decimalLongitude > as.numeric(within[1]) &
                                      decimalLongitude < as.numeric(within[3]) &
                                      decimalLatitude > as.numeric(within[2]) &
                                      decimalLatitude < as.numeric(within[4])))
    }
  }; nrow(insitu)
  # remove
  if(!is.na(manual.edit$remove)){
    remove <- unlist(strsplit(manual.edit$remove,"; "))
    insitu <- insitu %>% filter(!(UID %in% remove))
  }; nrow(insitu)
  ### for gap analysis of nine genera dataset; select US points only
  insitu <- clip.by.boundary(insitu,wgs.proj,boundary.wgs)
  # add back
  if(!is.na(manual.edit$keep)){
    keep <- unlist(strsplit(manual.edit$keep,"; "))
    add <- insitu_raw %>% filter(UID %in% keep)
    insitu <- suppressMessages(full_join(insitu,add))
  }; nrow(insitu)
  
  ### for gap analysis of US oaks 2020
  #exsitu_raw <- read.csv(file.path(main_dir,target_sp[sp],
  #	paste0(target_sp[sp],"_exsitu.csv")), na.strings=c("","NA"), stringsAsFactors = F)
  #	nrow(exsitu_raw)
  #exsitu <- exsitu_raw %>%
  #	filter(!is.na(latitude) & !is.na(longitude)) %>%
  #	rename(decimalLatitude = latitude,
  #			 	 decimalLongitude = longitude)
  #	nrow(exsitu)
  #insitu_raw <- read.csv(file.path(main_dir,target_sp[sp],
  #	paste0(target_sp[sp],"_insitu.csv")), na.strings=c("","NA"), stringsAsFactors = F)
  #	nrow(insitu_raw)
  #insitu <- insitu_raw %>%
  #	filter(!is.na(latitude) & !is.na(longitude)) %>%
  #	rename(decimalLatitude = latitude,
  #			 	 decimalLongitude = longitude)
  #insitu <- full_join(insitu,exsitu)
  #nrow(insitu)
  
  # print progress
  print("Calculating EOO")
  
  ### CALCULATE EOO (convex hull)
  
  # using package ConR to calculate area of convex hull
  insitu_df <- insitu %>% select(decimalLatitude,decimalLongitude,species_name_acc)
  hull_area <- EOO.computing(XY=insitu_df,write_results=F)
  
  ### CALCULATE PROTECTED AREA COVERAGE
  
  ## POINT-IN-POLYGON METHOD
  
  # print progress
  print("Starting point-in-PA analysis")
  
  ## plot all points, to see difference after thinning
  # select coordinate columns
  #latlong <- insitu %>% select(decimalLongitude,decimalLatitude)
  # turn occurrence point data into a SpatialPointsDataFrame
  #sp_df <- SpatialPointsDataFrame(latlong, insitu, proj4string = wgs.proj)
  #plot(sp_df)
  
  # see function for methodology details
  pt_pa_stats <- pts.in.pa(insitu,"decimalLatitude","decimalLongitude",
                        "species_name_acc",2,wgs.proj,pa_layers_sp_wgs)
  
  ## BUFFER-IN-POLYGON METHOD
  
  # print progress
  print(paste0("Starting buffer-PA-intersection analysis at ", Sys.time()))
  
  # see function for methodology details
  buff_pa_stats <- buffer.in.pa(insitu,50000,wgs.proj,aea.proj,
                               pa_layers_sf_aea,ecol4_sp)
  
  ## NOT USING
  
  ## create df with just ex situ points
  #exsitu <- insitu %>% filter(database == "Ex_situ")
  # get number of individuals ex situ with lat-long data
  #print(paste("Number of ex situ individuals:",sum(as.numeric(exsitu$establishmentMeans))))
  
  ### TURN THIS ON TO CALCULATE FOR MORTON ACCESSIONS ONLY
  #exsitu <- exsitu %>% filter(datasetName == "MortonArb")
  
  # check there are ex situ points, if not skip to end (can't do calculations)
  #	if(nrow(exsitu) == 0){
  #	  # add text results to summary table
  #		summary_add <- data.frame(
  #			species = gsub("_"," ",target_sp[sp]),
  #			geo_sm = NA, geo_md = NA,	geo_lg = NA, geo_vlg = NA,
  #			eco_sm = NA, eco_md = NA, eco_lg = NA,
  #			eco_usl4_sm = NA, eco_usl4_md = NA, eco_usl4_lg = NA,
  #			EOO = hull_area,
  #				### turn the next row off for gap analysis of US oaks 2020
  #			#dist_filter = dist_filter_val,
  #			stringsAsFactors=F)
  #		print("No ex situ points; skipping buffer calculations")
  #	} else {
  #
  #		### CALCULATE EX SITU COVERAGE
  #
  #			## Geographic coverage
  #
  #		# calculate area based on largest buffers
  #		geo_coverage_vlg <- compare.buff.area(insitu,exsitu,largest_buff,wgs.proj,aea.proj)
  #		# calculate area based on large buffers
  #		geo_coverage_lg <- compare.buff.area(insitu,exsitu,large_buff,wgs.proj,aea.proj)
  #		# calculate area based on medium buffers
  #		geo_coverage_md <- compare.buff.area(insitu,exsitu,med_buff,wgs.proj,aea.proj)
  #		# calculate area based on small buffers
  #		geo_coverage_sm <- compare.buff.area(insitu,exsitu,small_buff,wgs.proj,aea.proj)
  #
  #			## Ecological coverage
  #
  #		## Global ecoregions
  #		# count ecoregions under large buffers
  #		eco_coverage_lg <- compare.eco.count(insitu,exsitu,large_buff,wgs.proj,aea.proj,ecoregions)
  #		# count ecoregions under medium buffers
  #		eco_coverage_md <- compare.eco.count(insitu,exsitu,med_buff,wgs.proj,aea.proj,ecoregions)
  #		# count ecoregions under small buffers
  #		eco_coverage_sm <- compare.eco.count(insitu,exsitu,small_buff,wgs.proj,aea.proj,ecoregions)
  #
  #		## U.S. Level 4 (most specific) ecoregions
  #		# get just points that are in the U.S.
  #		us_insitu <- clip.by.boundary(insitu,wgs.proj,boundary.wgs)
  #		us_exsitu <- clip.by.boundary(exsitu,wgs.proj,boundary.wgs)
  #		# if there are in situ and ex situ points in the U.S., then calculate coverage
  #		if(nrow(us_exsitu) > 0 & nrow(us_insitu) > 0){
  #			# count ecoregions under large buffers
  #			ecol4_coverage_lg <- compare.ecol4.count(us_insitu,us_exsitu,large_buff,wgs.proj,aea.proj,ecol4)
  #			# count ecoregions under medium buffers
  #			ecol4_coverage_md <- compare.ecol4.count(us_insitu,us_exsitu,med_buff,wgs.proj,aea.proj,ecol4)
  #			# count ecoregions under small buffers
  #			ecol4_coverage_sm <- compare.ecol4.count(us_insitu,us_exsitu,small_buff,wgs.proj,aea.proj,ecol4)
  #		# if there's distribution in the U.S. but no ex situ points, assign 0%
  #		} else if(nrow(us_exsitu) == 0 & nrow(us_insitu) > 0){
  #			ecol4_coverage_lg <- "0%"
  #			ecol4_coverage_md <- "0%"
  #			ecol4_coverage_sm <- "0%"
  #		# if not in U.S. then NA
  #		} else {
  #			ecol4_coverage_lg <- NA
  #			ecol4_coverage_md <- NA
  #			ecol4_coverage_sm <- NA
  #		}
  #
  
  ### SUMMARY TABLE
  
  ## Add results to summary table
  summary_add <- data.frame(
    species = gsub("_"," ",target_sp[sp]),
    EOO = hull_area,
    dist_filter = dist_filter_val,
    stringsAsFactors=F)
  # add results from pt-in-pa analysis
  summary_add <- cbind(summary_add,pt_pa_stats)
  # add results from buffer-in-pa analysis
  summary_add <- cbind(summary_add,buff_pa_stats)
  
  print(summary_add)
  write.csv(summary_add, paste0("PA-coverage_",target_sp[sp],".csv"), 
            row.names = F)
  
  #summary_tbl[sp,] <- summary_add

}

## write summary table
#summary_tbl
#write.csv(summary_tbl, "GapAnalysis_PA-coverage_04-25-2022.csv", row.names = F)

### for gap analysis of US oaks 2020
#write.csv(summary_tbl, file.path(main_dir,
#	"Oaks2020_BufferTable.csv"), row.names = F)
