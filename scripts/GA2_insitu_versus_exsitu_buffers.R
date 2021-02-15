### Author: Emily Beckman  ###  Date: 12/11/2020

### DESCRIPTION: Calculate geographic and ecological coverage of ex situ
	# collections, and (optionally) create a map of coverage.
	# See Beckman et al. (2021) for examples of output table and maps: [[LINK WHEN READY]]

### INPUTS:
	# Ecoregions
		# U.S. only: https://www.epa.gov/eco-research/level-iii-and-iv-ecoregions-continental-united-states
			## US Level III Ecoregions shapefile without state boundaries (28 mb)
				# ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip
			## US Level IV Ecoregions shapefile with state boundaries (69 mb)
				# ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4_state_boundaries.zip
		# Global: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
			## Terrestrial Ecoregions of the World, via WWF (Olson et al., 2001)
				# https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip?1349272619
	## U.S. state boundaries
		# https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_5m.zip
	## Global country boundaries
		# https://opendata.arcgis.com/datasets/252471276c9941729543be8789e06e12_0.zip
	## In situ occurrence points (latitude and longitude in decimal degrees)
		# I am using the output from 3-1_refine_occurrence_points.R
			# https://github.com/MortonArb-CollectionsValue/OccurrencePoints/tree/master/scripts
		# Each file has data for only one species and is named "Genus_species.csv"
		# You can read in data for mult. species in one file but need to edit the
		#		code to split after reading in
	## Ex situ wild localities (latitude and longitude in decimal degrees)
		# I am using the output from 3-1_refine_occurrence_points.R, which has a
		#		"database" column that has "Ex_situ" to distinguish the ex situ records
		#		from the rest of the in situ records

### OUTPUTS:
	# Summary table with output geographic and ecological coverage for each
	#		target species, using three buffer sizes: 20km, 50km, and 100km
	# If desired, create map for each species, showing ecoregions, insitu buffers,
	#		exsitu buffers, number of plants from exsitu wild collection locations


################################################################################

#################
### LIBRARIES ###
#################

my.packages <- c("leaflet","raster","sp","rgeos","dplyr","rgdal",
	"RColorBrewer","Polychrome","cleangeo","smoothr")
#install.packages (my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)

select <- dplyr::select

#################
### FUNCTIONS ###
#################

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

# create buffers around points and calculate area
calc.buff.area <- function(pts,radius,pt_proj,buff_proj,boundary){
	# create buffers
	buffers <- create.buffers(pts,radius,pt_proj,buff_proj,boundary)
	# calculate buffer area
	buff_area <- buffers@polygons[[1]]@area/1000000
	#print(paste("Area covered by buffers:", round(buff_area,0),"km²"))
	return(buff_area)
}

# create data frame with ecoregion data extracted for area covered by buffers,
#		for both in situ and ex situ points, then compare count of ecoregions;
##	for U.S. EPA Level 4 Ecoregions
count.eco.usl4 <- function(pts,radius,pt_proj,buff_proj,ecoregions,boundary){
	# create buffers
	buffers <- create.buffers(pts,radius,pt_proj,buff_proj,boundary)
	# make sure ecoregions are in same projection as buffers
	eco_proj <- spTransform(ecoregions,buff_proj)
	# intersect buffers with ecoregions
	buff_join_eco <- raster::intersect(buffers,eco_proj)
	# count number of ecoregions under buffers
	count_eco <- nrow(buff_join_eco@data %>% distinct(US_L4CODE))
	#print(paste0("Number of ecoregions under ex situ buffers: ",count_eco))
	return(count_eco)
}
##	for WWF Global Ecoregions
count.eco.wwf <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco){
	# create buffers
	buffers <- create.buffers(pts,radius,pt_proj,buff_proj,boundary)
	# make sure ecoregions are in same projection as buffers
	eco_proj <- spTransform(ecoregions,buff_proj)
	# intersect buffers with ecoregions
	buff_join_eco <- raster::intersect(buffers,eco_proj)
	# count number of ecoregions under buffers
	count_eco <- nrow(buff_join_eco@data %>% distinct(ECO_ID))
	#print(paste0("Number of ecoregions under ex situ buffers: ",count_eco))
	return(count_eco)
}

# format text in cell for output table
format.cell <- function(ex_result,in_result,final_result){
	cell <- paste0(format(round(ex_result,0),format="d",big.mark=",")," / ",
								 format(round(in_result,0),format="d",big.mark=","),"\n",
								 "(",round(final_result,0),"%)")
	return(cell)
}

#########################
### WORKING DIRECTORY ###
#########################

# set up working directories
	# for point data (in situ and ex situ)
pts_dir <- "/Volumes/GoogleDrive/My Drive/Conservation Gap Analysis"
	# for polygon data (ecoregions, states, countries)
poly_dir <- "./Desktop/work"
	# for outputs
output_dir <- "/Volumes/GoogleDrive/My Drive/Conservation Gap Analysis"

################################################################################
## Set things up
################################################################################

### DEFINE PROJECTIONS

# define initial projection of points (usually WGS 84)
wgs.proj <- sp::CRS(SRS_string="EPSG:4326")
	##CRS arguments: +proj=longlat +datum=WGS84 +no_defs
# define projection for calculations (meters must be the unit)
aea.proj <- sp::CRS(SRS_string="EPSG:5070")
	##CRS arguments: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs

### READ IN POLYGON DATA

## Ecoregions
	## U.S. only
		# https://www.epa.gov/eco-research/level-iii-and-iv-ecoregions-continental-united-states
ecoregions_l4 <- readOGR(file.path(poly_dir,"us_eco_l4_state_boundaries/us_eco_l4.shp"))
ecoregions_l4_nobound <- readOGR(file.path(poly_dir,"us_eco_l4/us_eco_l4_no_st.shp"))
ecoregions_l4.wgs <- spTransform(ecoregions_l4_nobound,wgs.proj)
ecoregions_l4_clean <- ecoregions_l4.wgs
ecoregions_l4_clean <- clgeo_Clean(ecoregions_l4) # takes a long time
ecoregions_l3 <- readOGR(file.path(poly_dir,"us_eco_l3/us_eco_l3.shp"))
ecoregions_l3.wgs <- spTransform(ecoregions_l3,wgs.proj)
ecoregions_l3_clean.wgs <- ecoregions_l3.wgs
#ecoregions_l3_clean.wgs <- clgeo_Clean(ecoregions_l3.wgs)
	## Global (WWF)
		# https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
#ecoregions <- readOGR(file.path(poly_dir,"official/wwf_terr_ecos.shp"))
#ecoregions.wgs <- spTransform(ecoregions,wgs.proj)
#ecoregions.wgs@data$ECO_ID <- as.factor(ecoregions.wgs@data$ECO_ID)

## States
	## U.S. states
		# https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_5m.zip
us_states <- readOGR(file.path(poly_dir,"cb_2018_us_state_5m/cb_2018_us_state_5m.shp"))
target_states <- c("AK","AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID",
  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE",
  "NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
  "TX","UT","VT","WV","WA","VA","WI","WY")
us_states <- us_states[us_states@data$STUSPS %in% target_states,]
		# create polygon for clipping buffers later
us_states.wgs <- spTransform(us_states,wgs.proj)
us_boundary.wgs <- aggregate(us_states.wgs,dissolve = TRUE)
us_states.aea <- spTransform(us_states,aea.proj)
us_boundary.aea <- aggregate(us_states.aea,dissolve = TRUE)

## Countries
	## Global
		# https://opendata.arcgis.com/datasets/252471276c9941729543be8789e06e12_0.zip
world_country_shp <- readOGR(file.path(poly_dir,"UIA_World_Countries_Boundaries-shp/World_Countries__Generalized_.shp"))
		# can filter so only target countries, if desired; speeds things up
sort(unique(world_country_shp@data$ISO))
target_countries <- c("US","CA","MX")
world_country_shp <- world_country_shp[world_country_shp@data$ISO %in% target_countries,]
		# create polygon for clipping buffers later
#world_country.wgs <- spTransform(world_country_shp,wgs.proj)
#world_boundary.wgs <- aggregate(world_country.wgs,dissolve = TRUE)
#world_country.aea <- spTransform(world_country_shp,aea.proj)
#world_boundary.aea <- aggregate(world_country.aea,dissolve = TRUE)

### CREATE COLOR PALETTES / MAP ICONS

# Ecoregions polygons
	# change inputs to "ecoregions.wgs@data$ECO_ID" if using global ecoregions
		# U.S. LEVEL 3
eco_pal_colors <- createPalette(length(unique(ecoregions_l3_clean.wgs@data$NA_L3CODE)),
	seedcolors = c("#ba3c3c","#ba7d3c","#baab3c","#3ca7ba","#3c6aba","#573cba","#943cba","#ba3ca1","#ba3c55"),
	range = c(5,42), target = "normal", M=50000)
swatch(eco_pal_colors)
eco_pal_colors <- as.vector(eco_pal_colors)
eco_pal <- colorFactor(eco_pal_colors,ecoregions_l3_clean.wgs@data$NA_L3CODE)
		# U.S. LEVEL 4
eco_pal_colors_l4 <- createPalette(length(unique(ecoregions_l4.wgs@data$US_L4CODE)),
	seedcolors = c("#ba3c3c","#ba7d3c","#baab3c","#3ca7ba","#3c6aba","#573cba","#943cba","#ba3ca1","#ba3c55"),
	range = c(5,42), target = "normal", M=50000)
swatch(eco_pal_colors_l4)
eco_pal_colors_l4 <- as.vector(eco_pal_colors_l4)
eco_pal_l4 <- colorFactor(eco_pal_colors_l4,ecoregions_l4.wgs@data$US_L4CODE)

# Ex situ point data triangle icons
triangle_sm <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
 	iconWidth = 8, iconHeight = 8)
triangle_md <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
 	iconWidth = 15, iconHeight = 15)
triangle_lg <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
 	iconWidth = 22, iconHeight = 22)

################################################################################
## Calculate geographic and ecological coverage of ex situ collections
################################################################################

### CREATE LIST OF TARGET SPECIES

target_sp <- c(
  #"Carya_floridana","Carya_myristiciformis"#,
	#"Fagus_grandifolia"#,
  #"Gymnocladus_dioicus"#,
  #"Juglans_californica","Juglans_cinerea","Juglans_hindsii",
  #  "Juglans_major","Juglans_microcarpa","Juglans_nigra"#,
  #"Lindera_benzoin","Persea_borbonia","Persea_humilis","Persea_palustris",
  #  "Sassafras_albidum"#,
  "Pinus_albicaulis","Pinus_balfouriana","Pinus_coulteri","Pinus_flexilis",
    "Pinus_lambertiana","Pinus_monticola","Pinus_muricata","Pinus_palustris",
    "Pinus_ponderosa","Pinus_radiata","Pinus_strobiformis","Pinus_torreyana"#,
  #"Taxus_brevifolia","Taxus_canadensis","Taxus_floridana"
)
#sp <- 10

### START SUMMARY TABLE

# we add each target species as we go along
summary_tbl <- data.frame(species = "start",
	geo_20 = "start", eco_20 = "start",
	geo_50 = "start", eco_50 = "start",
	geo_100 = "start", eco_100 = "start",
	geo_avg = "start", eco_avg = "start", stringsAsFactors=F)

### CYCLE THROUGH TARGET SPECIES TO CALCULATE EX SITU COVERAGE

##
for(sp in 1:length(target_sp)){
##

	# print progress
  cat("\tStarting ", target_sp[sp])

	### READ IN AND PREP POINT DATA

	## in situ and ex situ points (output from 3-1_refine_occurrence_points.R)
	pts <- read.csv(file.path(pts_dir,
		paste0("occurrence_points/outputs/spp_edited_points/",target_sp[sp],".csv")),
		na.strings=c("","NA"), stringsAsFactors = F)
	nrow(pts)
	## clip points by boundary so only in target area
		# select coordinate columns
	latlong <- pts %>% select(decimalLongitude,decimalLatitude)
		# turn occurrence point data into a SpatialPointsDataFrame
	spatial_pts <- SpatialPointsDataFrame(latlong, pts, proj4string = wgs.proj)
		# clip (can change to world_country_boundary, as desired)
	spatial_pts <- spatial_pts[us_boundary.wgs, ]
	insitu <- spatial_pts@data
	nrow(insitu)

	## layer of ex situ points only
	exsitu <- insitu[insitu$database == "Ex_situ", ]
	exsitu$lat_round <- round(as.numeric(exsitu$decimalLatitude),digits=1)
	exsitu$long_round <- round(as.numeric(exsitu$decimalLongitude),digits=1)
	exsitu <- exsitu %>%
			# comment the next line out if column names are prov_type and num_indiv already
		rename(prov_type = basisOfRecord, num_indiv = establishmentMeans) %>%
		filter(prov_type != "H?") %>%
	  group_by(lat_round,long_round) %>%
	  mutate(num_indiv = sum(as.numeric(num_indiv))) %>%
	  ungroup() %>%
	  distinct(lat_round,long_round,.keep_all=T) %>%
		select(-lat_round,-long_round)
	exsitu$num_indiv <- as.numeric(exsitu$num_indiv)
	nrow(exsitu)
	## optionally, remove any bad ex situ points manually by ID number
	if(exsitu$species_name_acc[1] == "Juglans californica"){
		exsitu <- exsitu %>% filter(UID != "id00092049" & UID != "id00091728")
	} else if (exsitu$species_name_acc[1] == "Juglans major"){
		exsitu <- exsitu %>% filter(UID != "id00092280")
	} else if(exsitu$species_name_acc[1] == "Carya myristiciformis"){
		exsitu <- exsitu %>% filter(UID != "id00090650" & UID != "id00091027")
	} else if(exsitu$species_name_acc[1] == "Taxus brevifolia"){
		exsitu <- exsitu %>% filter(UID != "id00090887" & UID != "id00090999")
	} else if(exsitu$species_name_acc[1] == "Taxus floridana"){
		exsitu <- exsitu %>% filter(UID != "id00090410" & UID != "id00090494")
	} else if(exsitu$species_name_acc[1] == "Persea borbonia"){
		exsitu <- exsitu %>% filter(UID != "id00090649")
	} else if(exsitu$species_name_acc[1] == "Gymnocladus dioicus"){
		exsitu <- exsitu %>% filter(UID != "id00091548")
	} else if(exsitu$species_name_acc[1] == "Pinus monticola"){
		exsitu <- exsitu %>% filter(UID != "id00090886")
	} else if(exsitu$species_name_acc[1] == "Pinus torreyana"){
		exsitu <- exsitu %>% filter(UID != "id00090510")
	}
	sum(exsitu$num_indiv)
	as.data.frame(exsitu %>% summarize(prov_type,num_indiv))
	# split by number of individuals, to use different symbol
	exsitu1 <- exsitu %>% arrange(num_indiv) %>%
		filter(num_indiv <= 10)
	exsitu2 <- exsitu %>% arrange(num_indiv) %>%
		filter(num_indiv > 10 & num_indiv < 30)
	exsitu3 <- exsitu %>% arrange(num_indiv) %>%
		filter(num_indiv >= 30)

	## optionally, filter and arrange in situ points
	# set database as factor and order appropriately
	insitu$database <- factor(insitu$database,
  	levels = c("Ex_situ","FIA","GBIF","US_Herbaria","iDigBio","BISON","BIEN"))
	# arrange by database and filter out some flagged points
	insitu <- insitu %>% arrange(desc(database)) %>%
		filter(.cen & .inst & .bonapnative & .yr1950 & .yrna &
			basisOfRecord != "FOSSIL_SPECIMEN" & basisOfRecord != "LIVING_SPECIMEN" &
				basisOfRecord != "H?" &
			establishmentMeans != "INTRODUCED" & establishmentMeans != "MANAGED" &
				establishmentMeans != "INVASIVE")
	nrow(insitu)
	## optionally, remove any bad in situ points manually by ID number
	if(insitu$species_name_acc[1] == "Juglans californica"){
		insitu <- insitu %>% filter(UID != "id00092049" & UID != "id00091728" &
			UID != "id06078450" & UID != "id06026157")
	} else if(insitu$species_name_acc[1] == "Juglans nigra"){
		insitu <- insitu %>% filter(UID != "id06129314")
	} else if(insitu$species_name_acc[1] == "Carya floridana"){
		insitu <- insitu %>% filter(UID != "id06168140")
	} else if(insitu$species_name_acc[1] == "Persea humilis"){
		insitu <- insitu %>% filter(UID != "id06121432")
	} else if(insitu$species_name_acc[1] == "Taxus floridana"){
		insitu <- insitu %>% filter(UID != "id06309741" & UID != "id06130496" & UID != "id06010137")
	} else if(insitu$species_name_acc[1] == "Pinus albicaulis"){
		insitu <- insitu %>% filter(UID != "id06256808")
	} else if(insitu$species_name_acc[1] == "Pinus coulteri"){
		insitu <- insitu %>% filter(UID != "id06096947" & UID != "id06262996" & UID != "id06003152" &
			UID != "id06263103" & UID != "id06183523" & UID != "id06263090" & UID != "id06017682" &
			UID != "id06183860")
	} else if(insitu$species_name_acc[1] == "Pinus flexilis"){
		insitu <- insitu %>% filter(UID != "id06068905")
	} else if(insitu$species_name_acc[1] == "Pinus monticola"){
		insitu <- insitu %>% filter(UID != "id03459557" & UID != "id06277259")
	} else if(insitu$species_name_acc[1] == "Pinus muricata"){
		insitu <- insitu %>% filter(UID != "id00952421" & UID != "id06149674" & UID != "id06161748" &
			UID != "id06062431" & UID != "id06124285" & UID != "id05942324" & UID != "id06173334" &
			UID != "id06277934" &	UID != "id06194568")
	} else if(insitu$species_name_acc[1] == "Pinus palustris"){
		insitu <- insitu %>% filter(UID != "id06074552" & UID != "id05951530" & UID != "id06161182" &
			UID != "id05984243" & UID != "id06065364" & UID != "id06410210" & UID != "id03002952" &
			UID != "id06159681" &	UID != "id02393777")
	#} else if(insitu$species_name_acc[1] == "Pinus ponderosa"){
	#	insitu <- insitu %>% filter(UID != "id06424851" & UID != "id06424853" & UID != "id06058712" &
	#		UID != "id05938491" & UID != "id06174238")
	} else if(insitu$species_name_acc[1] == "Pinus radiata"){ #for this we are selecting points to KEEP
		insitu <- insitu %>% filter(UID == "id06112634" | UID == "id00978725" |
			UID == "id06283286" |	UID == "id00978678" | UID == "id06119492" | UID == "id05945553" |
			UID == "id06008561" | UID == "id06140845" | UID == "id06073053")
	} else if(insitu$species_name_acc[1] == "Pinus torreyana"){ #for this we are selecting points to KEEP
		insitu <- insitu %>% filter(UID == "id05917504" | UID == "id05983808" |
			UID == "id05914895" | UID == "id06310422")
	}
	nrow(insitu)
	exsitu_join <- exsitu %>%
		rename(basisOfRecord = prov_type, establishmentMeans = num_indiv)
	insitu <- rbind(insitu,exsitu_join)

	### CALCULATE EX SITU COVERAGE

	## Geographic coverage
		# 20 km buffers
	geo_20_insitu <- calc.buff.area(insitu,20000,wgs.proj,aea.proj,us_boundary.aea)
	geo_20_exsitu <- calc.buff.area(exsitu,20000,wgs.proj,aea.proj,us_boundary.aea)
	geo_20_percent <- (geo_20_exsitu/geo_20_insitu)*100
		# 50 km buffers
	geo_50_insitu <- calc.buff.area(insitu,50000,wgs.proj,aea.proj,us_boundary.aea)
	geo_50_exsitu <- calc.buff.area(exsitu,50000,wgs.proj,aea.proj,us_boundary.aea)
	geo_50_percent <- (geo_50_exsitu/geo_50_insitu)*100
		# 100 km buffers
	geo_100_insitu <- calc.buff.area(insitu,100000,wgs.proj,aea.proj,us_boundary.aea)
	geo_100_exsitu <- calc.buff.area(exsitu,100000,wgs.proj,aea.proj,us_boundary.aea)
	geo_100_percent <- (geo_100_exsitu/geo_100_insitu)*100

	## Ecological coverage (use count.eco.wwf function if using global ecoregions)
		# 20 km buffers
	eco_20_insitu <- count.eco.usl4(insitu,20000,wgs.proj,aea.proj,ecoregions_l4_clean,us_boundary.aea)
	eco_20_exsitu <- count.eco.usl4(exsitu,20000,wgs.proj,aea.proj,ecoregions_l4_clean,us_boundary.aea)
	eco_20_percent <- (eco_20_exsitu/eco_20_insitu)*100
		# 50 km buffers
	eco_50_insitu <- count.eco.usl4(insitu,50000,wgs.proj,aea.proj,ecoregions_l4_clean,us_boundary.aea)
	eco_50_exsitu <- count.eco.usl4(exsitu,50000,wgs.proj,aea.proj,ecoregions_l4_clean,us_boundary.aea)
	eco_50_percent <- (eco_50_exsitu/eco_50_insitu)*100
		# 100 km buffers
	eco_100_insitu <- count.eco.usl4(insitu,100000,wgs.proj,aea.proj,ecoregions_l4_clean,us_boundary.aea)
	eco_100_exsitu <- count.eco.usl4(exsitu,100000,wgs.proj,aea.proj,ecoregions_l4_clean,us_boundary.aea)
	eco_100_percent <- (eco_100_exsitu/eco_100_insitu)*100

	## Create text for results table
	geo_20_txt <- format.cell(geo_20_exsitu,geo_20_insitu,geo_20_percent)
	geo_50_txt <- format.cell(geo_50_exsitu,geo_50_insitu,geo_50_percent)
	geo_100_txt <- format.cell(geo_100_exsitu,geo_100_insitu,geo_100_percent)
	geo_avg_txt <- paste0(round(mean(c(geo_20_percent,geo_50_percent,geo_100_percent)),0),"%")
	eco_20_txt <- format.cell(eco_20_exsitu,eco_20_insitu,eco_20_percent)
	eco_50_txt <- format.cell(eco_50_exsitu,eco_50_insitu,eco_50_percent)
	eco_100_txt <- format.cell(eco_100_exsitu,eco_100_insitu,eco_100_percent)
	eco_avg_txt <- paste0(round(mean(c(eco_20_percent,eco_50_percent,eco_100_percent)),0),"%")

  ## Add text results to summary table
  summary_add <- data.frame(species = target_sp[sp],
			geo_20 = geo_20_txt, eco_20 = eco_20_txt,
			geo_50 = geo_50_txt, eco_50 = eco_50_txt,
			geo_100 = geo_100_txt, eco_100 = eco_100_txt,
			geo_avg = geo_avg_txt, eco_avg = eco_avg_txt, stringsAsFactors=F)
  summary_tbl[sp,] <- summary_add

	### CREATE MAP

	# create buffers for visualization (can change to world_country.wgs boundary as needed)
	insitu_buff_wgs <- create.buffers(insitu,20000,wgs.proj,wgs.proj,us_boundary.wgs)
	exsitu_buff_wgs <- create.buffers(exsitu,20000,wgs.proj,wgs.proj,us_boundary.wgs)

	# map everthing!
		# can turn layers on or off, or switch them for other polygons, as desired
  map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
    ## Base layer
    addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
		## Species name label
		addControl(paste0("<b>",gsub("_"," ",target_sp[sp])), position = "topright") %>%
		## World countries for background
		#addPolygons(data = world_country_shp,
		#	fillColor = "#ccc1ab", fillOpacity = 0.4,
		#	weight = 2, opacity = 0, color = "#ccc1ab") %>% ##d4d4d4 ##b5b5b5
		## Ecoregions
		addPolygons(
				# EPA Level III ecoregions
			#data = ecoregions_l3_clean.wgs,
			#fillColor = ~eco_pal(ecoregions_l3_clean.wgs@data$NA_L3CODE),
				# EPA Level IV ecoregions
			data = ecoregions_l4.wgs,
			fillColor = ~eco_pal_l4(ecoregions_l4.wgs@data$US_L4CODE),
			fillOpacity = 0.9, color = "#757575", weight = 1.5, opacity = 0.8) %>%
		## U.S. states outline
		addPolygons(data = us_states.wgs, fillColor = "transparent",
			weight = 1.5, opacity = 0.5, color = "black") %>%
		## Buffers
			# In situ
		addPolygons(data = insitu_buff_wgs,
			fillColor = "#a3a3a3", fillOpacity = 0.45,
			weight = 1.3, opacity = 0.9, color = "#c4c4c4",
			smoothFactor = 0) %>%
			# Ex situ
		addPolygons(data = exsitu_buff_wgs,
			fillColor = "white", fillOpacity = 0.55,
			weight = 1.3, color = "white", opacity = 0,
			smoothFactor = 0) %>%
		## Ex situ points
    addMarkers(data = exsitu1,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_sm, popup = exsitu1$UID) %>%
    addMarkers(data = exsitu2,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_md, popup = exsitu2$UID) %>%
    addMarkers(data = exsitu3,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_lg, popup = exsitu3$UID) %>%
		addScaleBar(position = "bottomright",
			options = scaleBarOptions(maxWidth = 150)) %>%
		setView(-98, 40, zoom = 5)
  map
	### can use "COMMAND+" mult. (I did five) times to make scale bar larger

  # save map
		# looks like these maps are too big to save? works best to view
		#		one-by-one in browser straight from R and take screenshot
  #htmlwidgets::saveWidget(map, file.path(output_dir,paste0(target_sp[sp],"_leaflet_map.html")))

	# print progress
  cat("\tEnding ", target_sp[sp], ", ", sp, " of ", length(target_sp), ".\n\n", sep="")

}

# write summary table
summary_tbl
write.csv(summary_tbl, file.path(output_dir,"ExSituCoverage_Table.csv"),row.names = F)























################################################################################
## Other old code bits
################################################################################

	# for creating legend
  map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
		## World countries for background
		addPolygons(data = world_country_shp,
			fillColor = "#695341", fillOpacity = 0.8,
			weight = 0) %>%
		## Buffers
		# In situ
		#addPolygons(data = insitu_buff_clip,
		#	fillColor = "#a3a3a3", fillOpacity = 0.45,
		#	weight = 1.3, opacity = 0.9, color = "#c4c4c4",
		#	smoothFactor = 0) %>%
		# Ex situ
		addPolygons(data = exsitu_buff_clip,
			fillColor = "white", fillOpacity = 0.55,
			weight = 1.3, color = "white", opacity = 0,
			smoothFactor = 0) %>%
		setView(-98, 40, zoom = 5)
	map



#devtools::install_github("statnmap/HatchedPolygons")
#library(HatchedPolygons)
#exsitu_buff_hatch <- HatchedPolygons::hatched.SpatialPolygons(exsitu_buff_clip, density = c(6,4), angle = c(45, 135))

		# RED AND BLUE SIMPLE MAP

  map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
    ## Base layer
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>% #
		## Species name label
		addControl(paste0("<b>",target_sp[sp]), position = "topright") %>%
		## U.S. states outline
		addPolygons(data = us_states, fillColor = "transparent",
			weight = 1.5, opacity = 0.7, color = "grey") %>%
		## Buffers
		addPolygons(data = insitu_buff_clip,
			fillColor = "red", fillOpacity = 0.25,
			weight = 1.5, opacity = 0.6, color = "red", smoothFactor = 0.5) %>%
		addPolygons(data = exsitu_buff_clip,
			fillColor = "blue", fillOpacity = 0.25,
			weight = 1.5, opacity = 0.6, color = "blue", smoothFactor = 0.5) %>%
		## Ex situ
    addMarkers(data = exsitu1,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_sm) %>%
    addMarkers(data = exsitu2,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_md) %>%
    addMarkers(data = exsitu3,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_lg)
  map

		# RASTER DISTRIBUTION MAP

  map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
    ## Base layer
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
		## Species name label
		addControl(paste0("<b>",target_sp[sp]), position = "topright") %>%
		## Raster distribution
		addRasterImage(distribution_agg_clip, opacity = 1,
			colors = dist_pal) %>%
		## U.S. states outline
		addPolygons(data = us_states, fillColor = "transparent",
			weight = 1.5, opacity = 0.7, color = "grey") %>%
		## Buffers
		addPolygons(data = insitu_buff_clip,
			fillColor = "red", fillOpacity = 0.2,
			weight = 1.5, opacity = 0.6, color = "red", smoothFactor = 0.5) %>%
		addPolygons(data = exsitu_buff_clip,
			fillColor = "blue", fillOpacity = 0.2,
			weight = 1.5, opacity = 0.6, color = "blue", smoothFactor = 0.5) %>%
		## Ex situ
    addMarkers(data = exsitu1,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_sm) %>%
    addMarkers(data = exsitu2,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_md) %>%
    addMarkers(data = exsitu3,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_lg) %>%
		addLegend(pal = dist_pal, values = values(distribution_agg_clip),
    	opacity = 1, title = "Modeled live basal area<br/>(Wilson et al., 2013)",
			position = "bottomright")
  map



  # create map
  map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
    ## Base layer
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>% #Esri.WorldGrayCanvas
		## Species name label
		addControl(paste0("<b>",target_sp[sp]), position = "topright") %>%
		## Raster distribution
		#addRasterImage(distribution_agg_clip, opacity = 1, colors = dist_pal,
		#	group = "Modeled live basal area (Wilson et al., 2013)") %>%
    ## In situ
    #addCircleMarkers(data = insitu,
		#	lng = ~decimalLongitude, lat = ~decimalLatitude,
    #  color = "#508757", radius = 4, fillOpacity = 0.5, stroke = F,
		#	group = "In situ occurrence points") %>%
		## EPA Level III ecoregions
		addPolygons(data = ecoregions_l3_clean.wgs,
			fillColor = ~eco_pal,#(ecoregions_l3_clean.wgs@data$Shape_Area),
			fillOpacity = 0.7, weight = 1, opacity = 0.4, color = "black",
			group = "EPA Level III Ecoregions") %>%
		## U.S. states outline
		addPolygons(data = us_states, fillColor = "transparent",
			weight = 1.5, opacity = 0.4, color = "black",
			group = "U.S. state outlines") %>%
		## Buffers
		addPolygons(data = insitu_buff_clip, fillColor = "white",
			fillOpacity = 0.6,
			weight = 1.5, opacity = 0.8, color = "white", smoothFactor = 0.5,
			group = "In situ buffers") %>%
		addPolygons(data = exsitu_buff_clip, fillColor = "grey",
			fillOpacity = 0.6,
			weight = 1.5, opacity = 0.8, color = "grey", smoothFactor = 0.5,
			group = "Ex situ buffers") %>%
		## Ex situ
    addMarkers(data = exsitu,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle) %>%
			#maxZoom = 10
			#radius = 6, fill = T, fillOpacity = 1,
			#fillColor = ~pal_exsitu(exsitu1@data$establishmentMeans),
			#stroke = T, color = "black", weight = 1, opacity = 1)
		## Layers control
    addLayersControl(
      overlayGroups = c("Modeled live basal area (Wilson et al., 2013)",
												#"In situ occurrence points",
												"U.S. state outlines",
												"EPA Level III Ecoregions",
												"In situ buffers",
												"Ex situ buffers"),
      options = layersControlOptions(collapsed = FALSE))# %>%
    #hideGroup("In situ occurrence points") %>%
		#addLegend(pal = dist_pal, values = values(distribution_agg_clip),
    #	opacity = 1, title = "Live basal area", position = "bottomright")
		#addControl(html = html_legend, position = "bottomright")
  map

}



pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(distribution_agg),
  na.color = "transparent")

	leaflet() %>%
		## background
		addProviderTiles("Esri.WorldGrayCanvas",
			options = providerTileOptions(maxZoom = 10)) %>%
		#addPolygons(data = ecoregions_l3.wgs)
		## insitu points
		addCircleMarkers(data = spatial_pts,
			radius = 3, fillOpacity = 1, stroke = F, color = "black")
		## exsitu points
		addCircleMarkers(data = exsitu, lng = ~decimalLongitude, lat = ~decimalLatitude,
			radius = 3, fillOpacity = 1, stroke = F, color = "white") %>%



	### RASTER DATA

	## Species native distribution
		# https://www.fs.usda.gov/rds/archive/catalog/RDS-2013-0013
		# raster of modeled species distribution from Wilson et al. 2013
	distribution <- raster(file.path(poly_dir,
		paste0("Wilson_2013_rasters/s",target_sp_codes[sp],".img")))
		# aggregate so its not as big and detailed
	distribution_agg <- aggregate(distribution, fact=20, fun=mean, dissolve = T)
		# remove lowest values (they cover the whole modeled area)
	#distribution_agg[distribution_agg < 0.01] <- NA
		# reproject
	distribution_agg <- projectRaster(distribution_agg, crs = wgs.proj)
		# crop
	distribution_agg_clip <- crop(distribution_agg, us_boundary)
		# mask the cells that do not overlap a polygon
	distribution_agg_clip <- rasterize(us_boundary, distribution_agg_clip,
		mask=TRUE)
		# create palette
	dist_pal <- colorNumeric(
	c("transparent","#bddb95","#a4db7f","#80db67","#4cc246","#2aa33b","#138a3b","#077036"),#,"#02522f"
		values(distribution_agg_clip),na.color = "transparent")
