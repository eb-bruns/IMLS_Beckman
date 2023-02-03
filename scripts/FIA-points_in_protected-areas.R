################################################################################

### FIA-points_in_protected-areas.R

### Author: Emily Beckman Bruns
### Creation date: 2 February 2023
### Funding: NSF ABI grant #1759759; PI Sean Hoban, The Morton Arboretum

### DESCRIPTION:
# This script downloads USFS Forest Inventory and Analysis (FIA) point data 
# for species that are threatened on the IUCN Red List, then calculates 
# the proportion of records that are present in protected areas 

### INPUTS:
# PROTECTED AREAS (rasterized version)
# 	World Database on Protected Areas (WDPA)
#		https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XIV9BL
# Citation:
#   Sosa Arango, Chrystian Camilo, 2020, "Protected areas (WDPA)", https://doi.org/10.7910/DVN/XIV9BL, Harvard Dataverse, V1

### OUTPUTS:
# CSV with results from analysis

################################################################################
# Load libraries
################################################################################

library("tidyverse")
library("raster")

################################################################################
# Set working directories
################################################################################

# directory that has the protected areas layer
pa_dir <- "/Volumes/GoogleDrive-103729429307302508433/.shortcut-targets-by-id/1S4DfBnSjySYqDIWnUrL9jT86RvwTK1J4/FIA data"

# directory that has the raw downloaded FIA data
fia_dir <- "/Volumes/GoogleDrive-103729429307302508433/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/FIA_data_Nov2022"

# directory where you want the output file
out_dir <- "/Volumes/GoogleDrive-103729429307302508433/.shortcut-targets-by-id/1S4DfBnSjySYqDIWnUrL9jT86RvwTK1J4/FIA data"

################################################################################
# Read in protected areas layer
################################################################################

## read in raster
pa <- raster(file.path(pa_dir,"wdpa_reclass.tif"))

################################################################################
# Extract FIA data for target species
################################################################################

## USDA Forest Service, Forest Inventory and Analysis (FIA)
# data: https://apps.fs.usda.gov/fia/datamart/datamart.html
# metadata doc: https://www.fia.fs.usda.gov/library/database-documentation/current/ver90/FIADB%20User%20Guide%20P2_9-0-1_final.pdf
# Note that as of late 2022 data for most of Alaska is not available, in addition to District of Columbia.
# There is also an rFIA package (released 2020) that could be tried- I think currently it's not working due to the recent FIA 2.0 release.

## I have downloded the FIA data manually (~6.65 GB) to The Morton Arboretum 
# GTCP Shared drive in the "FIA_data_Nov2022" folder
## If you don't have access to these data, you'll need to download from scratch:
# First, download the full FIA dataset via this link (6.65 GB !!)
# https://apps.fs.usda.gov/fia/datamart/CSV/CSV_FIADB_ENTIRE.zip
# this can take a few hours or more if you have a slow connection !
# Now unzip the file and move it where you'd like on your computer
# Next, go to the FIA DataMart and download each state tree file manually:
# https://apps.fs.usda.gov/fia/datamart/datamart.html
# To do this, click the "Select States" dropdown arrow and click "Select all"
# Then, scroll through the list and click (download) every TREE file
# (e.g. AL_TREE.csv)... I know this process sucks. You're not alone.
# If not every state shows in the list, you may have to select them one-by-one
# in the dropdown menu.
# Once you've done this for every state, pat yourself on the back and
# move them to a folder called "FIA_TREE"


# species codes for Threatened species
threated_spp_code <- c(139,232,252,421,541,543,544,545,546,851,16,101,121,124,211,212,548,601,721,811,844,972,6782,7279,6253,52,54,55,92,120,120,251,571,6511,6768,8353,8429,8928)

# species codes for Near Threatened and Data Deficient species
nt_dd_spp_code <- c(451,8455,8492,8916,6241,6251,14,41,104,109,119,201,231,261,262,542,549,604,821,940,990,7280)

# set up list of states and territories to cycle through (57 total)
state_abb <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID",
               "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE",
               "NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
               "TX","UT","VT","WV","WA","VA","WI","WY"
               # turn these off if you don't want territories:
               #,"AS","FM","GU","MP","PW","PR","VI"
)

# read in metadata tables and select relevant columns
# plot data (has lat-long information)
fia_plots <- read.csv(file.path(fia_dir,"CSV_FIADB_ENTIRE/ENTIRE_PLOT.csv"))
fia_plots <- fia_plots %>% dplyr::select(INVYR,STATECD,UNITCD,COUNTYCD,PLOT,LAT,LON)
fia_plots[,1:5] <- lapply(fia_plots[,1:5], function(x) as.integer(as.integer(x)))
fia_plots$LAT <- as.numeric(fia_plots$LAT)
fia_plots$LON <- as.numeric(fia_plots$LON)
# state and county codes and names
county_codes <- read.csv(file.path(fia_dir,"CSV_FIADB_ENTIRE/ENTIRE_COUNTY.csv"),
                         header = T, na.strings=c("","NA"), colClasses="character")
county_codes <- county_codes %>% dplyr::select(STATECD,COUNTYCD,COUNTYNM)
county_codes$STATECD <- as.integer(county_codes$STATECD)
county_codes$COUNTYCD <- as.integer(county_codes$COUNTYCD)

# function to extract target species data from each state CSV,
# via a local copy of the data
extract_tree_data_local <- function(state_abb){
  data <- data.frame()
  # read in tree data, which lists all species and the plots they're in;
  # this takes longer for larger states
  cat("Reading in data for",state_abb)
  state_df <- read.csv(file.path(fia_dir,"FIA_TREE",
                                 paste0(state_abb,"_TREE.csv")))
  # cycle through list of target taxon codes and extract those rows from
  # the state CSV
  for (sp in 1:length(species_codes)){
    target_sp <- state_df[which(state_df$SPCD==species_codes[[sp]]),]
    data <- rbind(data, target_sp)
  }
  # remove state file to make space for reading in the next one
  rm(state_df)
  # take a look at how much data were pulled
  cat("--> ",nrow(data)," observations.<br>")
  # keep only necessary data columns
  data_sm <- data %>% select(SPCD,INVYR,UNITCD,COUNTYCD,PLOT,STATECD,STATUSCD)
  # return reults
  return(data_sm)
  rm(sp)
}

# loop through states and pull data for target species
species_codes <- c(threated_spp_code,nt_dd_spp_code)
fia_outputs <- lapply(state_abb, extract_tree_data_local)
length(fia_outputs)

# stack state-by-state data extracted to create one dataframe
fia_raw <- data.frame()
for(file in seq_along(fia_outputs)){
  fia_raw <- rbind(fia_raw, fia_outputs[[file]])
}; nrow(fia_raw)

# join FIA data to supplemental tables
fia_raw <- left_join(fia_raw,fia_plots)
fia_raw <- left_join(fia_raw,county_codes)
str(fia_raw)

# write file of raw data
write.csv(fia_raw,file.path(out_dir,"fia_data_Threatened-NT-DD.csv"),
          row.names=FALSE)

# read in data if already created
#fia_raw <- read.csv(file.path(out_dir,"fia_data_Threatened-NT-DD.csv"))

################################################################################
# Run protected areas analysis
################################################################################

# set up spatial points object of FIA data
fia_spatial <- fia_raw[which(!is.na(fia_raw$LON) & !is.na(fia_raw$LAT)),]
fia_pts <- SpatialPointsDataFrame(coords = cbind(fia_spatial$LON,
                                                 fia_spatial$LAT),
                                  data = fia_spatial)

# find points that are in protected areas and join to data
pt_in_pa <- raster::extract(pa, fia_pts)
fia_pa <- cbind(pt_in_pa,fia_spatial)

# proportion in protected areas, by species
fia_pa_summary <- fia_pa %>% group_by(SPCD) %>% count(pt_in_pa) %>% ungroup()
  in_pa <- fia_pa_summary[which(fia_pa_summary$pt_in_pa==1),c("SPCD","n")]
    in_pa <- in_pa %>% rename(count_in_PA = n)
  out_pa <- fia_pa_summary[which(is.na(fia_pa_summary$pt_in_pa)),c("SPCD","n")]
    out_pa <- out_pa %>% rename(count_outside_PA = n)
fia_pa_summary_full <- full_join(in_pa,out_pa)
fia_pa_summary_full$proportion_in_PA <- (fia_pa_summary_full$count_in_PA/
  (fia_pa_summary_full$count_in_PA+fia_pa_summary_full$count_outside_PA))
head(fia_pa_summary_full)

# write summary table
write.csv(fia_pa_summary_full, 
          file.path(out_dir,"count_FIA_points_in_protected-areas.csv"), 
                    row.names = F)



