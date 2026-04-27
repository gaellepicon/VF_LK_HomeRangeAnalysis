#_______________________________________________________________________________
##
##      EXTRA SCRIPT: FIRST EXPLORATION OF BIRDS' LONG TRIPS
##      RUN "ANALYSES/1.DOWNLOAD_SAMPLE_DATA.R" BEFORE RUNNING THIS ONE
##
##      CREDIT: Gaëlle Picon (picon.gaelle@gmail.com)
##
#_______________________________________________________________________________

rm(list = ls()) # Free memory space (resets session by removing all
#                 existing objects)

#__________________________________________________________________
#
#### Have a first look at the long trips of the birds ####
#### --> Plot the birds' latitude and longitude over time and
####     see if some seem to perform long trips.
####     Optional: add min / max latitude and longitude thresholds
####     for easier interpertation of the plots
#__________________________________________________________________

#___________________________________________________________________
#
#### Load required packages ####
#___________________________________________________________________

library(here) # To deal with directories properly
library(dplyr) # Data manipulation
library(ggplot2) # For plots
library(lubridate) # Working with date/time
library(scales) # For plot visualization
library(patchwork) # To print several plots together
library(segclust2d) # For segmenting and clustering tracking data


#_______________________________________________________________________
#
#### Plot the birds' long trips with yearly lat / long plots ####
#_______________________________________________________________________

# Specify the species of interest (GF = Griffon vulture, LK = Lesser kestrel)
species = "GF"

# Specify the region of interest (CAU = Causses, ALP = Alps, ...)
region = "CAU"

# Load data
data <- read.csv(here::here(
  "outputs",
  species,
  region,
  paste0("data_adults_", region, "_filtered.csv")
))


# Run the function
lat_long_plots <- ehrm_latlong(
  data = data,
  species = species,
  region = region,
  max_north_lat = 6411673,
  max_south_lat = 6286683,
  max_east_long = 837967,
  max_west_long = 631239,
  quantile_threshold = 0.95,
  output_dir = NULL,
  save_plots = TRUE
)


# Display the plots together
all_long_plots <- wrap_plots(lat_long_plots$plots_longitude, ncol = 3) &
  theme(
    plot.title = element_text(size = 10)
  )

all_lat_plots <- wrap_plots(lat_long_plots$plots_latitude, ncol = 3) &
  theme(
    plot.title = element_text(size = 10)
  )

ggsave(
  here::here(
    "outputs/",
    species,
    region,
    "/EHRMs_lat_long/longitude_plots.png"
  ),
  plot = all_long_plots,
  dpi = 300
)
ggsave(
  here::here("outputs/", species, region, "/EHRMs_lat_long/latitude_plots.png"),
  plot = all_lat_plots,
  dpi = 300
)


#__________________________________________________________________________
#
#### Segment and cluster the locations to detect stationarity / EHRMs ####
#__________________________________________________________________________

# Test with some individuals
# Import tracks of individuals, calculated in 1.download_sample_data.R
data_adults <- read.csv(here::here(
  "outputs",
  species,
  region,
  "data_adults_CAU_tracks.csv"
))
data_adults <- data_adults %>%
  select(-X)

# Keep only some individuals: first, the ones that we know they performed
# EHRMs (thanks to the previous plots)
griffon_2011 <- data_adults %>%
  filter(bird_id == "GF_CAU_2010_ADU_W_BHZ_Griffon" & year(t_) == 2011)

clapas_2012 <- data_adults %>%
  filter(bird_id == "GF_CAU_2009_ADU_W_CIG_Clapas" & year(t_) == 2012)

yaourtiere_2013 <- data_adults %>%
  filter(bird_id == "GF_CAU_2010_ADU_W_FCH_Yaourtiere" & year(t_) == 2013)

kenjutsu_2019 <- data_adults %>%
  filter(bird_id == "GF_CAU_2018_IMM_W_HSA_Kenjutsu" & year(t_) == 2019)


# Now, extract individuals that did not perform EHRM according to the plots
griffon_2012 <- data_adults %>%
  filter(bird_id == "GF_CAU_2010_ADU_W_BHZ_Griffon" & year(t_) == 2012)

zola_2013 <- data_adults %>%
  filter(bird_id == "GF_CAU_2010_ADU_W_CJL_Zola" & year(t_) == 2013)

eire_2013 <- data_adults %>%
  filter(bird_id == "GF_CAU_2013_IMM_W_FCU_Eire" & year(t_) == 2013)


# Run segmentation
griffon_2011_seg <- segmentation(
  griffon_2011,
  seg.var = c("x_", "y_"),
  lmin = 120
)
