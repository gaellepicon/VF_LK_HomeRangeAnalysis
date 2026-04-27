#_______________________________________________________________________________
##
##      SCRIPT 2: CALCULATING HOME RANGES
##      RUN "ANALYSES/1.DOWNLOAD_SAMPLE_DATA.R" BEFORE RUNNING THIS ONE
##
##      CREDIT: Gaëlle Picon (picon.gaelle@gmail.com)
##
#_______________________________________________________________________________

rm(list = ls()) # Free memory space (resets session by removing all
#                 existing objects)

#_______________________________________________________________________________
#
#### Calculate population home ranges (HRs):
####
#### 3 STEPS:
#### 1. Calculate tracks from raw data (R/CalculateTracks.R)
####
#### 2. Identify residential / exploratory periods:
####    - Option 1: With segclust2d on birds previously identified as individuals
####                performing long trips in analyses/visualize_long_trips.R.
####                Then: R/CalculateSegClusts.R
####    - Option 2: With NSD / Dx3 / Dcentroid thresholds. Calculate the variables
####                (R/Calculate_NSD_Dx3_Dcentroid.R), then identify and plot the
####                thresholds you want (analyses/plot_NSD_Dx3_Dcentroid.R) and
####                finally label your tracking data with resident / exploratory
####                (R/LabelResidentialExploratoryPeriods.R)
####
#### 3. Calculate home ranges:
####    - Option 1: for all years combined (R/CalculateAllYearsHR.R)
####    - Option 2: for each year separately (R/CalculateYearlyHR.R)
####
#_______________________________________________________________________________

# Set the species of interest (GF = Griffon Vulture, LK = Lesser Kestrel)
species = "GF"

# Set the region of interest (CAU = Causses, ALP = Alps, PYR = Pyrenees)
region = "CAU"

# Set raw data directory
data_path <- file.path(here::here("data", species, region))

# Load raw data data
files2prepare = list.files(path = data_path, pattern = "\\.csv$")

################################################################
#### ---- CREATE TRACKS FROM RAW DATA ---- ####
################################################################

tracks_calculation <- calculate_tracks(
  region = region,
  species = species,
  files2prepare = files2prepare,
  data_path = data_path,
  input_dir = NULL,
  output_dir = NULL
)

################################################################
#### ----- IDENTIFY RESIDENTIAL / EXPLORATORY PERIODS ----- ####
################################################################

# Option 1: With segclust2d package ----
# You can clusterize the location to identify resident / exploratory locations.
# But it makes sense only on birds that effectively did some long trips. For the
# resident ones, segclust2d will try to find 2 (or more) clusters, whereas there is
# only a resident behaviour.
#
# In analyses/visualize_ehrm.R, I identified, for each year, which individuals
# crossed some north/south/east/west thresholds, i.e. performed long trips. Their names are
# listed in long_trip_birds.csv. Here I extract their names so that I can run segclust only
# on them.
long_trip_summary <- read.csv(
  here::here(
    "outputs",
    species,
    region,
    "EHRMs_lat_long_manual_thresholds",
    "long_trip_individuals.csv"
  )
)
long_trip_summary <- long_trip_summary |>
  dplyr::filter(long_trip == TRUE)


# Extract the ID and year, to match the writing of their files in
# "individual_yearly_tracks_resampled" folder.
long_trip_patterns <- long_trip_summary |>
  dplyr::mutate(
    short_id = sub(".*_([^_]+_[^_]+)$", "\\1", individual),
    pattern = paste0(short_id, "_", year)
  ) |>
  dplyr::distinct(pattern) |>
  dplyr::pull(pattern)

# Run the segmentation-clustering function
seg_clusts_calculation <- calculate_seg_clusts(
  region = region,
  species = species,
  data_path = data_path,
  test_individuals = long_trip_patterns, # Character vector of individual IDs you want to process;
  # set to NULL if you want to run on all individuals
  kmax = 20, # Maximum number of segments to test
  lmin = 96, # Minimum segment length (in number of fixes)
  nclust = 2, # Nb of cluster that segclust will calculate
  input_dir = NULL,
  output_dir = NULL
)

# OPTION 2: With NSD / Dx3 / Dcentroid thresholds ----
# - NSD: net square displacement between the location and a starting point. Here, for each bird,
#  the starting point will be calculated as the mean location of the first week of data.
# - Dx3: distance between the location and the location 3 days earlier.
# - Dcentroid: distance between noon locations and the centroid of all locations

# Step 1: calculate variables
nsd_dx3_dcentroid_calculation <- calculate_nsd_dx3_dcentroid(
  region = region,
  species = species,
  data_path = data_path,
  input_dir = NULL,
  output_dir = NULL
)

# Step 2: find and plot thresholds to identify exploratory periods
# Run analyses/plot_NSD_Dx3_Dcentroid.R

# Step 3: label the residential and exploratory periods
# Based on the thresholds you identified, label your data with resident / exploratory
# Set the thresholds before running the function
residential_exploratory_periods_label <- label_residential_exploratory(
  region = region,
  species = species,
  data_path = data_path,
  input_dir = NULL,
  output_dir = NULL,
  nsd_max_threshold = NULL,
  dx3_max_threshold = 96,
  dcentroid_max_threshold = 86
)

################################################################
#### ---- POPULATION HOME RANGE CALCULATION ---- ####
################################################################

# 2 options: 1. Calculate the population home range for all years combined
#            2. Calculate the population home range for each year separately

# OPTION 1: ALL YEARS TOGETHER ----
# 1. Calculate home ranges from the tracks with only resident locations for all years cumulated
# - In birds2remove, set the full name or a part of the name of the birds you don't want to process
#   (e.g. because they were tagged in your study region but breed in another region).
#   If you want to process all the birds, set to NULL.
# - In subset_prop, keep NULL if you want to process all the birds or set a proportion to subset
#   your data (e.g. 0.1 for 10%)
# - In "method":
#   -> set "all_data" if you want to calculate the home range based on all the tracking data
#   of all birds at once
#   -> set "indiv_union" if you want to calculate it as a merge of all the
#   individual home ranges independently. Don't forget the "" when setting this.

hr_calculation_all_years <- calculate_all_years_hr(
  region = region,
  species = species,
  input_dir = NULL,
  output_dir = NULL,
  bird2remove = c("EAN_Clarence", "KAR_Amel"),
  subset_prop = NULL,
  method = "all_data"
)


# OPTION 2: EACH YEAR SEPARATELY ----
# 2. Calculate home ranges from the tracks with only resident locations year by year
# - In birds2remove, set the full name or a part of the name of the birds you don't want to process
#   (e.g. because they were tagged in your study region but breed in another region).
#   If you want to process all the birds, set to NULL.
# - In subset_prop, keep NULL if you want to process all the birds or set a proportion to subset
#   your data (e.g. 0.1 for 10%).
# - In "method":
#   -> set "all_data" if you want to calculate the home range based on all the tracking data
#   of all birds at once
#   -> set "indiv_union" if you want to calculate it as a merge of all the
#   individual home ranges independently. Don't forget the "" when setting this.

hr_calculation_yearly <- calculate_yearly_hr(
  region = region,
  species = species,
  input_dir = NULL,
  output_dir = NULL,
  bird2remove = c("EAN_Clarence", "KAR_Amel"),
  subset_prop = NULL,
  method = "indiv_union"
)
