#_______________________________________________________________________________
##
##      SCRIPT 1: DOWNLOADING AND SUBSAMPLING DATA
##
##
##      CREDIT: Gaëlle Picon (picon.gaelle@gmail.com)
##
#_______________________________________________________________________________

rm(list = ls()) # Free memory space (resets session by removing all
#                 existing objects)

#___________________________________________________________________
#
#### Extract data from Movebank ####
#___________________________________________________________________

# Specify the species of interest (GF = Griffon vulture, LK = Lesser kestrel)
species = "GF"

# Specify the region of interest (CAU = Causses, ALP = Alps, ...)
region = "CAU"

# Set raw data directory
data_path <- file.path("data", species, region)

# Parameters: 1. username and password, 2. name of Movebank study, 3. years and months that will be downloaded
loginStored <- move::movebankLogin(
  username = "duriez",
  password = "Kerguelen47!"
)

studyName <- "Eurasian griffon vulture in France ID_PROG 961"

year <- c(
  "2010",
  "2011",
  "2012",
  "2013",
  "2014",
  "2015",
  "2016",
  "2017",
  "2018",
  "2019",
  "2020",
  "2021",
  "2022",
  "2023",
  "2024",
  "2025"
)

month <- c(
  "01",
  "02",
  "03",
  "04",
  "05",
  "06",
  "07",
  "08",
  "09",
  "10",
  "11",
  "12"
)

# Run the function that downloads adult and immature birds data from movebank and resamples to 1 location / hour
downloaded_birds <- extract_movebank_ad_imm(
  loginStored = loginStored,
  studyName = studyName,
  year = year,
  month = month,
  region = "CAU"
)

print(downloaded_birds)
