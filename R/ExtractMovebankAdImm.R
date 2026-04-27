#_______________________________________________________________________________________________________________
##
##      GOAL OF THE FUNCTION: Extract locations from adult and immature birds tagged in France & stored in Movebank.
##                            + resample to one location per hour
##
##
##      CREDIT: Yohan Sassi (yoh.sassi22@gmail.com). Adapted into function
#       by Gaëlle Picon (picon.gaelle@gmail.com)
##
#_______________________________________________________________________________________________________________

extract_movebank_ad_imm <- function(
  loginStored,
  studyName,
  year,
  month,
  region,
  species,
  output_dir = NULL
) {
  library(dplyr) # Data manipulation
  library(here) # To deal with directories properly
  library(lubridate) # Working with date/time
  library(stringr) # String manipulation
  library(move2) # Movebank API + animal movement data
  library(move) # Movebank API + animal movement data
  library(sf) # Modern spatial package (replaces sp, rgdal, rgeos)
  library(raster) # Raster/grid data

  # Set default output directory
  if (is.null(output_dir)) {
    output_dir <- here::here("data", species, region, "/")
  }

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Get all individual IDs for this study and pull all the identifiers
  indNames <- as.data.frame(getMovebankAnimals(studyName, loginStored)) |>
    dplyr::pull(local_identifier)

  # Filter out individuals that are not from the region of interest
  indNames_regional <- indNames[substr(indNames, 4, 6) == region]

  # We also keep only adults and immature birds
  indNames_regional_ADU_IMM <- indNames_regional[
    substr(indNames_regional, 13, 15) == "ADU" |
      substr(indNames_regional, 13, 15) == "IMM"
  ]

  message(paste(
    "Found",
    length(indNames_regional_ADU_IMM),
    "individuals to process"
  ))

  # Keep track of the individuals successfully downloaded and the ones that failed
  successful <- character()
  failed <- character()

  # Loop over individuals
  for (i in 1:length(indNames_regional_ADU_IMM)) {
    indName <- indNames_regional_ADU_IMM[i]
    message(paste("Processing", i))

    #------------------------------------------------
    # Download data from Movebank for each individual
    #------------------------------------------------
    indData <- try(
      getMovebankLocationData(
        study = studyName,
        sensorID = "GPS",
        login = loginStored,
        animalName = indName,
        timestamp_start = paste0(year[1], month[1], "01000000000"),
        timestamp_end = paste0(
          year[length(year)],
          month[length(month)],
          "31235959000"
        )
      ),
      silent = TRUE
    )

    # Skip if the function failed i.e. no recordings for the individual for the year of interest
    if (inherits(indData, "try-error") || nrow(indData) == 0) {
      message("No data for ", indNames[i], " — skipping.")
      failed <- c(failed, indName)
      next
    }

    #----------------------------------------
    # Clean data: keep only visible (non-filtered) locations
    #----------------------------------------
    indData <- indData[indData$visible == "true", ]

    #----------------------------------------
    # Convert coordinates to UTM
    #----------------------------------------

    # indData_utm <- df_to_spatial_df_utm(
    #   dataframe = indData,
    #   column_longitude = "location.long",
    #   column_latitude = "location.lat",
    #   utm_zone = 31,
    #   hemisphere = "N"
    # )

    # Convert sf object back to data.frame if you need a standard format
    # indData_utm_df <- indData_utm %>% st_drop_geometry()

    #----------------------------------------
    # Resample to one location per hour
    #----------------------------------------
    indData <- indData %>%
      mutate(timestamp = as.POSIXct(timestamp, tz = "UTC")) %>%
      mutate(hour_group = floor_date(timestamp, unit = "hour")) %>%
      group_by(hour_group) %>%
      dplyr::slice(1) %>% # Keep first point in each hour
      ungroup() %>%
      dplyr::select(-hour_group)

    message(paste(
      "Resampled to",
      nrow(indData),
      "hourly locations for",
      indName
    ))

    #----------------------------------------
    # Save the cleaned data
    #----------------------------------------

    write.csv(
      indData,
      file = paste0(output_dir, indName, ".csv"),
      row.names = FALSE
    )

    successful <- c(successful, indName)

    #----------------------------------------
    # Free memory
    #----------------------------------------
    rm(indData)
    gc()
  }

  # Return summary
  return(list(
    successful = successful,
    failed = failed,
    n_successful = length(successful),
    n_failed = length(failed)
  ))
}
