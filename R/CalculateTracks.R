#_______________________________________________________________________________________________________________
##
##      GOAL OF THE FUNCTION: Prepare data to calculate the home ranges afterwards
##
##
##      CREDIT: Gaëlle Picon (picon.gaelle@gmail.com), from Akshay Bharadwaj 2024 scripts
##
#_______________________________________________________________________________________________________________

calculate_tracks <- function(
  region,
  species,
  files2prepare,
  data_path,
  input_dir = NULL,
  output_dir = NULL
) {
  message("Initializing")

  # Set input directory
  if (is.null(input_dir)) {
    input_dir <- here::here(
      "data",
      species,
      region,
      "/"
    )
  }

  # Set output directory
  if (is.null(output_dir)) {
    output_dir <- here::here(
      "outputs",
      species,
      region,
      "individual_yearly_tracks/"
    )
  }

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create an empty list to accumulate data frames
  data_list <- list()

  message(paste("Loading data from ", length(files2prepare), "files"))

  # Loop on all individuals
  for (i in 1:length(files2prepare)) {
    # Importing the csv and adding it to the list

    data_ind <- read.csv(file = paste0(input_dir, files2prepare[i]))
    data_list[[i]] <- data_ind
  }

  # Combine all data frames at once for efficiency
  data <- dplyr::bind_rows(data_list)

  # Filter out data with timestamp format "YYYY-MM-DD" (= 10 characters) to keep only those with "YYYY-MM-DD HH-MM-SS" format.
  # Then, keep only useful columns and create a "year" column
  message("Cleaning data")

  data <- data %>%
    dplyr::filter(nchar(as.character(timestamp)) > 10) %>%
    dplyr::select(-c(1, 11, 14:19, 21:40, 43, 45:61)) %>%
    dplyr::mutate(
      UTC_datetime = lubridate::ymd_hms(timestamp),
      year = lubridate::year(UTC_datetime)
    )

  # Inspect NAs
  nb_nas <- sum(is.na(data$location.long) | is.na(data$location.lat))
  message("Found ", nb_nas, "rows with NA in coordinates")

  # Remove lines with NAs in coordinates
  data_clean <- data %>%
    dplyr::filter(
      !is.na(location.long) &
        !is.na(location.lat)
    )

  message("Converting to RGF93")

  # Transform into RGF93:
  # Convert into a spatial object
  data_sf <- sf::st_as_sf(
    data_clean,
    coords = c("location.long", "location.lat"),
    crs = 4326,
    remove = TRUE
  )

  # Change CRS from WGS84 (EPSG 4326) to Lambert 93 (EPSG 2154)
  data_sf <- sf::st_transform(data_sf, crs = 2154)

  # Create a table with the coordinates only
  coordinates_RGF93 <- as.data.frame(st_coordinates(data_sf))

  # Extracting the Long and Lat coordinates in ESPG 2154 for later use
  X_93 <- coordinates_RGF93$X
  Y_93 <- coordinates_RGF93$Y
  data <- cbind.data.frame(data_clean, X_93, Y_93)

  message("Export filtered data")

  write.csv(
    data,
    file.path(
      output_dir,
      paste0("data_adults_immat", region, "_filtered.csv")
    ),
    row.names = FALSE
  )

  # Remove NAs
  data <- data %>%
    dplyr::filter(!is.na(UTC_datetime))

  message("Create tracks for all dataset")

  # Validate required columns and remove rows with NAs in key columns
  required_cols <- c("X_93", "Y_93", "UTC_datetime")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  data <- data %>%
    dplyr::filter(!is.na(X_93) & !is.na(Y_93) & !is.na(UTC_datetime))

  # Create an amt object for the segclust and KDE calculations later on
  locations <- amt::make_track(
    data,
    X_93,
    Y_93,
    UTC_datetime,
    bird_id = individual.local.identifier,
    crs = 2154
  )

  message("Export all tracks")

  write.csv(
    locations,
    paste0(
      output_dir,
      "data_ad_imm_",
      region,
      "_tracks.csv"
    )
  )

  message("Create individual tracks")

  # Create tracks individual by individual
  individuals <- unique(locations$bird_id)

  for (id in individuals) {
    # Filter data for this individual
    locs_id <- locations %>% filter(bird_id == id)

    # Extract the individual's name (here, located after the first 18 characters)
    ind_name <- substr(id, 19, nchar(id))

    # Extract the year of the data
    years <- unique(lubridate::year(locs_id$t_))

    # Save one file per individual per year
    for (yr in years) {
      locs_year <- locs_id %>% dplyr::filter(lubridate::year(t_) == yr)

      message("Export individual tracks")

      # Ensure the subdirectory exists before saving
      subdir <- file.path(output_dir, "individual_yearly_tracks")
      if (!dir.exists(subdir)) {
        dir.create(subdir, recursive = TRUE)
      }

      # Create a file name with the name and the year
      filename <- file.path(
        subdir,
        paste0(ind_name, "_", yr, "_tracks.csv")
      )

      # Save file
      write.csv(locs_year, filename, row.names = FALSE)
    }
  }
}
