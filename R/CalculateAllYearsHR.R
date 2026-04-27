#_______________________________________________________________________________________________________________
##
##      GOAL OF THE FUNCTION: Calculate population home range for all years together
##
##
##      CREDIT: Gaëlle Picon (picon.gaelle@gmail.com)
#_______________________________________________________________________________________________________________

calculate_all_years_hr <- function(
  region,
  species,
  input_dir = NULL,
  output_dir = NULL,
  bird2remove = NULL,
  subset_prop = NULL,
  method = NULL
) {
  library(adehabitatHR)
  library(dplyr)
  library(raster)
  library(suncalc)
  library(sf)
  library(sp)

  message("Initializing")
  if (is.null(method)) {
    stop("method is not set. Use 'all_data' or 'indiv_union'.")
  }
  if (!method %in% c("all_data", "indiv_union")) {
    stop(sprintf(
      "Unknown method: '%s'. Use 'all_data' or 'indiv_union'.",
      method
    ))
  }

  # Set input directory
  if (is.null(input_dir)) {
    input_dir <- here::here(
      "outputs",
      species,
      region,
      "individual_data_nsd_dx3_dcentroid_labelled/"
    )
  }

  # Set output directory:
  # if method == "all_data", set a "home_ranges_all_data" subdirectory,
  # if method == "indiv_union", set a "home_ranges_indiv_union" subdirectory
  if (method == "all_data") {
    output_dir <- here::here(
      "outputs",
      species,
      region,
      "home_ranges_all_data/"
    )
  } else if (method == "indiv_union") {
    output_dir <- here::here(
      "outputs",
      species,
      region,
      "home_ranges_indiv_union/"
    )
  }

  # Create output directory if it doesn't exist yet
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # ------------------------------------------------------------
  # 1. Load data
  # ------------------------------------------------------------
  message("Loading data")

  # Load data
  files2prepare <- list.files(
    path = input_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )

  # Remove files of birds whose names are listed in bird2remove
  if (!is.null(bird2remove)) {
    pattern <- paste(bird2remove, collapse = "|")
    files2prepare <- files2prepare[
      !stringr::str_detect(basename(files2prepare), pattern)
    ]
    message("Removed files of ", paste(bird2remove, collapse = ", "))
  }

  data <- data.table::rbindlist(
    lapply(files2prepare, read.csv),
    fill = TRUE
  ) |>
    as.data.frame()

  # Sample subset_fraction of the rows from the combined dataset
  if (!is.null(subset_prop)) {
    set.seed(123)
    data <- data %>%
      sample_frac(subset_prop)
    message(paste("Sampling", subset_prop * 100, "% of the data"))
  }

  # ------------------------------------------------------------
  # 2. Keep only daylight locations
  # ------------------------------------------------------------
  message("Keep daylight locations")

  # Reproject the data in WGS84
  coords_WGS84 <- data |>
    dplyr::select(x_, y_) |>
    sf::st_as_sf(coords = c("x_", "y_"), crs = 2154) |>
    sf::st_transform(crs = 4326) |>
    sf::st_coordinates() |>
    as.data.frame() |>
    rename(lon = X, lat = Y)

  # Build the dataframe for suncalc
  suncalc_input <- data.frame(
    date = as.Date(data$t_original),
    lat = coords_WGS84$lat,
    lon = coords_WGS84$lon
  )

  # Calculate sunrise / sunset times
  sun_times <- getSunlightTimes(
    data = suncalc_input,
    keep = c("sunrise", "sunset"),
    tz = "UTC"
  )

  # Add to the original dataset and keep daylight locations
  data_day <- data |>
    mutate(
      t_posix = as.POSIXct(t_original, tz = "UTC"),
      sunrise = sun_times$sunrise,
      sunset = sun_times$sunset,
      is_daylight = t_posix >= sunrise & t_posix <= sunset
    ) |>
    filter(is_daylight == TRUE) |>
    dplyr::select(-sunrise, -sunset, -is_daylight)

  # ------------------------------------------------------------
  # 3. Keep only resident locations
  # ------------------------------------------------------------
  message("Keep only resident locations")

  # Keep only resident / stationary periods
  data_resident <- data_day |>
    filter(period_status == "resident")

  # ------------------------------------------------------------
  # 4. Remove duplicates
  # ------------------------------------------------------------
  message("Removing duplicates")

  # Remove duplicates, if any
  data_unique <- data_resident %>%
    distinct(bird_id, t_original, .keep_all = TRUE)

  # ------------------------------------------------------------
  # 5. Calculating home range
  # ------------------------------------------------------------
  message("Calculating population home range")

  # Convert the time column
  data_unique$t_original <- lubridate::ymd_hms(data_unique$t_original)

  # Create an amt object
  data_locations <- amt::make_track(
    data_unique,
    x_,
    y_,
    t_original,
    short_id,
    crs = 2154
  )

  # Remove NAs
  data_locations <- data_locations |>
    filter(!is.na(x_), !is.na(y_))

  # Create kernelUDs for the home ranges
  if (method == "all_data") {
    message("Method: all_data — calculating HR on all locations at once")

    homerange_locations <- amt::hr_kde(
      data_locations,
      h = amt:::hr_kde_ref(data_locations),
      levels = c(0.5, 0.9, 0.95)
    )

    hr_contours <- amt::hr_isopleths(
      homerange_locations,
      levels = c(0.5, 0.9, 0.95)
    )
  } else if (method == "indiv_union") {
    message(
      "Method: indiv_union — calculating HR for each individual and merging them"
    )

    individual_hrs <- list()

    for (ind in unique(data_locations$short_id)) {
      message(sprintf("Processing %s", ind))
      data_ind <- data_locations %>% filter(short_id == ind)

      if (nrow(data_ind) < 10) {
        warning(sprintf(
          "Skipping %s — too few locations (%d)",
          ind,
          nrow(data_ind)
        ))
        next
      }

      hr_ind <- amt::hr_kde(
        data_ind,
        h = amt:::hr_kde_ref(data_ind),
        levels = c(0.5, 0.9, 0.95)
      )

      individual_hrs[[ind]] <- amt::hr_isopleths(hr_ind)
    }

    message("Combining individual home ranges")
    # Combine all individual HRs
    hr_contours <- dplyr::bind_rows(individual_hrs) |>
      group_by(level) |>
      summarise(geometry = sf::st_union(geometry), .groups = "drop")
  }

  # Set as factor
  hr_contours$level <- as.factor(hr_contours$level)

  message("Extracting contours for each level")
  # Extract each contour separately
  hr_95 <- hr_contours[hr_contours$level == 0.95, ]
  mapview::mapview(hr_95)

  hr_90 <- hr_contours[hr_contours$level == 0.90, ]
  mapview::mapview(hr_90)

  hr_50 <- hr_contours[hr_contours$level == 0.50, ]
  mapview::mapview(hr_50)

  # ------------------------------------------------------------
  # 6. Exporting
  # ------------------------------------------------------------
  message("Exporting home ranges")

  sf::st_write(
    hr_95,
    paste0(output_dir, "/GF_Causses_HR95_href_", method, ".shp"),
    delete_layer = TRUE,
    quiet = TRUE
  )

  sf::st_write(
    hr_90,
    paste0(output_dir, "/GF_Causses_HR90_href_", method, ".shp"),
    delete_layer = TRUE,
    quiet = TRUE
  )

  sf::st_write(
    hr_50,
    paste0(output_dir, "/GF_Causses_HR50_href_", method, ".shp"),
    delete_layer = TRUE,
    quiet = TRUE
  )
}
