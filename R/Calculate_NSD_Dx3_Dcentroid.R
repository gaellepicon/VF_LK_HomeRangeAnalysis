#______________________________________________________________________________________________________________________
##
##      GOAL OF THE FUNCTION: Calculate NSD, Dx3 and Dcentroid to identify residential / exploratory periods of birds
##
##
##      CREDIT: Gaëlle Picon (picon.gaelle@gmail.com)
##
#______________________________________________________________________________________________________________________

# NSD: net square displacement between the location and a starting point. Here, for each bird, the starting point will be
# calculated as the mean location of the first week of data, because the first location doesn't necessarily make sense.
# It would have made sense if they were reintroduced birds, for instance.
#
# Dx3: distance between the location and the location 3 days earlier.
#
# Dcentroid: distance between noon locations and the centroid of all locations
#
# All these variables may not be useful, you may only use one to discriminate the behaviours.

calculate_nsd_dx3_dcentroid <- function(
  region,
  species,
  data_path,
  input_dir,
  output_dir
) {
  library(amt)
  library(dplyr)
  library(ggplot2)
  library(classInt)

  # Set input directory
  if (is.null(input_dir)) {
    input_dir <- here::here(
      "outputs",
      species,
      region,
      "individual_yearly_tracks/"
    )
  }

  # Set output directory
  if (is.null(output_dir)) {
    output_dir <- here::here(
      "outputs",
      species,
      region,
      "individual_data_nsd_dx3_dcentroid/"
    )
  }

  # Create output directory if it doesn't exist yet
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  message("Importing data")
  all_tracks <- list.files(
    input_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )

  # Create a list to store the results
  all_data <- list()

  # For each bird:
  for (track in all_tracks) {
    ind_name <- sub("_tracks\\.csv$|\\.csv$", "", basename(track))
    message("Processing: ", ind_name)
    data <- read.csv(track)

    # Convert the timestamp column into posixct format and remove NA
    data <- data |>
      dplyr::mutate(t_ = lubridate::ymd_hms(t_)) |>
      filter(!is.na(t_))

    # If the monitoring period is less than 3 days, skip the animal
    if (max(data$t_) - min(data$t_) < lubridate::days(3)) {
      message(
        "Skipping ",
        ind_name,
        " because monitoring period is less than 3 days."
      )
      next
    }
    ### ---- NSD ---- ###
    # Calculate the mean location of the first week of data (for NSD)
    first_week_mean <- data |>
      group_by(bird_id, year = lubridate::year(t_)) |>
      dplyr::filter(t_ <= first(t_) + lubridate::days(7)) |>
      summarise(
        mean_x = mean(x_),
        mean_y = mean(y_),
        .groups = "drop"
      )

    # Then, put the mean location back to the original data and calculate NSD
    data_nsd <- data |>
      left_join(first_week_mean, by = "bird_id") |>
      dplyr::mutate(
        nsd = (x_ - mean_x)^2 + (y_ - mean_y)^2
      ) |>
      select(-mean_x, -mean_y, year)

    ### ---- Dx3 ---- ###
    # Get the min time of the dataset:
    min_time <- min(data_nsd$t_)

    # Convert to a data.table
    data_nsd_dt <- data.table::setDT(data_nsd)

    # Create a lagged dataset and remove the "fake" dates before the beginning of the processed year
    # i.e.: since our datasets all start in January of the given year, the lagged dataset will create
    # dates in December of the previous year, for which we don't have data.
    data_lag72 <- data_nsd |>
      select(bird_id, x_, y_, t_) |>
      mutate(
        target_time = t_ - lubridate::days(3),
        x_lag72 = x_,
        y_lag72 = y_,
        t_original = t_
      ) |>
      select(bird_id, target_time, x_lag72, y_lag72, t_original) |>
      filter(target_time >= min_time) |>
      data.table::as.data.table()

    # Perform a rolling join to find the nearest lagged location for each original location
    data_rolled <- data_lag72[
      data_nsd,
      on = .(bird_id, target_time = t_),
      roll = "nearest",
      nomatch = NA
    ]

    # Calculate Dx3
    data_rolled[,
      dx3 := ifelse(
        !is.na(x_lag72),
        sqrt((x_ - x_lag72)^2 + (y_ - y_lag72)^2),
        NA
      )
    ]

    ### ---- Dcentroid ---- ###
    # Filter positions at noon
    noon_positions <- data_rolled |>
      mutate(hour = lubridate::hour(t_original)) |>
      filter(hour >= 11 & hour <= 13)

    # Calculate centroid of the individual
    centroids <- noon_positions |>
      group_by(bird_id, lubridate::year(t_original)) |>
      summarise(centroid_x = mean(x_), centroid_y = mean(y_), .groups = "drop")

    # Merge centroids and noon positions and calculate Dcentroid (distance of the location to the centroid)
    noon_positions_centroid <- noon_positions |>
      left_join(centroids, by = c("bird_id")) |>
      mutate(dcentroid = sqrt((x_ - centroid_x)^2 + (y_ - centroid_y)^2))

    data_nsd_dx3_dcentroid <- data_rolled |>
      left_join(
        noon_positions_centroid |>
          select(bird_id, t_original, dcentroid) |>
          distinct(bird_id, t_original, .keep_all = TRUE),
        by = c("bird_id", "t_original")
      )

    # Convert back to a data.frame
    df_nsd_dx3_dcentroid <- as.data.frame(data_nsd_dx3_dcentroid) |>
      dplyr::select(
        x_,
        y_,
        t_original,
        bird_id,
        nsd,
        x_lag72,
        y_lag72,
        dx3,
        dcentroid
      )

    # Convert nsd in square kilometers and dx3 + dcentroid in kilometers and add short_id
    df_nsd_dx3_dcentroid_converted <- df_nsd_dx3_dcentroid |>
      dplyr::mutate(
        nsd_km2 = nsd / 1000000,
        dx3_km = dx3 / 1000,
        dcentroid_km = dcentroid / 1000,
        short_id = ind_name
      )

    # Store the results in the list
    all_data[[ind_name]] <- df_nsd_dx3_dcentroid_converted

    # Save data
    write.csv(
      df_nsd_dx3_dcentroid_converted,
      file.path(
        output_dir,
        paste0(ind_name, "_nsd_dx3_dcentroid.csv")
      ),
      row.names = FALSE
    )
  }

  # Combine all results into a single data frame
  all_data_df <- bind_rows(all_data)

  # Create a summary for all individual-bird
  summary_plot <- all_data_df |>
    group_by(bird_id, lubridate::year(t_original)) |>
    summarise(
      max_nsd_km2 = max(nsd_km2, na.rm = TRUE),
      max_dx3_km = max(dx3_km, na.rm = TRUE),
      max_dcentroid_km = max(dcentroid_km, na.rm = TRUE),
      short_id = first(short_id),
      .groups = "drop"
    )

  message("Saving the summary for all individual-years")
  # Save the summary
  write.csv(
    summary_plot,
    file.path(output_dir, "summaries/summary_max_nsd_dx3_dcentroid.csv")
  )

  message("Done")
}
