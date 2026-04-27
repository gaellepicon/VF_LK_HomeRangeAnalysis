#_______________________________________________________________________________________________________________
##
##      GOAL OF THE FUNCTION: Resample data to one location per hour
##
##
##      CREDIT: Gaëlle Picon (picon.gaelle@gmail.com)
##
#_______________________________________________________________________________________________________________

resample_tracks <- function(
  region,
  species,
  input_dir = NULL,
  output_dir = NULL
) {
  message("Initializing resampling")

  library(amt)
  library(dplyr)
  library(lubridate)
  library(here)

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
      "individual_yearly_tracks_resampled"
    )
  }

  # Create output directory if necessary
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # List csv files
  all_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

  if (length(all_files) == 0) {
    stop(sprintf("No CSV files found in %s", input_dir))
  }

  # Create the output list and run the loop
  results_list <- list()

  for (f in all_files) {
    ind_name <- sub("\\.csv$", "", basename(f))
    message("Resampling: ", ind_name)

    # Load data
    track <- tryCatch(
      read.csv(f),
      error = function(e) {
        warning(sprintf("Could not read %s: %s", f, e$message))
        return(NULL)
      }
    )
    if (is.null(track)) {
      next
    }

    # Check if the columns required for clustering step are present
    required_cols <- c("x_", "y_", "t_", "bird_id")
    if (!all(required_cols %in% names(track))) {
      warning(sprintf("%s is missing required columns. Skipping.", ind_name))
      next
    }

    # Ensure timestamp is POSIXct and sort data chronologically
    track <- track %>%
      mutate(t_ = as.POSIXct(t_, tz = "UTC")) %>%
      arrange(t_)

    # Resample to one fix per 60 minutes from first fix
    track_resampled <- tryCatch(
      {
        amt::make_track(track, x_, y_, t_, crs = 2154, all_cols = T) %>%
          amt::track_resample(
            rate = lubridate::hours(1),
            tolerance = lubridate::minutes(10)
          ) %>%
          as.data.frame()
      },
      error = function(e) {
        warning(sprintf("Resampling failed for %s: %s", ind_name, e$message))
        return(NULL)
      }
    )
    if (is.null(track_resampled)) {
      next
    }

    message(sprintf(
      "Original fixes: %d | Resampled fixes: %d",
      nrow(track),
      nrow(track_resampled)
    ))

    # Save resampled track
    out_csv <- file.path(output_dir, paste0(ind_name, "_resampled.csv"))
    write.csv(track_resampled, out_csv, row.names = FALSE)

    results_list[[ind_name]] <- track_resampled
  }

  message("All done.")
  invisible(results_list)
}
