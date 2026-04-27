#______________________________________________________________________________________________________________________
##
##      GOAL OF THE FUNCTION: Label the residential / exploratory periods of birds thanks to nsd and dx3 thresholds
##
##
##      CREDIT: Gaëlle Picon (picon.gaelle@gmail.com)
##
#______________________________________________________________________________________________________________________

# Run R/Calculate_NSD_Dx3_Dcentroid.R function before to calculate these variables and to identify the relevant threshold
# to discriminate residential and exploratory periods.

label_residential_exploratory <- function(
  region = NULL,
  species = NULL,
  data_path = NULL,
  input_dir = NULL,
  output_dir = NULL,
  nsd_max_threshold = NULL,
  dx3_max_threshold = NULL,
  dcentroid_max_threshold = NULL
) {
  # Set input directory
  if (is.null(input_dir)) {
    input_dir <- here::here(
      "outputs",
      species,
      region,
      "individual_data_nsd_dx3_dcentroid/"
    )
  }

  # Set output directory
  if (is.null(output_dir)) {
    output_dir <- here::here(
      "outputs",
      species,
      region,
      "individual_data_nsd_dx3_dcentroid_labelled/"
    )
  }

  # Create output directory if it doesn't exist yet
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Import data
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

    # Classify from noon locations only (the only ones that have dcentroid info)
    # data_noon <- data |>
    #   filter(!is.na(dcentroid_km)) |>
    #   mutate(
    #     period_status = case_when(
    #       dcentroid_km > dcentroid_threshold |
    #         dx3_km > dx3_threshold ~ "exploratory",
    #       TRUE ~ "resident"
    #     )
    #   )

    data_noon <- data |>
      filter(!is.na(dcentroid_km)) |>
      mutate(
        period_status = case_when(
          dcentroid_km > dcentroid_threshold ~ "exploratory",
          TRUE ~ "resident"
        )
      )

    # Merge labels back to the full dataset
    data_labelled <- data |>
      left_join(
        data_noon |> select(bird_id, t_original, period_status),
        by = c("bird_id", "t_original")
      )

    # Propagate labels to the empty rows
    data_labelled_fill <- data_labelled %>%
      arrange(bird_id, t_original) %>%
      group_by(bird_id) %>%
      mutate(
        period_status = ifelse(
          is.na(period_status),
          lag(period_status),
          period_status
        ),
        period_status = ifelse(
          is.na(period_status),
          lead(period_status),
          period_status
        )
      ) |>
      tidyr::fill(period_status, .direction = "downup") |>
      ungroup()

    # Save the annotated file
    write.csv(
      data_labelled_fill,
      file.path(
        output_dir,
        paste0(ind_name, "_annotated.csv")
      ),
      row.names = FALSE
    )
  }
}
