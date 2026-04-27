#_______________________________________________________________________________________________________________
##
##      GOAL OF THE FUNCTION: Create segments and clusters to remove locations of long-distance travels
##
##
##      CREDIT: Gaëlle Picon (picon.gaelle@gmail.com)
##
#_______________________________________________________________________________________________________________

calculate_seg_clusts <- function(
  region,
  species,
  data_path,
  test_individuals = NULL, # Character vector of individual IDs to test on
  kmax = NULL, # Maximum number of segments to test
  lmin = NULL, # Minimum segment length (in number of fixes)
  nclust = NULL, # Number of clusters (1 = stationary, 2 = long trip)
  input_dir = NULL,
  output_dir = NULL
) {
  message("Initializing")

  # Load packages
  library(ggplot2)
  library(dplyr)
  library(segclust2d)
  library(ggspatial)

  # --- Check required parameters ---
  if (is.null(kmax)) {
    stop("kmax is not set")
  }
  if (is.null(lmin)) {
    stop("lmin is not set")
  }
  if (is.null(nclust)) {
    stop("nclust is not set")
  }

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
      "/"
    )
  }

  # Create output directory if necessary
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Subdirectories for outputs
  plot_dir <- here::here(output_dir, "diagnostic_plots")

  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }

  seg_dir <- here::here(output_dir, "segmented_tracks")

  if (!dir.exists(seg_dir)) {
    dir.create(seg_dir, recursive = TRUE)
  }

  # List all CSV files
  all_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

  # Subset to test individuals if provided
  if (!is.null(test_individuals)) {
    pattern <- paste(test_individuals, collapse = "|")
    all_files <- grep(pattern, all_files, value = TRUE)

    if (length(all_files) == 0) {
      stop(
        "No CSV files matched the provided test_individuals. Check spelling."
      )
    }
    message(sprintf(
      "Test mode: running on %d individual(s).",
      length(all_files)
    ))
  } else {
    message(sprintf("Running on all %d CSV files.", length(all_files)))
  }

  # --- Main loop ---
  results_list <- list()

  for (f in all_files) {
    ind_name <- sub("\\.csv$", "", basename(f))
    message("Processing: ", ind_name)

    # Load data
    track <- tryCatch(
      read.csv(f, stringsAsFactors = FALSE),
      error = function(e) {
        warning(sprintf("Failed to read %s: %s", f, e$message))
        return(NULL)
      }
    )
    if (is.null(track)) {
      next
    }

    # Check that necessary columns are present
    required_cols <- c("x_", "y_", "t_", "bird_id")
    if (!all(required_cols %in% names(track))) {
      warning(sprintf("%s is missing required columns. Skipping.", ind_name))
      next
    }

    # Ensure t_ is POSIXct
    track$t_ <- as.POSIXct(track$t_, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

    # Order data by time
    track <- track |> dplyr::arrange(t_)

    # Skip if too few fixes for segmentation
    if (nrow(track) < lmin * 2) {
      warning(sprintf(
        "%s has too few fixes (%d) for segmentation with lmin=%d. Skipping.",
        ind_name,
        nrow(track),
        lmin
      ))
      next
    }

    # Compute a safe kmax for the individual:
    # When one individual has too few locations, the kmax = 20 may be too high.
    # Thus, we set a safe kmax_safe depending on the individual's number of fixes.
    # See Patin et al. 2019 and segclust2d README.
    kmax_safe <- min(kmax, floor(0.75 * nrow(track) / lmin))
    message(sprintf("Kmax_safe value: %d", kmax_safe))

    # Compute centroid of all locations as a proxy for colony centre
    centroid_x <- mean(track$x_)
    centroid_y <- mean(track$y_)

    # Add distance from centroid as a column
    track <- track %>%
      mutate(dist_centroid = sqrt((x_ - centroid_x)^2 + (y_ - centroid_y)^2))

    # Then, run segmentation and clustering
    seg_result <- tryCatch(
      segclust2d::segclust(
        x = track,
        Kmax = kmax_safe,
        lmin = lmin,
        ncluster = nclust,
        seg.var = c("x_", "y_"),
        scale.var = TRUE
      ),
      error = function(e) {
        warning(sprintf("segclust failed for %s: %s", ind_name, e$message))
        return(NULL)
      }
    )
    if (is.null(seg_result)) {
      next
    }
    # Extract segmented data
    track_seg <- segclust2d::augment(seg_result)

    # Deal with segclust's cluster numbering, that ends up with stationary being cluster 1 or 2 and same for long trips
    # We consider that the stationary cluster always has lower SD of x and y than the long trip cluster
    cluster_spread <- track_seg %>%
      group_by(state) %>%
      summarise(
        spread = mean(c(sd(x_), sd(y_))),
        .groups = "drop"
      ) %>%
      arrange(spread)

    stationary_state <- cluster_spread$state[1] # lowest spread = stationary
    longtrip_state <- cluster_spread$state[2] # highest spread = long trip

    message(sprintf(
      "Stationary cluster: %d | Long trip cluster: %d",
      stationary_state,
      longtrip_state
    ))

    # Recode state into a consistent label regardless of segclust2d's arbitrary numbering
    track_seg <- track_seg %>%
      mutate(state_recoded = ifelse(state == stationary_state, 1, 2))

    # Save segmented track
    out_csv <- file.path(seg_dir, paste0(ind_name, "_segmented.csv"))
    write.csv(track_seg, out_csv, row.names = FALSE)

    # PLOTS
    state_labels <- c("1" = "Cluster 1", "2" = "Cluster 2")
    state_colours <- c("1" = "#2271a5", "2" = "#c04628")

    # 1. Diagnostic plots
    p_likelihood <- segclust2d::plot_likelihood(seg_result)

    # 2. Trajectory in space with cluster colouring
    # Prepare data: convert into sf object (for mapping compatibility)
    track_seg_sf_RGF93 <- sf::st_as_sf(
      track_seg,
      coords = c("x_", "y_"),
      crs = 2154
    )

    # Set the right CRS (for mapping compatibilty with maptiles)
    track_seg_sf_WGS84 <- sf::st_transform(track_seg_sf_RGF93, crs = 4326)

    # Load map tiles for the area covered by the track
    tiles <- maptiles::get_tiles(
      track_seg_sf_WGS84,
      provider = "OpenStreetMap",
      zoom = 9,
      crop = TRUE
    )

    # Add lat and long columns for the plot
    track_seg_sf_WGS84 <- track_seg_sf_WGS84 %>%
      mutate(
        lon = sf::st_coordinates(.)[, 1],
        lat = sf::st_coordinates(.)[, 2]
      )

    # Create the map
    p_map <- ggplot(
      track_seg_sf_WGS84,
      aes(x = lon, y = lat, colour = factor(state_recoded))
    ) +
      annotation_map_tile(type = "osm", zoom = 9, quiet = TRUE, alpha = 0.5) +
      geom_path(colour = "grey70", linewidth = 0.3) +
      geom_point(size = 0.9) +
      # scale_colour_brewer(
      #   palette = "Set1",
      #   name = "Cluster",
      #   labels = c("1" = "Cluster 1", "2" = "Cluster 2")
      # ) +
      scale_colour_manual(
        values = state_colours,
        labels = state_labels,
        name = "Cluster"
      ) +
      coord_sf() +
      labs(
        title = ind_name,
        subtitle = sprintf(
          "lmin = %d | Kmax = %d | nclust = %d",
          lmin,
          kmax,
          nclust
        ),
        x = "X (m)",
        y = "Y (m)"
      ) +
      theme_bw()

    # 3. X and Y coordinates over time, coloured by cluster
    p_time <- track_seg %>%
      dplyr::mutate(fix_index = seq_along(x_)) %>%
      tidyr::pivot_longer(
        cols = c("x_", "y_"),
        names_to = "coord",
        values_to = "value"
      ) %>%
      ggplot(aes(x = fix_index, y = value, colour = factor(state_recoded))) +
      geom_point(size = 0.5) +
      facet_wrap(~coord, ncol = 1, scales = "free_y") +
      scale_colour_manual(
        values = state_colours,
        labels = state_labels,
        name = "Cluster"
      ) +
      labs(
        title = ind_name,
        x = "Fix index",
        y = "Coordinate value (m)"
      ) +
      theme_bw()

    # Save plots
    ggsave(
      filename = file.path(plot_dir, paste0(ind_name, "_likelihood.png")),
      plot = p_likelihood,
      width = 8,
      height = 5
    )
    ggsave(
      filename = file.path(plot_dir, paste0(ind_name, "_map.png")),
      plot = p_map,
      width = 8,
      height = 7
    )
    ggsave(
      filename = file.path(plot_dir, paste0(ind_name, "_time.png")),
      plot = p_time,
      width = 10,
      height = 5
    )

    # Store result
    results_list[[ind_name]] <- track_seg

    message(sprintf(
      "Done. Plots and segmented CSV saved for %s.",
      ind_name
    ))
  }

  message("All individuals done.")
  return(results_list)
}
