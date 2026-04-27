#_______________________________________________________________________________________________________________
##
##      GOAL OF THE FUNCTION: Visualize extra home range movements (EHRMs)
##                            with lat / long plot
##
##
##      CREDIT: Gaëlle Picon (picon.gaelle@gmail.com)
##
#_______________________________________________________________________________________________________________

ehrm_latlong <- function(
  data,
  species,
  region,
  max_north_lat = NULL,
  max_south_lat = NULL,
  max_east_long = NULL,
  max_west_long = NULL,
  quantile_threshold = NULL,
  output_dir = NULL,
  save_plots = TRUE
) {
  message("Initializing")

  # Load libraries
  library(dplyr)
  library(ggplot2)

  # Set output directory
  if (is.null(output_dir)) {
    output_dir <- here::here("outputs", species, region, "EHRMs_lat_long")
  }

  # Create output directory if saving plots
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Prepare data
  message("Preparing data")

  # Convert timestamp to POSIXct if not already
  if (!inherits(data$UTC_datetime, "POSIXct")) {
    data <- data %>%
      dplyr::mutate(UTC_datetime = lubridate::ymd_hms(UTC_datetime))
  }

  # Create a year column if it doesn't exist yet
  if (!"year" %in% base::names(data)) {
    data <- data %>%
      dplyr::mutate(year = lubridate::year(UTC_datetime))
  }

  # Add a shorter name to the individuals, for better display in the plots
  data <- data %>%
    dplyr::mutate(
      short_id = paste0(
        sub(".*_([^_]+_[^_]+)$", "\\1", individual.local.identifier),
        "_",
        year
      )
    )

  # If max nort / south / east / west thresholds are not provided, calculate them with quantile
  if (is.null(max_north_lat)) {
    max_north_lat <- quantile(data$Y_93, quantile_threshold, na.rm = TRUE)
    message(sprintf(
      "max_north_lat set automatically to %.0f (quantile %.2f)",
      max_north_lat,
      quantile_threshold
    ))
  }
  if (is.null(max_south_lat)) {
    max_south_lat <- quantile(data$Y_93, 1 - quantile_threshold, na.rm = TRUE)
    message(sprintf(
      "max_south_lat set automatically to %.0f (quantile %.2f)",
      max_south_lat,
      1 - quantile_threshold
    ))
  }
  if (is.null(max_east_long)) {
    max_east_long <- quantile(data$X_93, quantile_threshold, na.rm = TRUE)
    message(sprintf(
      "max_east_long set automatically to %.0f (quantile %.2f)",
      max_east_long,
      quantile_threshold
    ))
  }
  if (is.null(max_west_long)) {
    max_west_long <- quantile(data$X_93, 1 - quantile_threshold, na.rm = TRUE)
    message(sprintf(
      "max_west_long set automatically to %.0f (quantile %.2f)",
      max_west_long,
      1 - quantile_threshold
    ))
  }

  # Get unique individuals and years
  individuals <- unique(data$individual.local.identifier)
  years <- sort(unique(data$year))
  n_individuals <- length(individuals)
  n_years <- length(years)

  message(paste("Found ", n_individuals, " individuals"))
  message(paste("Found ", n_years, " years: ", paste(years, collapse = ", ")))

  # Set a color palette so that the individuals will always have the same color
  short_ids <- unique(data$short_id)
  id_colors <- setNames(
    viridis::viridis(length(short_ids), option = "turbo"),
    short_ids
  )

  # Store the outputs
  plots_long <- list()
  plots_lat <- list()
  summary_list <- list()

  # Loop through each year
  for (yr in years) {
    message(paste("Processing year ", yr))

    # Filter data for this year
    data_yearly <- data %>% filter(year == yr)

    # Get individuals present in this year
    individuals_year <- unique(data_yearly$individual.local.identifier)
    short_ids_year <- unique(data_yearly$short_id)

    message(paste(length(individuals_year), " individuals in ", yr))

    # Identify individuals that cross the max north value
    cross_max_north <- data_yearly %>%
      dplyr::filter(Y_93 > max_north_lat) %>%
      distinct(individual.local.identifier)

    # Identify individuals that cross the max south value
    cross_max_south <- data_yearly %>%
      dplyr::filter(Y_93 < max_south_lat) %>%
      distinct(individual.local.identifier)

    # Identify individuals that cross the max east value
    cross_max_east <- data_yearly %>%
      dplyr::filter(X_93 > max_east_long) %>%
      distinct(individual.local.identifier)

    # Identify individuals that cross the max west value
    cross_max_west <- data_yearly %>%
      dplyr::filter(X_93 < max_west_long) %>%
      distinct(individual.local.identifier)

    # Define, for each individual that cross the thresholds, which point will have a label in the plots
    # Here, the northernmost / southernmost / easternmost / westernmost points will be labelled
    north_labels <- data_yearly %>%
      dplyr::semi_join(cross_max_north, by = "individual.local.identifier") %>%
      dplyr::group_by(individual.local.identifier) %>%
      dplyr::slice_max(Y_93, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()

    south_labels <- data_yearly %>%
      dplyr::semi_join(cross_max_south, by = "individual.local.identifier") %>%
      dplyr::group_by(individual.local.identifier) %>%
      dplyr::slice_min(Y_93, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()

    east_labels <- data_yearly %>%
      dplyr::semi_join(cross_max_east, by = "individual.local.identifier") %>%
      group_by(individual.local.identifier) %>%
      slice_max(X_93, n = 1, with_ties = FALSE) %>%
      ungroup()

    west_labels <- data_yearly %>%
      dplyr::semi_join(cross_max_west, by = "individual.local.identifier") %>%
      group_by(individual.local.identifier) %>%
      slice_min(X_93, n = 1, with_ties = FALSE) %>%
      ungroup()

    # Create the longitude over time plot
    message("Create longitude plot")

    plot_longitude <- ggplot(
      data_yearly,
      aes(
        x = UTC_datetime,
        y = X_93,
        color = short_id,
        group = short_id
      )
    ) +
      geom_line(alpha = 0.7, linewidth = 0.3) +
      # geom_point(alpha = 0.3, size = 0.5) +
      geom_hline(
        yintercept = max_east_long,
        linetype = "dashed",
        linewidth = 0.5,
        color = "red"
      ) +
      geom_hline(
        yintercept = max_west_long,
        linetype = "dashed",
        linewidth = 0.5,
        color = "red"
      ) +
      scale_color_manual(
        values = id_colors,
        drop = TRUE
      ) +
      labs(
        title = paste("Longitude across time - ", yr),
        x = "time",
        y = "longitude"
      ) +
      theme_bw() +
      geom_text(
        data = east_labels,
        aes(label = short_id),
        color = "red",
        size = 3,
        vjust = -0.6,
        show.legend = FALSE
      ) +
      geom_text(
        data = west_labels,
        aes(label = short_id),
        color = "red",
        size = 3,
        vjust = -0.6,
        show.legend = FALSE
      ) +
      theme(
        legend.position = "none"
      )

    # Store the plot
    plots_long[[as.character(yr)]] <- plot_longitude

    # Print the plot
    print(plot_longitude)

    # Save the plot
    if (save_plots) {
      ggsave(
        filename = file.path(output_dir, paste0("longitude_plot_", yr, ".png")),
        plot = plot_longitude,
        width = 12,
        height = 8,
        dpi = 300
      )

      message(paste("Saved longitude plot for ", yr))
    }

    # Create the latitude over time plot
    message("Create latitude plot")

    plot_latitude <- ggplot(
      data_yearly,
      aes(
        x = UTC_datetime,
        y = Y_93,
        color = short_id,
        group = short_id
      )
    ) +
      geom_line(alpha = 0.7, linewidth = 0.3) +
      # geom_point(alpha = 0.3, size = 0.5) +
      geom_hline(
        yintercept = max_north_lat,
        linetype = "dashed",
        linewidth = 0.5,
        color = "red"
      ) +
      geom_hline(
        yintercept = max_south_lat,
        linetype = "dashed",
        linewidth = 0.5,
        color = "red"
      ) +
      scale_color_manual(
        values = id_colors,
        drop = TRUE
      ) +
      labs(
        title = paste("Latitude across time - ", yr),
        x = "time",
        y = "latitude"
      ) +
      theme_bw() +
      geom_text(
        data = north_labels,
        aes(label = short_id),
        color = "red",
        size = 3,
        vjust = -0.6,
        show.legend = FALSE
      ) +
      geom_text(
        data = south_labels,
        aes(label = short_id),
        color = "red",
        size = 3,
        vjust = -0.6,
        show.legend = FALSE
      ) +
      theme(
        legend.position = "none"
      )

    # Store the plot
    plots_lat[[as.character(yr)]] <- plot_latitude

    # Print the plot
    print(plot_latitude)

    # Save the plot
    if (save_plots) {
      ggsave(
        filename = file.path(output_dir, paste0("latitude_plot_", yr, ".png")),
        plot = plot_latitude,
        width = 12,
        height = 8,
        dpi = 300
      )

      message(paste("Saved latitude plot for ", yr))
    }

    # Build summary
    yearly_summary <- data.frame(
      individual = individuals_year,
      year = yr,
      cross_max_north = individuals_year %in%
        cross_max_north$individual.local.identifier,
      cross_max_south = individuals_year %in%
        cross_max_south$individual.local.identifier,
      cross_max_east = individuals_year %in%
        cross_max_east$individual.local.identifier,
      cross_max_west = individuals_year %in%
        cross_max_west$individual.local.identifier
    ) %>%
      mutate(
        long_trip = cross_max_north |
          cross_max_south |
          cross_max_east |
          cross_max_west
      )

    summary_list[[as.character(yr)]] <- yearly_summary
  }
  # Save summary
  full_summary <- bind_rows(summary_list)

  summary_path <- file.path(output_dir, "long_trip_individuals.csv")
  write.csv(full_summary, summary_path, row.names = FALSE)
  message(sprintf("Long trip summary saved to %s", summary_path))
  message(sprintf(
    "Total bird-years with long trips: %d / %d",
    sum(full_summary$long_trip),
    nrow(full_summary)
  ))

  # Return  plots and metadata
  return(list(
    plots_longitude = plots_long,
    plots_latitude = plots_lat,
    summary = full_summary,
    years = years,
    n_years = n_years,
    n_individuals = n_individuals,
    individuals = individuals,
    thresholds = list(
      max_north_lat = max_north_lat,
      max_south_lat = max_south_lat,
      max_east_long = max_east_long,
      max_west_long = max_west_long
    )
  ))
}
