#______________________________________________________________________________________________________________________
##
##      EXPLORATION SCRIPT: Find NSD, DX3, DCENTROID thresholds and plot them
##                          The function R/Calculate_NSD_Dx3_Dcentroid.R must be ran before to calculate the variables
##
##      CREDIT: Gaëlle Picon (picon.gaelle@gmail.com)
##
#______________________________________________________________________________________________________________________

library(dplyr)
library(ggplot2)

# Set input directory
input_dir <- here::here(
  "outputs",
  species,
  region,
  "individual_data_nsd_dx3_dcentroid/"
)


# Set output directory
output_dir <- here::here(
  "outputs",
  species,
  region,
  "individual_data_nsd_dx3_dcentroid/"
)

# Import data
summary_plot <- read.csv(file.path(
  output_dir,
  "summaries/summary_max_nsd_dx3_dcentroid.csv"
))


### PLOTS ###

## NSD ------
# Identify NSD thresholds with k-means clustering
# 2 thresholds:
log_nsd <- log10(summary_plot$max_nsd_km2)
kmeans_result_log_nsd <- kmeans(log_nsd, centers = 2)
thresholds_nsd_max <- 10^kmeans_result_log_nsd$centers
print(thresholds_nsd_max)
# 1    2472.671
# 2  113806.921

# 1 threshold:
log_nsd <- log10(summary_plot$max_nsd_km2)
kmeans_result_log_nsd <- kmeans(log_nsd, centers = 1)
thresholds_nsd_max <- 10^kmeans_result_log_nsd$centers
print(thresholds_nsd_max)
# 1 3816.277

# Plot the distribution of the NSD-max values and convert x in log scale for better visualization
p_nsd <- ggplot(summary_plot, aes(x = max_nsd_km2)) +
  geom_histogram(fill = "#6A6970", color = "lightgrey") +
  geom_vline(
    xintercept = 3816,
    linetype = "dashed",
    colour = "#2574C4",
    linewidth = 1,
    show.legend = TRUE
  ) +
  # geom_vline(
  #   xintercept = 113806,
  #   linetype = "dashed",
  #   colour = "#2574C4",
  #   linewidth = 1
  # ) +
  annotate(
    "label",
    x = 3816,
    y = Inf, # top of the plot
    label = "3 816 km²",
    vjust = 1.5,
    size = 5,
    colour = "#2574C4",
    fill = "white"
  ) +
  # annotate(
  #   "label",
  #   x = 113806,
  #   y = Inf,
  #   label = "113 806 km²",
  #   vjust = 1.5,
  #   size = 5,
  #   colour = "#2574C4",
  #   fill = "white"
  # ) +
  scale_x_log10(
    breaks = c(100, 1000, 10000, 100000, 1000000, 10000000),
    limits = c(100, 1500000)
  ) +
  labs(
    title = "Distribution of max NSD values across individual-years",
    subtitle = "Threshold method: k-means",
    x = "Max NSD (km²) - log scale",
    y = "Count"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    title = element_text(size = 15),
    caption = element_text(size = 14)
  )

print(p_nsd)

# Save the nsd plot
ggsave(
  file.path(output_dir, "/plots/nsd_1kmeans.png"),
  plot = p_nsd,
  width = 12,
  height = 8,
  dpi = 300
)


## DX3 ------
# Identify Dx3 thresholds with k-means clustering
# 2 thresholds:
kmeans_result__dx3 <- kmeans(summary_plot$max_dx3_km, centers = 2)
thresholds_dx3_max <- kmeans_result__dx3$centers
print(thresholds_dx3_max)
# 1   67.21337
# 2   464.84135

# 1 threshold:
kmeans_result__dx3 <- kmeans(summary_plot$max_dx3_km, centers = 1)
thresholds_dx3_max <- kmeans_result__dx3$centers
print(thresholds_dx3_max)
# 1 96.37276

# Plot the distribution of the Dx3-max values
p_dx3 <- ggplot(summary_plot, aes(x = max_dx3_km)) +
  geom_histogram(
    fill = "#6A6970",
    color = "lightgrey",
    binwidth = 20,
    boundary = 0
  ) +
  geom_vline(
    xintercept = 96,
    linetype = "dashed",
    colour = "#2574C4",
    linewidth = 1
  ) +
  # geom_vline(
  #   xintercept = 464,
  #   linetype = "dashed",
  #   colour = "#2574C4",
  #   linewidth = 1
  # ) +
  annotate(
    "label",
    x = 96,
    y = Inf, # top of the plot
    label = "96 km",
    vjust = 1.5,
    size = 5,
    colour = "#2574C4",
    fill = "white"
  ) +
  # annotate(
  #   "label",
  #   x = 464,
  #   y = Inf,
  #   label = "464 km",
  #   vjust = 1.5,
  #   size = 5,
  #   colour = "#2574C4",
  #   fill = "white"
  # ) +
  labs(
    title = "Distribution of max Dx3 values across individual-years",
    subtitle = "Threshold method: k-means",
    x = "Max Dx3 (km)",
    y = "Count"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    title = element_text(size = 15),
    caption = element_text(size = 14)
  )

print(p_dx3)

# Save the dx3 plot
ggsave(
  file.path(output_dir, "/plots/dx3_1kmeans.png"),
  plot = p_dx3,
  width = 12,
  height = 8,
  dpi = 300
)


## DCENTROID ------
# Identify Dcentroid threshold with k-means clustering
# 1 thresholds:
kmeans_result_dcentroid <- kmeans(summary_plot$max_dcentroid_km, centers = 1)
thresholds_dcentroid_max <- kmeans_result_dcentroid$centers
print(thresholds_dcentroid_max)
# 1 86.37013

# Plot the distribution of the max Dcentroid values
p_dcentroid <- ggplot(summary_plot, aes(x = max_dcentroid_km)) +
  geom_histogram(
    fill = "#6A6970",
    color = "lightgrey",
    binwidth = 20,
    boundary = 0
  ) +
  geom_vline(
    xintercept = 86,
    linetype = "dashed",
    colour = "#2574C4",
    linewidth = 1
  ) +
  # geom_vline(
  #   xintercept = 464,
  #   linetype = "dashed",
  #   colour = "#2574C4",
  #   linewidth = 1
  # ) +
  annotate(
    "label",
    x = 96,
    y = Inf, # top of the plot
    label = "86 km",
    vjust = 1.5,
    size = 5,
    colour = "#2574C4",
    fill = "white"
  ) +
  # annotate(
  #   "label",
  #   x = 464,
  #   y = Inf,
  #   label = "464 km",
  #   vjust = 1.5,
  #   size = 5,
  #   colour = "#2574C4",
  #   fill = "white"
  # ) +
  labs(
    title = "Distribution of max Dcentroid values across individual-years",
    subtitle = "Threshold method: k-means",
    x = "Max Dcentroid (km)",
    y = "Count"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    title = element_text(size = 15)
  )

print(p_dcentroid)

# Save the dcentroid plot
ggsave(
  file.path(output_dir, "/plots/dcentroid_1kmeans.png"),
  plot = p_dcentroid,
  width = 12,
  height = 8,
  dpi = 300
)


## EXPLORATION STATUS ------
# Add the exploration status of each individual-year depending on the max NSD and max Dx3 values and the thresholds identified above
# OPTION 1: Exploration statuses set according to NSD and Dx3
summary_plot_explo_status_nsd_dx3 <- summary_plot |>
  mutate(
    exploration_status = case_when(
      max_nsd_km2 < 3816 & max_dx3_km < 96 ~ "residential",
      TRUE ~ "exploratory"
    )
  )

# Save the summary with exploration status
write.csv(
  summary_plot_explo_status_nsd_dx3,
  file.path(output_dir, "/summaries/summary_explo_status_nsd_dx3.csv")
)

# Plot the summary to check the max NSD and max Dx3 for all individual-years and assign
# colours to the points depending on their exploration status
p_summary <- ggplot(
  summary_plot_explo_status_nsd_dx3,
  aes(x = max_nsd_km2, y = max_dx3_km, color = exploration_status)
) +
  geom_point(size = 3) +
  # geom_text(size = 2.5, vjust = -0.8, show.legend = FALSE) +
  geom_vline(xintercept = 3816, linetype = "dashed", colour = "black") +
  # geom_vline(xintercept = 113806, linetype = "dashed", colour = "black") +
  geom_hline(yintercept = 96, linetype = "dashed", colour = "black") +
  # geom_hline(yintercept = 400, linetype = "dashed", colour = "black") +
  labs(
    title = "Exploration statuses of individual-years depending on their max NSD and max Dx3 values",
    subtitle = "Dashed lines represent the max NSD and max Dx3 thresholds. \nMethod: k-means",
    x = "Max NSD (km²)",
    y = "Max Dx3 (km)",
    color = "Exploration status"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    title = element_text(size = 15),
    legend.text = element_text(size = 14)
  )

print(p_summary)

# Save the plot
ggsave(
  file.path(output_dir, "/plots/explo_status_nsd_dx3_kmeans.png"),
  plot = p_summary,
  width = 12,
  height = 8,
  dpi = 300
)

# OPTION 2: Exploration statuses set according to Dcentroid and Dx3
summary_plot_explo_status_dcentroid_dx3 <- summary_plot |>
  mutate(
    exploration_status = case_when(
      max_dcentroid_km < 86 & max_dx3_km < 96 ~ "residential",
      TRUE ~ "exploratory"
    )
  )

# Save the summary with exploration status
write.csv(
  summary_plot_explo_status_dcentroid_dx3,
  file.path(output_dir, "/summaries/summary_explo_status_dcentroid_dx3.csv")
)

# Plot the summary to check the max NSD and max Dx3 for all individual-years and assign
# colours to the points depending on their exploration status
p_summary <- ggplot(
  summary_plot_explo_status_dcentroid_dx3,
  aes(x = max_dcentroid_km, y = max_dx3_km, color = exploration_status)
) +
  geom_point(size = 3) +
  # geom_text(size = 2.5, vjust = -0.8, show.legend = FALSE) +
  geom_vline(xintercept = 86, linetype = "dashed", colour = "black") +
  # geom_vline(xintercept = 113806, linetype = "dashed", colour = "black") +
  geom_hline(yintercept = 96, linetype = "dashed", colour = "black") +
  # geom_hline(yintercept = 400, linetype = "dashed", colour = "black") +
  labs(
    title = "Exploration statuses of individual-years depending on their max Dcentroid and max Dx3 values",
    subtitle = "Dashed lines represent the max Dcentroid and max Dx3 thresholds. \nMethod: k-means",
    x = "Max Dcentroid (km²)",
    y = "Max Dx3 (km)",
    color = "Exploration status"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    title = element_text(size = 15),
    legend.text = element_text(size = 14)
  )

print(p_summary)

# Save the plot
ggsave(
  file.path(output_dir, "/plots/explo_status_dcentroid_dx3_kmeans.png"),
  plot = p_summary,
  width = 12,
  height = 8,
  dpi = 300
)
