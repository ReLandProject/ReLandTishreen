library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(magrittr)
library(patchwork)
library(sf)

# Load data -----------------------------------------------------------------------

sites_plot_all <- FALSE

# sites_to_plot <- read.csv(here::here("data/raw/csv", "sites_to_plot.csv"))
sites_to_plot <- read.csv(here::here("data/raw/csv", "sites_to_plot2.csv"))

dahiti_water_level <- read.csv(here::here("data/processed/csv", "tishreen_dahiti_water_level_r.csv"))

polygons_water_surface <- read.csv(here::here("data/processed/csv", "tishreen_surface_time_series_r.csv"))

tshr_polys_pct_info_min <- read.csv(here::here("output/csv", "tshr_polys_area_pct_info_min.csv")) %>%
  set_colnames(sub("X", "", colnames(.), fixed = TRUE)) %>%
  rename("Cyclically Affected" = "Affected")

tshr_polys_pct_info_max <- read.csv(here::here("output/csv", "tshr_polys_area_pct_info_max.csv")) %>%
  set_colnames(sub("X", "", colnames(.), fixed = TRUE)) %>%
  rename("Cyclically Affected" = "Affected")

#  # Filter sites to plot
# if (sites_plot_all == FALSE) {

#    sites_to_plot <- data.frame("name" = sites)
# }


# Quantitative data (overall) -----------------------------------------------------------------------

poly_quant_data <- data.frame(
  "PolygonStatus" = factor(c("Always Submerged at h.w.l.", "Cyclically Affected", "Always Exposed at l.w.l.", "Never Exposed", "Never Submerged"),
    levels = c("Always Submerged at h.w.l.", "Cyclically Affected", "Always Exposed at l.w.l.", "Never Exposed", "Never Submerged"),
    ordered = TRUE
  ),
  "MinWaterLevel" = colSums(tshr_polys_pct_info_min[, c("AlwaysSub", "Cyclically Affected", "AlwaysEm", "NeverEm", "NeverSub")]),
  "MaxWaterLevel" = colSums(tshr_polys_pct_info_max[, c("AlwaysSub", "Cyclically Affected", "AlwaysEm", "NeverEm", "NeverSub")]),
  row.names = NULL
)

# Subtract the values of the NeverEm and NeverSub sites from the ComplSub and NeverAff columns
poly_quant_data[1, 2:3] <- poly_quant_data[1, 2:3] - 18
poly_quant_data[3, 2:3] <- poly_quant_data[3, 2:3] - 16

poly_quant_data_l <- poly_quant_data %>%
  pivot_longer(cols = -PolygonStatus, names_to = "LakeStatus", values_to = "NumSites")

polygon_status_barplot <- ggplot(filter(poly_quant_data_l, NumSites > 0), mapping = aes(x = LakeStatus, y = NumSites, fill = PolygonStatus)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = .8) +
  geom_text(aes(label = NumSites),
    vjust = -0.25, color = "black",
    position = position_dodge(0.8), size = 3.5
  ) +
  scale_fill_manual(values = c("red", "yellow", "green", "black", "grey95")) +
  labs(
    title = "Status of Archaeological Sites in the Tishreen Reservoir - 2000-2023",
    caption = "Data from two images per year, one for each water level period",
    y = "Number of Sites", fill = "Sites Status"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    # legend.position = c(0.45, 0.93),
    # legend.direction = "horizontal",
    legend.title = element_blank()
  )

polygon_status_barplot

ggsave(
  filename = "status_polygons_barplot.png", plot = polygon_status_barplot, device = "png",
  path = here::here("output/plots"),
  width = 30,
  height = 18,
  units = "cm", dpi = 300
)

# Quantitative data (per year) -----------------------------------------------------------------------

# IMPORTANT: REMOVE SITES NEVER EMERGED AND NEVER SUBMERGED for the next plots

sites_to_remove <- tshr_polys_pct_info_min[which(tshr_polys_pct_info_min$NeverSub == 1 | tshr_polys_pct_info_min$NeverEm == 1), ] %>%
  select(starts_with("name"))

tshr_polys_pct_info_max_filtered <- tshr_polys_pct_info_max[!(tshr_polys_pct_info_max$Name %in% sites_to_remove$Name), ]
tshr_polys_pct_info_min_filtered <- tshr_polys_pct_info_min[!(tshr_polys_pct_info_min$Name %in% sites_to_remove$Name), ]


dahiti_df <- data.frame(
  "Years" = c(dahiti_water_level$date_min, dahiti_water_level$date_max),
  "Period" = c(
    rep("MinWaterLevel", length(dahiti_water_level$date_min)),
    rep("MaxWaterLevel", length(dahiti_water_level$date_max))
  ),
  "WaterLevel" = c(dahiti_water_level$water_level_min, dahiti_water_level$water_level_max)
)

get_timeseries_pct_numsites_all <- function(y, Period, WaterSurface) {
  a <- y %>%
    summarise(across(ends_with("_em"), list(sum = ~ sum(.x > 0, na.rm = T)))) %>%
    pivot_longer(everything(), names_to = "Years", values_to = "NumPolys") %>%
    mutate(Years = str_remove(Years, "_em_sum")) %>%
    mutate(Years = str_replace(Years, "_", "-")) %>%
    mutate(Period = Period) %>%
    cbind(., WaterSurface = WaterSurface)

  print(a)
}

polys_pct_area_numsites_all <- get_timeseries_pct_numsites_all(
  y = tshr_polys_pct_info_min_filtered,
  Period = "MinWaterLevel",
  WaterSurface = polygons_water_surface$water_surface_min
) %>%
  bind_rows(get_timeseries_pct_numsites_all(
    y = tshr_polys_pct_info_max_filtered,
    Period = "MaxWaterLevel",
    WaterSurface = polygons_water_surface$water_surface_max
  )) %>%
  mutate(Period = factor(Period)) %>%
  mutate(Years = factor(Years, ordered = TRUE))


polys_pct_area_numsites_all <- polys_pct_area_numsites_all %>%
  add_row(Years = "2012-01", NumPolys = NA, Period = "MinWaterLevel", WaterSurface = NA, .before = 13) %>%
  add_row(Years = "2012-06", NumPolys = NA, Period = "MaxWaterLevel", WaterSurface = NA, .before = 36)

polys_pct_time_series_plot_min_all <- ggplot(
  data = polys_pct_area_numsites_all,
  aes(Years, NumPolys, fill = Period)
) +
  geom_bar(width = 0.5, stat = "identity", alpha = .8, colour = "#737373") +
  # facet_rep_grid(~ Period, scales = "free") +
  scale_y_continuous(expand = c(0.01, 1)) +
  labs(y = "Number of Emerged Polygons") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8.3), axis.title.x = element_blank(),
    strip.text.x = element_text(size = 10, colour = "black"),
    text = element_text(size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11)
  )

polys_pct_time_series_plot_min_all

# split dataframe for water level plot
split_df <- split(polys_pct_area_numsites_all, polys_pct_area_numsites_all$Period)

split_dahiti_df <- split(dahiti_df, dahiti_df$Period)


# Add missing rows to plot the data correctly
split_dahiti_df$MaxWaterLevel <- split_dahiti_df$MaxWaterLevel %>%
  add_row(Years = head(split_df$MaxWaterLevel$Years, -5), Period = "MaxWaterLevel", WaterLevel = NA, .before = 1) %>%
  add_row(Years = last(split_df$MaxWaterLevel$Years), Period = "MaxWaterLevel", WaterLevel = NA)

split_dahiti_df$MinWaterLevel <- split_dahiti_df$MinWaterLevel %>%
  add_row(Years = head(split_df$MinWaterLevel$Years, -5), Period = "MinWaterLevel", WaterLevel = NA, .before = 1) %>%
  add_row(Years = last(split_df$MinWaterLevel$Years), Period = "MinWaterLevel", WaterLevel = NA)



# dahiti <- TRUE
dahiti <- FALSE

if (dahiti) {
  water_info <- split_dahiti_df
  max_aes <- aes(Years, WaterLevel, colour = "#F8776D", group = 1)
  min_aes <- aes(Years, WaterLevel, colour = "#00C0B8", group = 1)
  labels <- labs(y = "Reservoir Water Level (m - s.l.m. Approx.)")
  filename <- "number_of_polygons_per_year_water_level_comb.png"
  title <- "Tishreen Dam Water Surface and Number of Emerged Sites Polygons (2000-2023)"
  caption <- "Water Level Data: Database for Hydrological Time Series of Inland Waters (DAHITI)"
} else {
  water_info <- split_df
  max_aes <- aes(Years, WaterSurface, colour = "#F8776D", group = 1)
  min_aes <- aes(Years, WaterSurface, colour = "#00C0B8", group = 1)
  labels <- labs(y = "Reservoir Water Area (sqkm - Approx.)")
  filename <- "number_of_polygons_per_year_water_area_comb.png"
  title <- "Tishreen Dam Water Level and Number of Emerged Sites Polygons (2000-2023)"
  caption <- "Water Surface Data: Vectorized Reclassified Rasters"
}

water_level_plot <- ggplot() +
  geom_line(data = water_info$MaxWaterLevel, max_aes) +
  geom_line(data = water_info$MinWaterLevel, min_aes) +
  scale_y_continuous(expand = c(0.01, 1)) +
  scale_x_discrete(breaks = sort(polys_pct_area_numsites_all$Years)[seq(1, 48, 1)], guide = guide_axis(check.overlap = TRUE)) +
  scale_colour_identity(name = "Period", guide = "legend", labels = c("MinWaterLevel", "MaxWaterLevel")) +
  labels +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8.3), axis.title.x = element_blank(),
    strip.text.x = element_text(size = 10, colour = "black"),
    text = element_text(size = 10),
    legend.position = "NONE",
  )
water_level_plot

# No need for an intermediate patch, it will break the "collect" option in plot_layout()
patch <- water_level_plot / polys_pct_time_series_plot_min_all +
  plot_annotation(
    tag_levels = "A",
    title = title,
    caption = caption
  ) &
  theme(
    plot.title = element_text(size = 14),
    plot.caption = element_text(size = 8),
    plot.tag = element_text(size = 11)
  )

patch

ggsave(
  filename,
  plot = patch, device = "png",
  path = here::here("output/plots"), width = 240, height = 160, units = "mm", dpi = 300
)

# Selected sites plot -----------------------------------------------------------------------

# Min Water Level

sites_pct_area_min_filtered <- tshr_polys_pct_info_min_filtered %>%
  select(Name, Shape_Area, ends_with("_em")) %>% # Select only the columns with emerged percentage
  filter(tshr_polys_pct_info_max_filtered$Name %in% sites_to_plot$Name) %>%
  set_colnames(gsub(x = sub("X", "", colnames(.), fixed = TRUE), pattern = "*_em", replacement = "")) %>%
  set_colnames(gsub(colnames(.), pattern = "*_", replacement = "-")) # replace separators for dates

# Add dummy values for missing years to show them in the plots
sites_pct_area_min_filtered <- sites_pct_area_min_filtered %>%
  mutate("2012-01" = NA, .before = "2013-03") %>%
  rename(., "Shape_Area" = "Shape-Area")


sites_pct_area_min_long <- sites_pct_area_min_filtered %>%
  pivot_longer(-starts_with(c("Name", "Shape_Area")), names_to = "Years", values_to = "PctEmergedArea") %>%
  mutate(Name = factor(.$Name, levels = unique(.$Name), ordered = TRUE)) %>%
  mutate(label = paste(.$Name, " - Area: ", .$Shape_Area, " ha", sep = ""))

# Max Water Level

sites_pct_area_max_filtered <- tshr_polys_pct_info_max_filtered %>%
  select(Name, Shape_Area, ends_with("_em")) %>% # Select only the columns with emerged percentage
  filter(tshr_polys_pct_info_max_filtered$Name %in% sites_to_plot$Name) %>%
  set_colnames(gsub(x = sub("X", "", colnames(.), fixed = TRUE), pattern = "*_em", replacement = "")) %>%
  set_colnames(gsub(colnames(.), pattern = "*_", replacement = "-")) # replace separators for dates

# Add dummy values for missing years to show them in the plots
sites_pct_area_max_filtered <- sites_pct_area_max_filtered %>%
  mutate("2012-06" = NA, .before = "2013-08") %>%
  rename(., "Shape_Area" = "Shape-Area")


sites_pct_area_max_long <- sites_pct_area_max_filtered %>%
  pivot_longer(-starts_with(c("Name", "Shape_Area")), names_to = "Years", values_to = "PctEmergedArea") %>%
  mutate(Name = factor(.$Name, levels = unique(.$Name), ordered = TRUE)) %>%
  mutate(label = paste(.$Name, " - Area: ", .$Shape_Area, " ha", sep = ""))


# Plot data together
sites_pct_area_min_max_long <- sites_pct_area_min_long %>%
  mutate(Period = "MinWaterLevel") %>%
  bind_rows(mutate(sites_pct_area_max_long, Period = "MaxWaterLevel")) %>%
  mutate(Period = as.factor(Period))

# custom_levels <- c("Tell Banat - Area: 2 ha",
#                    "Tell Jurn Kebir - Area: 1 ha",
#                    "Tell Qadahiye - Area: 5 ha",
#                    "Tell Qara Qozak - Area: 14 ha",
#                    "Yusef Pasha - Area: 19 ha",
#                    "Tell Ahmar - Area: 71 ha",
#                    "Tell Qitar - Area: 29 ha",
#                    "Tell Qumluq - Area: 14 ha")

custom_levels <- c(
  "Tell Banat village - Area: 25 ha",
  "Tell Qara Qozak - Area: 14 ha",
  "Khirbet el Matbukh - Area: 0 ha",
  "Um Ruthah Tahtani - Area: 6 ha",
  "Tell Ahmar - Area: 71 ha",
  "Tell Kosak Shamali - Area: 3 ha"
)

sites_pct_area_min_max_long <- sites_pct_area_min_max_long %>%
  mutate(label = factor(label, levels = custom_levels, ordered = TRUE))


pct_area_min_max_plot <- ggplot(sites_pct_area_min_max_long, aes(x = Years, y = PctEmergedArea, fill = Period)) +
  geom_bar(width = 0.8, stat = "identity", position = "dodge", alpha = .8) +
  geom_hline(yintercept = c(25, 50, 75), linetype = "dashed", size = 0.5, color = "#4040401e") +
  scale_colour_identity(name = "Period", guide = "legend", labels = c("MinWaterLevel", "MaxWaterLevel")) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) + # Enforce same scale on the plots
  scale_fill_manual(values = c("grey26", "grey65")) +
  facet_wrap(~label, scales = "fixed", ncol = 2) +
  theme_light() +
  labs(
    y = "Percentage of Site Area Resurfaced",
    title = "Percentage of Site Area Resurfaced During Periods of Minimum and Maximum Water Level (2000-2023)"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5), axis.title.x = element_blank(),
    strip.text.x = element_text(size = 8, colour = "black"),
    # text = element_text(size = 12),
    axis.text.y = element_text(size = 12), legend.text = element_text(size = 12),
    plot.title = element_text(size = 12)
  ) +
  guides(x = guide_axis(angle = 60))

pct_area_min_max_plot

# prev width 380
ggsave(
  plot = pct_area_min_max_plot, filename = "percentage_emerged_area_selected_sites2.png",
  path = here::here("output/plots"),
  device = "png", width = 240, height = 160, units = "mm", dpi = 300
)

# Tables -----------------------------------------------------------------------

# Number of site per category per site type
poly_quant_data <- data.frame(
  "PolygonStatus" = factor(c("Always Submerged at h.w.l.", "Affected", "Always Exposed at l.w.l.", "Never Exposed", "Never Submerged"),
    levels = c("Always Submerged at h.w.l.", "Affected", "Always Exposed at l.w.l.", "Never Exposed", "Never Submerged"),
    ordered = TRUE
  ),
  "MinWaterLevel" = colSums(tshr_polys_pct_info_min[, c("AlwaysSub", "Affected", "AlwaysEm", "NeverEm", "NeverSub")]),
  "MaxWaterLevel" = colSums(tshr_polys_pct_info_max[, c("AlwaysSub", "Affected", "AlwaysEm", "NeverEm", "NeverSub")]),
  row.names = NULL
)

# Subtract the values of the NeverEm and NeverSub sites from the ComplSub and NeverAff columns
poly_quant_data[1, 2:3] <- poly_quant_data[1, 2:3] - 18
poly_quant_data[3, 2:3] <- poly_quant_data[3, 2:3] - 16

# Isolate the site always sub in the max water level and set its value of Affected as 1
always_sub_index <- which(tshr_polys_pct_info_max$AlwaysSub == 1 & tshr_polys_pct_info_max$NeverEm != 1)

tshr_polys_pct_info_max[always_sub_index, "Affected"] <- 1

tshr_polys_pct_info_max[always_sub_index, "AlwaysSub"] <- 0

site_type_quant <- tshr_polys_pct_info_max %>%
  mutate(Type = str_to_title(Type)) %>%
  group_by(Type) %>%
  summarise(
    Affected = sum(Affected),
    NeverEm = sum(NeverEm),
    NeverSub = sum(NeverSub)
  )

cols_df <- site_type_quant$Type

site_type_quant <- site_type_quant[2:4] %>%
  t() %>%
  set_colnames(cols_df)

write.csv(site_type_quant, here::here("output/csv/site_type_quant.csv"))

# Plot single sites -----------------------------------------------------------------------
# Selected sites plot -----------------------------------------------------------------------
# Filter for Tell Ahmar and Qara Kozak
sites_to_plot <- sites_to_plot %>%
  filter(sites_to_plot$Name %in% c("Tell Ahmar", "Tell Qara Qozak"))

custom_levels <- c(
  "Tell Qara Qozak - Area: 14 ha",
  "Tell Ahmar - Area: 71 ha"
)

sites_pct_area_min_max_long <- sites_pct_area_min_max_long %>%
  mutate(label = factor(label, levels = custom_levels, ordered = TRUE))


sites <- split(sites_pct_area_min_max_long, sites_pct_area_min_max_long$Name)

sites_plots <- list()

for (i in seq_along(sites)) {
  title <- paste0("Percentage of Site Area Resurfaced at the site of ", sites[[i]]$Name, "(2000-2023)")
  sites_plots[[i]] <- ggplot(sites[[i]], aes(x = Years, y = PctEmergedArea, fill = Period)) +
    geom_bar(width = 0.8, stat = "identity", position = "dodge", alpha = .8) +
    geom_hline(yintercept = c(25, 50, 75), linetype = "dashed", size = 0.5, color = "#4040401e") +
    scale_colour_identity(name = "Period", guide = "legend", labels = c("MinWaterLevel", "MaxWaterLevel")) +
    scale_fill_manual(values = c("grey26", "grey65")) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) + # Enforce same scale on the plots
    facet_wrap(~label, scales = "fixed", ncol = 2) +
    theme_light() +
    labs(
      y = "Percentage of Site Area Resurfaced",
      title = title
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 5), axis.title.x = element_blank(),
      strip.text.x = element_text(size = 8, colour = "black"),
      # text = element_text(size = 12),
      axis.text.y = element_text(size = 12), legend.text = element_text(size = 12),
      plot.title = element_text(size = 12)
    ) +
    guides(x = guide_axis(angle = 60))

  name <- unique(sites[[i]]$Name)

  ggsave(
    plot = sites_plots[[i]], filename = paste0("percentage_emerged_area_", name, ".png"),
    path = here::here("output/plots"),
    device = "png", width = 240, height = 160, units = "mm", dpi = 300
  )
}
