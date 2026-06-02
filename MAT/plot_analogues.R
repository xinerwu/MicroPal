# Plot the location of the n best analogues for each sample
# The expected format for the coordinates of your database points is a table 
# with 3 columns, the column headers being: stations, Latitude, Longitude
# The order of the columns can be different. R is case-sensitive.

library(ggplot2)
library(tidyr)
library(dplyr)
library(sf)
library(patchwork)
library(rnaturalearth)
library(tibble)

# Specify the unit of sample ID (eg. cm, ka BP, year BP)
sample_id_unit <- "ka BP"

# Enter your core site
core_lon <- -58.03
core_lat <- 61.46

# Import the analogue file
ana_file_name <- 'MATrecons/P4_analogues.txt'
# Import database coordinates file
coor <- read.delim('coor1968.txt',header = TRUE)

# Set map area
ylim <- c(min(coor$Latitude), 90)
xlim <- c(-180, 180)

################# No modification necessary below this line ####################
######################## Unless you know what to do ############################

# Output file name
out_name <- ana_file_name %>% sub("_.*", "",x=.) %>% paste0("_analogue_sites_map.pdf")

# Read analogue site name and dissimilarity
all_lines <- readLines(ana_file_name)
lines_to_skip <- grep("Top n Closest Sites", all_lines)
#lines_to_skip <- target_line + 1
analogues_name <- read.table(ana_file_name, skip=lines_to_skip[1], 
                             header = TRUE, check.names = FALSE, fill = TRUE, 
                             stringsAsFactors = FALSE,
                             nrows = lines_to_skip[2]-lines_to_skip[1]-4)
analogues_dist <- read.table(ana_file_name, skip=lines_to_skip[2], 
                             header = TRUE, check.names = FALSE, fill = TRUE, 
                             stringsAsFactors = FALSE)

# Pivot to long format
sites_long <- analogues_name %>%
  rownames_to_column(var = "sample_id") %>%
  pivot_longer(
    cols = -sample_id,            # Keep sample_id fixed
    names_to = "k",               # The column headers (1,2,3,...) go here
    values_to = "stations"        # The site IDs ("224", "210"...) go here
  ) %>%
  # Convert stations to character to make joining with coordinate data easier later
  mutate(k = as.numeric(k),stations = as.character(stations))
dist_long <- analogues_dist %>%
  rownames_to_column(var = "sample_id") %>%
  pivot_longer(
    cols = -sample_id,
    names_to = "k",
    values_to = "distance"
  ) %>%
  mutate(k = as.numeric(k))

coor <- coor %>% mutate(stations = as.character(stations))

# Join extracted sites with BD coordinate data
mapping_data <- sites_long %>%
  left_join(coor, by = "stations")

# Convert to a spatial 'sf' object for ggplot2 mapping
mapping_sf <- st_as_sf(mapping_data, coords = c("Longitude", "Latitude"), crs = 4326)
mapping_sf <- mapping_sf %>%
  left_join(dist_long, by = c("sample_id", "k"))

missing_coords <- mapping_data %>% filter(is.na(Latitude) | is.na(Longitude))
print(missing_coords)

# Find the global min and max of your distance metric
min_dist <- min(mapping_sf$distance, na.rm = TRUE)
max_dist <- max(mapping_sf$distance, na.rm = TRUE)

#world <- ne_countries(scale = "medium", returnclass = "sf")
world <- ne_download(scale = "medium", type = "land", category = "physical", returnclass = "sf")
reference_point_sf <- st_as_sf(
  data.frame(site_name = "Core", Longitude = core_lon, Latitude = core_lat),
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# Convert the character strings back into numbers so they sort mathematically
mapping_sf <- mapping_sf %>%
  mutate(sample_id = round(as.numeric(sample_id),3))

plot_list <- list()
unique_samples <- sort(unique(mapping_sf$sample_id))

# Create the maps one by one and store in the list
for (current_sample in unique_samples) {
  single_sample_data <- mapping_sf %>%
    filter(sample_id == current_sample)
  p <- ggplot() +
    # Add the world map background first
    geom_sf(data = world, fill = "gray90", color = "white") +
    # Add the core site
    geom_sf(data = reference_point_sf, color = "red", size = 2.5, shape = 4) +
    # Add your specific analogue sites on top
    geom_sf(data = single_sample_data, aes(color = distance), size = 1.5) +
    # Focus on the DB region
    coord_sf(ylim = ylim, xlim = xlim) +
    # The continuous colormap
    scale_color_viridis_c(option = "viridis", name = "Analogue\nDistance",
                          limits = c(min_dist, max_dist)) +
    theme_bw() +
    labs(title = paste(current_sample, sample_id_unit)) +
    theme(
      axis.text = element_text(size = 6),
      axis.title = element_blank(),
      plot.title = element_text(size = 9, face = "bold"),
    )
  plot_list[[as.character(current_sample)]] <- p
}

plots_per_page <- 24
chunked_plots <- split(plot_list, ceiling(seq_along(plot_list) / plots_per_page))

# Prepare a blank map as a place holder to avoid shrinking problem
empty_sf <- st_sf(st_drop_geometry(mapping_sf[0, ]), geometry = st_sfc(crs = 4326))
empty_world <- world[0, ]
empty_ref <- reference_point_sf[0, ]

blank_map <- ggplot() +
  geom_sf(data = empty_world, fill = "gray90", color = "white") +
  geom_sf(data = empty_ref, color = "red", size = 2.5, shape = 4) +
  geom_sf(data = empty_sf, aes(color = distance)) +
  coord_sf(ylim = ylim, xlim = xlim) +
  scale_color_viridis_c(option = "viridis", name = "Analogue\nDistance",
                        limits = c(min_dist, max_dist)) +
  theme_bw() +
  labs(title = "Ghost") +
  theme(
    axis.text      = element_text(size = 6, color = "transparent"),
    axis.ticks     = element_line(color = "transparent"),
    plot.title     = element_text(size = 9, color = "transparent"),
    legend.position = "none",
    panel.background = element_rect(fill = "transparent"),
    panel.border = element_blank(),
    panel.grid     = element_blank()
  )

# Export to PDF
pdf(out_name, width = 8.5, height = 11)

# Loop through the pages and print them to the PDF
for (i in seq_along(chunked_plots)) {
  page_plots <- chunked_plots[[i]]
  
  if (length(page_plots) < plots_per_page) {
    empty_slots <- plots_per_page - length(page_plots)
    
    for (j in 1:empty_slots) {
      page_plots[[length(page_plots) + 1]] <- blank_map # put a blank map in empty slots
    }
  }
  
  page_grid <- wrap_plots(page_plots,ncol = 3, nrow = 8) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "Closest 5 Analogues per Sample",
      subtitle = "Red cross indicates core site"
    ) &
    theme(legend.position = "bottom")
  print(page_grid)
  message(sprintf("Printing page %d of %d...", i, length(chunked_plots)))
}

# 6. Close the PDF device
dev.off()

message("Map of the analogues successfully generated!")
message(out_name)
