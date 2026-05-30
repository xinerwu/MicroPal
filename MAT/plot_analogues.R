# plot the location of the n best analogues for each sample

library(ggplot2)
library(tidyr)
library(dplyr)
library(sf)
library(ggforce)
library(rnaturalearth)
library(tibble)

# Specify the number of analogues used
#n.analogues <- 5

# Specify the unit of sample ID (eg. cm, ka BP, year BP)
sample_id_unit <- "ka BP"

# Enter your core site
core_lon <- -58.03
core_lat <- 61.46
 
# Import the analogue file
ana_file_name <- 'MATrecons/P4_analogues.txt'
# Import database coordinates file
coor <- read.delim('coor1968.txt',header = TRUE)

################# No modification necessary below this line ####################
######################## Unless you know what to do ############################

# OUtput file name
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

# Create the faceted map
map <- ggplot() +
  # Add the world map background first
  geom_sf(data = world, fill = "gray90", color = "white") +
  # Add the core site
  geom_sf(data = reference_point_sf, color = "red", size = 3, shape = 4) +
  # Add your specific analogue sites on top
  geom_sf(data = mapping_sf, aes(color = distance), size = 2) +
  # Focus on the DB region
  coord_sf(ylim = c(min(coor$Latitude), 90)) +
  ## Split the maps by sample_id
  #facet_wrap(~ sample_id, labeller = as_labeller(function(id) paste(id, sample_id_unit))) +
  # The continuous colormap
  scale_color_viridis_c(option = "viridis", name = "Analogue\nDistance") +
  #theme_minimal() +
  theme_bw() +
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_blank(),
    strip.background = element_rect(fill = "gray20"),
    strip.text = element_text(color = "white", face = "bold"),
    legend.position = "bottom"
  ) +
  labs(title = "Closest 5 Analogues per Sample",
       subtitle = "Blue diamond indicates core site")

# Set up grid for a single page
page_rows <- 8
page_cols <- 3

# We build a temporary plot just to let ggforce do the math
temp_plot <- map + 
  facet_wrap_paginate(
    ~ sample_id, 
    ncol = page_cols, 
    nrow = page_rows, 
    page = 1
  )

total_pages <- n_pages(temp_plot)

# Export to PDF
pdf(out_name, width = 8.5, height = 11)

# Loop through the pages and print them to the PDF
for (i in 1:total_pages) {
  
  # Add the paginated facet layer for the current page 'i'
  p <- map +
    facet_wrap_paginate(
      ~ sample_id, 
      ncol = page_cols, 
      nrow = page_rows, 
      labeller = as_labeller(function(id) paste(id, sample_id_unit)),
      page = i
    )
  
  # Print sends the plot to the open PDF device (creating a new page)
  print(p)
  
  # Optional: Print progress to the console
  message(sprintf("Printing page %d of %d...", i, total_pages))
}

# 6. Close the PDF device (CRITICAL STEP)
# If you don't run dev.off(), the file will be locked and corrupted
dev.off()

message("Map of the analogues successfully generated!")
message(out_name)
