################################################################################
# 05_mapping.R
#
# This script aggregates conservation site data for mapping, converts the data 
# to an sf object, downloads and crops a base UK map to Scotland, and produces 
# several maps using ggplot2 with custom colour scales for conservation outcomes 
# or designations.
################################################################################

#-----------------------------------------------
# Load Required Libraries for Mapping
#-----------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)  
library(forcats)   

#-----------------------------------------------

# assuming dataformap dataset is already loaded

#-----------------------------------------------
# 1. Aggregate Data by Site and Compute Mode of PAImpact
#-----------------------------------------------
# (Assumes that get_mode() is already defined in your session)
sites_mode <- dataformap %>%
  group_by(SiteCode, SiteName, Longitude, Latitude, DESIG) %>%
  summarise(PAImpact_mode = get_mode(PAImpact),
            .groups = "drop")

#-----------------------------------------------
# 2. Convert Aggregated Data to an sf Object
#-----------------------------------------------
# Using the coordinates (Longitude, Latitude) and the WGS84 CRS (EPSG:4326)
sites_sf <- st_as_sf(sites_mode, coords = c("Longitude", "Latitude"), crs = 4326)

#-----------------------------------------------
# 3. Download and Crop a UK Map to Scotland
#-----------------------------------------------
# Download the UK map at medium scale from rnaturalearth
uk_map <- ne_countries(scale = "medium", country = "United Kingdom", returnclass = "sf")
# Crop the map to an approximate bounding box for Scotland (adjust as needed)
scotland_map <- st_crop(uk_map, xmin = -8, xmax = -0.5, ymin = 55.2, ymax = 59)

#-----------------------------------------------
# 4. Define Custom Colours for Conservation Outcome or Designation
#-----------------------------------------------
# Example for PAImpact categories:
impact_colours <- c(
  "Negative Impact" = "sienna3",
  "No Impact"       = "mediumpurple2",
  "Positive Impact" = "#008080"
)
# (Optional) Define colours for designations (DESIG)
designation_colours <- c(
  "LNR"    = "#1b9e77",   # Local Nature Reserve
  "SSSI"   = "#d95f02",   # Site of Special Scientific Interest
  "National Park" = "#7570b3",
  "Regional Park" = "#e7298a",
  "SPA"    = "#66a61e",   # Special Protection Area
  "Ramsar" = "#e6ab02",
  "SCI"    = "#a6761d"    # Site of Community Importance
)

#-----------------------------------------------
# 5. Create Maps
#-----------------------------------------------

## Map 1: Conservation Outcome (PAImpact_mode)
map_outcome <- ggplot() +
  geom_sf(data = scotland_map, fill = "gray90", colour = "black") +
  geom_sf(data = sites_sf, aes(colour = PAImpact_mode), size = 1, alpha = 0.8) +
  scale_colour_manual(values = impact_colours) +
  labs(colour = "Conservation Outcome") +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(0.4, "cm"),
        legend.text  = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"))
print(map_outcome)

# Save Map 1 to file
ggsave(filename = "map_outcome.png", plot = map_outcome, width = 6, height = 4, dpi = 300)


## Map 2: Primary Objective Map (Optional)
# Extract unique site-level data for PrimaryObjective if available
sites_unique <- dataformap %>%
  distinct(SiteCode, SiteName, PrimaryObjective, Longitude, Latitude)
sites_sf_unique <- st_as_sf(sites_unique, coords = c("Longitude", "Latitude"), crs = 4326)

map_primary <- ggplot() +
  geom_sf(data = scotland_map, fill = "gray90", colour = "black") +
  geom_sf(data = sites_sf_unique, aes(colour = PrimaryObjective), size = 0.8, alpha = 0.6) +
  scale_colour_manual(values = c("navyblue", "#D95F02", "goldenrod3", "#E7298A", "darkgreen")) +
  scale_x_continuous(breaks = seq(-8, -2, by = 2), labels = function(x) paste0(abs(x), "°W")) +
  labs(colour = "Primary Objective") +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"))
print(map_primary)

# Save Map 2 to file
ggsave(filename = "map_primary_objective.png", plot = map_primary, width = 6, height = 4, dpi = 300)


# Count occurrences of each designation across all sites
designation_counts <- table(sites_sf$DESIG)
# Display the counts
print(designation_counts)


## Map 3: Designation Map
# Ensure DESIG is a factor for ordering and colouring
sites_sf$DESIG <- as.factor(sites_sf$DESIG)

designation_colours <- c(
  "Local Nature Reserve" = "#1b9e77",  
  "Site Of Special Scientific Interest (Uk)" = "#d95f02",  
  "National Park" = "#7570b3",  
  "Ramsar Site" = "#e6ab02",  
  "Regional Park" = "#e7298a",  
  "Site of Community Importance (Habitats Directive)" = "#e6ab02", 
  "Special Protection Area (Birds Directive)" = "#1f78b4"         
)

map_designation <- ggplot() +
  geom_sf(data = scotland_map, fill = "gray90", colour = "black") +
  geom_sf(data = sites_sf, aes(colour = DESIG), size = 0.8, alpha = 0.6) +
  scale_colour_manual(values = designation_colours) +
  scale_x_continuous(breaks = seq(-8, -2, by = 2),
                     labels = function(x) paste0(abs(x), "°W")) +
  labs(colour = "PA Designation") +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7))
print(map_designation)




## Optional Map with Additional Cartographic Elements
map_with_elements <- ggplot() +
  geom_sf(data = scotland_map, fill = "grey95", colour = "black") +
  geom_sf(data = sites_sf, aes(colour = DESIG), size = 2, alpha = 0.9) +
  scale_colour_brewer(palette = "Set2", name = "Designation") +
  theme_classic() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold", colour = "black"),
        plot.subtitle = element_text(size = 10, colour = "black")) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)
print(map_with_elements)



################################################################################
# End of 05_mapping.R
################################################################################
