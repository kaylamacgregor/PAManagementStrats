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
# Assumes `dataformap` and `get_mode()` are already defined
#-----------------------------------------------

#-----------------------------------------------
# 1. Aggregate Data by Site and Compute Modes
#-----------------------------------------------
sites_mode <- dataformap %>%
  group_by(SiteCode, SiteName, Longitude, Latitude, DESIG) %>%
  summarise(
    PAImpact_mode        = get_mode(PAImpact),
    Interventions_mode   = get_mode(Interventions),
    SpeciesFocus_mode    = get_mode(SpeciesFocus),
    PrimaryObjective     = get_mode(PrimaryObjective),
    
    .groups = "drop"
  )

#-----------------------------------------------
# 2. Convert Aggregated Data to sf Object
#-----------------------------------------------
sites_sf <- st_as_sf(sites_mode, coords = c("Longitude", "Latitude"), crs = 4326)

#-----------------------------------------------
# 3. Download and Crop a UK Map to Scotland
#-----------------------------------------------
uk_map <- ne_countries(scale = "medium", country = "United Kingdom", returnclass = "sf")
scotland_map <- st_crop(uk_map, xmin = -8, xmax = -0.5, ymin = 55.2, ymax = 59)

#-----------------------------------------------
# 4. Define Custom Colour Palettes
#-----------------------------------------------

# For PAImpact
impact_colours <- c(
  "Negative Impact" = "sienna3",
  "No Impact"       = "mediumpurple2",
  "Positive Impact" = "#008080"
)

# For Designations (used in Map 3)
designation_colours <- c(
  "Local Nature Reserve" = "#1b9e77",  
  "Site Of Special Scientific Interest (Uk)" = "#d95f02",  
  "National Park" = "#7570b3",  
  "Ramsar Site" = "#e6ab02",  
  "Regional Park" = "#e7298a",  
  "Site of Community Importance (Habitats Directive)" = "#e6ab02", 
  "Special Protection Area (Birds Directive)" = "#1f78b4"
)

# For Interventions
intervention_colours <- c(
  "Adaptive"     = "coral",  
  "Non-Adaptive" = "#1f78b4" 
)


speciesfocus_colours <- c(
  "Waterbird"       = "coral",
  "MultiSp"     = "#1f78b4"
)

# in line with barchart colours (fig 4)


#-----------------------------------------------
# 5. Map 1: Conservation Outcome (PAImpact)
#-----------------------------------------------
map_outcome <- ggplot() +
  geom_sf(data = scotland_map, fill = "gray90", colour = "black") +
  geom_sf(data = sites_sf, aes(colour = PAImpact_mode), size = 1, alpha = 0.8) +
  scale_colour_manual(values = impact_colours) +
  labs(colour = "Conservation Impact") +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    legend.text  = element_text(size = 7, colour = "black"),
    legend.title = element_text(size = 7, colour = "black")
  )
ggsave("map_impact.png", map_outcome, width = 6, height = 4, dpi = 300)
print(map_outcome)

#-----------------------------------------------
# 6. Map 2: Primary Objective
#-----------------------------------------------
map_primary <- ggplot() +
  geom_sf(data = scotland_map, fill = "gray90", colour = "black") +
  geom_sf(data = sites_sf, aes(colour = PrimaryObjective), size = 0.8, alpha = 0.6) +
  scale_colour_manual(values = c("navyblue", "#D95F02", "goldenrod3", "#E7298A", "darkgreen")) +
  scale_x_continuous(breaks = seq(-8, -2, by = 2), labels = function(x) paste0(abs(x), "°W")) +
  labs(colour = "Primary Objective") +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7, colour = "black"),
    legend.title = element_text(size = 7, colour = "black")
  )
ggsave("map_primary_objective.png", map_primary, width = 6, height = 4, dpi = 300)
print(map_primary)

#-----------------------------------------------
# 7. Map 3: Designation Map
#-----------------------------------------------
sites_sf$DESIG <- as.factor(sites_sf$DESIG)

map_designation <- ggplot() +
  geom_sf(data = scotland_map, fill = "gray90", colour = "black") +
  geom_sf(data = sites_sf, aes(colour = DESIG), size = 0.8, alpha = 0.6) +
  scale_colour_manual(values = designation_colours) +
  scale_x_continuous(breaks = seq(-8, -2, by = 2), labels = function(x) paste0(abs(x), "°W")) +
  labs(colour = "PA Designation") +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7, colour = "black"),
    legend.title = element_text(size = 7, colour = "black"),
    axis.text = element_text(size = 7)
  )
print(map_designation)

# Count and print designation frequencies
designation_counts <- table(sites_sf$DESIG)
print(designation_counts)

#-----------------------------------------------
# 8. Map 4: Interventions
#-----------------------------------------------
map_intervention <- ggplot() +
  geom_sf(data = scotland_map, fill = "gray90", colour = "black") +
  geom_sf(data = sites_sf, aes(colour = Interventions_mode), size = 0.8, alpha = 0.6) +
  scale_colour_manual(values = intervention_colours) +
  scale_x_continuous(breaks = seq(-8, -2, by = 2), labels = function(x) paste0(abs(x), "°W")) +
  labs(colour = "Intervention Type") +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7, colour = "black"),
    legend.title = element_text(size = 7, colour = "black"),
    axis.text = element_text(size = 7)
  )
print(map_intervention)

#-----------------------------------------------
# 9. Map 5: Species Focus
#-----------------------------------------------

map_speciesfocus <- ggplot() +
  geom_sf(data = scotland_map, fill = "gray90", colour = "black") +
  geom_sf(data = sites_sf, aes(colour = SpeciesFocus_mode), size = 0.8, alpha = 0.6) +
  scale_colour_manual(values = speciesfocus_colours) +
  scale_x_continuous(breaks = seq(-8, -2, by = 2),
                     labels = function(x) paste0(abs(x), "°W")) +
  labs(colour = "Species Focus") +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7, colour = "black"),
    legend.title = element_text(size = 7, colour = "black"),
    axis.text = element_text(size = 7)
  )

print(map_speciesfocus)


#save the plots
ggsave("map_int.png", map_intervention, width = 6, height = 4, dpi = 300)
ggsave("map_species_focus.png", map_speciesfocus, width = 6, height = 4, dpi = 300)


################################################################################
# End of 05_mapping.R
################################################################################
