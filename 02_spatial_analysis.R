################################################################################
# Spatial Autocorrelation using Mantel Tests for birddata
# Variables: PAImpact (Conservation Outcome), PrimaryObjective, Interventions,
#            and SpeciesFocus
# this script assumed dataformap dataset is already loaded from script 01_data_processing.R
#
# For each variable:
#   1. Aggregate data by site (using SiteName) – assuming that Longitude and Latitude
#      are constant within a site.
#   2. Compute an ecological dissimilarity matrix using Gower’s distance.
#   3. Compute a geographic distance matrix (in metres) using the geosphere package.
#   4. Run a Mantel test (Spearman correlation, 999 permutations) to assess spatial
#      autocorrelation.
################################################################################

# Load required libraries
library(dplyr)
library(cluster)     # For daisy() to calculate Gower's distance
library(geosphere)   # For distm() to compute geographic distances
library(vegan)       # For mantel()

# Helper function to calculate the mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

### 1. Conservation Outcome (PAImpact)
# Aggregate by site 
site_outcome <- dataformap %>%
  group_by(SiteName) %>%
  summarise(mode_PAImpact = get_mode(PAImpact),
            Longitude = first(Longitude),
            Latitude = first(Latitude)) %>%
  ungroup()

# Ecological dissimilarity matrix (convert PAImpact to factor)
impact_dist <- daisy(data.frame(PAImpact = as.factor(site_outcome$mode_PAImpact)),
                     metric = "gower")

# Geographic distance matrix (in metres)
coords_outcome <- as.matrix(site_outcome[, c("Longitude", "Latitude")])
geo_dist_matrix_outcome <- distm(coords_outcome)
geo_dist_outcome <- as.dist(geo_dist_matrix_outcome)

# Run Mantel test for PAImpact
mantel_result_PA <- mantel(impact_dist, geo_dist_outcome, method = "spearman", permutations = 999)
print(mantel_result_PA)


### 2. PrimaryObjective
# Aggregate data by site – select the most frequent PrimaryObjective per SiteName
site_primary <- dataformap %>%
  group_by(SiteName) %>%
  summarise(PrimaryObjective = names(sort(table(PrimaryObjective), decreasing = TRUE))[1],
            Longitude = first(Longitude),
            Latitude = first(Latitude)) %>%
  ungroup()

# Compute the ecological dissimilarity matrix (convert PrimaryObjective to factor)
primary_dist <- daisy(data.frame(PrimaryObjective = as.factor(site_primary$PrimaryObjective)),
                      metric = "gower")

# Compute geographic distance matrix for PrimaryObjective
coords_primary <- as.matrix(site_primary[, c("Longitude", "Latitude")])
geo_dist_matrix_primary <- distm(coords_primary)
geo_dist_primary <- as.dist(geo_dist_matrix_primary)

# Run Mantel test for PrimaryObjective
mantel_result_primary <- mantel(primary_dist, geo_dist_primary, method = "spearman", permutations = 999)
print(mantel_result_primary)


### 3. Interventions
# Aggregate data by site – select the most common Interventions per SiteName
site_interventions <- dataformap %>%
  group_by(SiteName) %>%
  summarise(Interventions = names(which.max(table(Interventions))),
            Longitude = first(Longitude),
            Latitude = first(Latitude)) %>%
  ungroup()

# Compute the ecological dissimilarity matrix (convert Interventions to factor)
intervention_dist <- daisy(data.frame(Interventions = as.factor(site_interventions$Interventions)),
                           metric = "gower")

# Compute geographic distance matrix for Interventions
coords_int <- as.matrix(site_interventions[, c("Longitude", "Latitude")])
geo_dist_matrix_int <- distm(coords_int)
geo_dist_int <- as.dist(geo_dist_matrix_int)

# Run Mantel test for Interventions
mantel_result_interventions <- mantel(intervention_dist, geo_dist_int, method = "spearman", permutations = 999)
print(mantel_result_interventions)


### 4. SpeciesFocus
# Aggregate data by site 
site_speciesfocus <- dataformap%>%
  group_by(SiteName) %>%
  summarise(SpeciesFocus = names(which.max(table(SpeciesFocus))),
            Longitude = first(Longitude),
            Latitude = first(Latitude)) %>%
  ungroup()

# Compute the ecological dissimilarity matrix (convert SpeciesFocus to factor)
speciesfocus_dist <- daisy(data.frame(SpeciesFocus = as.factor(site_speciesfocus$SpeciesFocus)),
                           metric = "gower")

# Compute geographic distance matrix for SpeciesFocus
coords_spf <- as.matrix(site_speciesfocus[, c("Longitude", "Latitude")])
geo_dist_matrix_spf <- distm(coords_spf)
geo_dist_spf <- as.dist(geo_dist_matrix_spf)

# Run Mantel test for SpeciesFocus
mantel_result_speciesfocus <- mantel(speciesfocus_dist, geo_dist_spf, method = "spearman", permutations = 999)
print(mantel_result_speciesfocus)


################################################################################
# Correlograms
################################################################################



# --- 1. Conservation Outcome (PAImpact) Correlogram ---
site_outcome <- dataformap %>%
  group_by(SiteName, Longitude, Latitude) %>%
  summarise(PAImpact_mode = get_mode(PAImpact), .groups = "drop")

impact_dist <- daisy(data.frame(PAImpact = as.factor(site_outcome$PAImpact_mode)), metric = "gower")

coords <- as.matrix(site_outcome[, c("Longitude", "Latitude")])
geo_dist_matrix <- distm(coords)
geo_dist_km <- as.dist(geo_dist_matrix / 1000)

mantel_corr_outcome <- mantel.correlog(D.eco = impact_dist, D.geo = geo_dist_km, n.class = 6)
plot(mantel_corr_outcome, main = "Mantel Correlogram: Conservation Outcome (km)")

# --- 2. PrimaryObjective Correlogram ---
site_primary <- dataformap %>%
  group_by(SiteName, Longitude, Latitude) %>%
  summarise(PrimaryObjective = names(sort(table(PrimaryObjective), decreasing = TRUE))[1], .groups = "drop")

primary_dist <- daisy(data.frame(PrimaryObjective = as.factor(site_primary$PrimaryObjective)), metric = "gower")

coords_primary <- as.matrix(site_primary[, c("Longitude", "Latitude")])
geo_dist_primary_m <- distm(coords_primary)
geo_dist_primary_km <- as.dist(geo_dist_primary_m / 1000)

mantel_corr_primary <- mantel.correlog(D.eco = primary_dist, D.geo = geo_dist_primary_km, n.class = 6)
plot(mantel_corr_primary, main = "Mantel Correlogram: Primary Objective (km)")

# --- 3. Interventions Correlogram ---
site_interventions <- dataformap %>%
  group_by(SiteName, Longitude, Latitude) %>%
  summarise(Interventions = names(which.max(table(Interventions))), .groups = "drop")

intervention_dist <- daisy(data.frame(Interventions = as.factor(site_interventions$Interventions)), metric = "gower")

coords_int <- as.matrix(site_interventions[, c("Longitude", "Latitude")])
geo_dist_int_m <- distm(coords_int)
geo_dist_int_km <- as.dist(geo_dist_int_m / 1000)

mantel_corr_interventions <- mantel.correlog(D.eco = intervention_dist, D.geo = geo_dist_int_km, n.class = 6)
plot(mantel_corr_interventions, main = "Mantel Correlogram: Interventions (km)")

# --- 4. SpeciesFocus Correlogram ---
site_speciesfocus <- dataformap %>%
  group_by(SiteName, Longitude, Latitude) %>%
  summarise(SpeciesFocus = names(which.max(table(SpeciesFocus))), .groups = "drop")

speciesfocus_dist <- daisy(data.frame(SpeciesFocus = as.factor(site_speciesfocus$SpeciesFocus)), metric = "gower")

coords_spf <- as.matrix(site_speciesfocus[, c("Longitude", "Latitude")])
geo_dist_spf_m <- distm(coords_spf)
geo_dist_spf_km <- as.dist(geo_dist_spf_m / 1000)

mantel_corr_speciesfocus <- mantel.correlog(D.eco = speciesfocus_dist, D.geo = geo_dist_spf_km, n.class = 6)
plot(mantel_corr_speciesfocus, main = "Mantel Correlogram: Species Focus (km)")


