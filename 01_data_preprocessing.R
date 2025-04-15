################################################################################
# 01_data_preprocessing.R
#
# This script loads and cleans two datasets from an Excel file:
#   - birddata: used for modelling analyses (from Sheet16)
#   - dataformap: used for spatial analyses and mapping (from Sheet12)
#
# NB: Most data processing/input was done in Excel prior to importing.
#
# Both datasets are processed and stored in separate objects.
################################################################################




# Set working directory 
setwd("~/Documents/diss/r studio/code")

#-------------------------------
# Load required libraries
#-------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#-------------------------------
# Load and clean the birddata dataset (Sheet16)
#-------------------------------
birddata <- read_excel("~/Documents/diss/categories/new table.xlsx", sheet = "Sheet16")

# Set NA handling so that NA values are excluded during modelling
options(na.action = "na.exclude")


# Define species of interest
threatened_species <- c("Eurasian Oystercatcher", "Northern Lapwing")
non_threatened_species <- c("Common Teal", "Mute Swan")

# Ensure that PAImpact is an ordered factor and create a SpeciesStatus column
birddata <- birddata %>%
  mutate(
    PAImpact = factor(PAImpact, levels = c("Negative Impact", "No Impact", "Positive Impact")),
    SpeciesStatus = case_when(
      CommonName %in% threatened_species ~ "Threatened",
      CommonName %in% non_threatened_species ~ "Non-Threatened",
      TRUE ~ NA_character_    # Or "Other" if you want a label for non-target species
    )
  )

# check results 
head(birddata[, c("CommonName", "SpeciesStatus")])



birddata <- birddata %>% 
  mutate(
    SiteCode         = as.factor(SiteCode),
    Species          = as.factor(Species),
    PAImpact         = factor(PAImpact, levels = c("Negative Impact", "No Impact", "Positive Impact"), ordered = TRUE),
    CommonName       = as.character(CommonName),
    MigStatus        = as.factor(MigStatus),
    RedList          = as.factor(RedList),
    Diet             = as.factor(Diet),
    SiteName         = as.factor(SiteName),
    PrimaryObjective = as.factor(PrimaryObjective),
    Interventions    = as.factor(Interventions),
    OverallImpact    = as.factor(OverallImpact),
    SpeciesFocus     = as.factor(SpeciesFocus),
    WaterbirdType    = as.factor(WaterbirdType),
    ManagedFor       = as.factor(ManagedFor),
    SpeciesStatus    = as.factor(SpeciesStatus),
    Target           = as.factor(Target),
    SpecificTargetSp = as.factor(SpecificTargetSp)
  )

# Recode Diet values
birddata$Diet <- gsub("PlantSeed", "Herbivore", birddata$Diet)
birddata$Diet <- gsub("VertFishScav", "Scavenger", birddata$Diet)
birddata$Diet <- gsub("Invertebrate", "Carnivore", birddata$Diet)
# (Reassign factor to Diet after recoding)
birddata <- birddata %>%
  mutate(Diet = as.factor(Diet))

#-------------------------------
# Load and clean the dataformap dataset (Sheet12)
#-------------------------------
dataformap <- read_excel("~/Documents/diss/categories/new table.xlsx", sheet = "Sheet12")

dataformap <- dataformap %>%
  mutate(
    SiteCode         = as.character(SiteCode),
    Species          = as.character(Species),
    PAImpact         = factor(PAImpact, levels = c("Negative Impact", "No Impact", "Positive Impact"), ordered = TRUE),
    CommonName       = as.character(CommonName),
    Family           = as.character(Family),
    Order            = as.character(Order),
    MigStatus        = as.factor(MigStatus),
    RedList          = as.factor(RedList),
    Diet             = as.factor(Diet),
    SiteName         = as.character(SiteName),
    Country          = as.character(Country),
    Longitude        = as.numeric(Longitude),
    Latitude         = as.numeric(Latitude),
    IUCN_CAT         = as.factor(IUCN_CAT),
    WDPAID           = as.character(WDPAID),
    STATUS_YR        = as.integer(STATUS_YR),
    DESIG            = as.character(DESIG),
    PrimaryObjective = as.factor(PrimaryObjective),
    Interventions    = as.factor(Interventions),
    SpeciesFocus     = as.factor(SpeciesFocus),
    OverallImpact    = factor(OverallImpact, levels = c("Negative", "Neutral", "Positive"))
  )

# Recode Diet values in dataformap
dataformap$Diet <- gsub("PlantSeed", "Herbivore", dataformap$Diet)
dataformap$Diet <- gsub("VertFishScav", "Scavenger", dataformap$Diet)
dataformap$Diet <- gsub("Invertebrate", "Carnivore", dataformap$Diet)




