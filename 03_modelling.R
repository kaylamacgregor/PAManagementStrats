################################################################################
# 03_modelling.R
#
# This script fits cumulative link mixed models (CLMMs) to model conservation 
# impact (PAImpact) as an ordinal response variable. It includes:
#   - Fitting the null model and separate models for management strats (PrimaryObjective,
#     Interventions, SpeciesFocus),
#   - Models for species traits (Diet, MigStatus, WaterbirdType),
#   - Target models (Target, SpecificTargetSp, SpeciesStatus),
#   - Interaction models (between species traits and management strats),
#   - Full models combining multiple predictors,
#   - Model comparisons (AICc),
#   - Extraction of odds ratios with confidence intervals.
#
# All analyses are conducted on the birddata dataset (previously cleaned in the 
# data preprocessing script). This script assumes that birddata is already loaded.
################################################################################

#-----------------------------------------------
# Load required libraries (if not already loaded)
#-----------------------------------------------
library(dplyr)           # Data manipulation
library(ordinal)         # For clmm() and clm() functions (cumulative link models)
library(MASS)            # For polr(), used in proportional odds models 
library(brant)           # To check the proportional odds assumption using the Brant test
library(emmeans)         # For post-hoc pairwise comparisons
library(MuMIn)           # For AICc calculations and model selection
library(sjPlot)          # For visualising model estimates
library(gridExtra)       # For arranging output tables/plots
library(ggplot2)         # For plotting graphs
library(RVAideMemoire)   # Provides Anova.clmm() for Type II tests in full models

#-----------------------------------------------
# Ensure NA handling is set
#-----------------------------------------------
options(na.action = "na.exclude")


#-----------------------------------------------
# Assumption Checks: Proportional Odds using the Brant Test
#-----------------------------------------------
# Fit a proportional odds model with polr (from MASS) for comparison
mod_polr <- polr(PAImpact ~ PrimaryObjective + Interventions + SpeciesFocus + Diet + MigStatus, 
                 data = birddata, Hess = TRUE)
brant(mod_polr)
# Run Brant tests for individual predictors if necessary
brant(polr(PAImpact ~ PrimaryObjective, data = birddata, Hess = TRUE))
brant(polr(PAImpact ~ Interventions, data = birddata, Hess = TRUE))
brant(polr(PAImpact ~ SpeciesFocus, data = birddata, Hess = TRUE))
brant(polr(PAImpact ~ Diet, data = birddata, Hess = TRUE))
brant(polr(PAImpact ~ MigStatus, data = birddata, Hess = TRUE))
brant(polr(PAImpact ~ WaterbirdType, data = birddata, Hess = TRUE))
brant(polr(PAImpact ~ Target, data = birddata, Hess = TRUE))
brant(polr(PAImpact ~ SpecificTargetSp, data = birddata, Hess = TRUE)) # violates poa (p=0.03)
brant(polr(PAImpact ~ SpeciesStatus, data = birddata, Hess = TRUE))

# For predictors that violate the proportional odds assumption, fit a partial proportional odds model:
# ie: mod_stsppom <- clm(PAImpact ~ SpecificTargetSp, data = birddata, link = "logit", parallel = FALSE)
# summary(mod_stsppom)
#-----------------------------------------------
# Fit CLMM Models
#-----------------------------------------------

# 1. Null Model
mod_null <- clmm(PAImpact ~ 1 + (1 | SiteName) + (1 | Species), data = birddata)
summary(mod_null)

# 2. Management Models (Research Question 1)
# These models test the effect of each management strategy individually
mod_primary <- clmm(PAImpact ~ PrimaryObjective + (1 | SiteName) + (1 | Species), data = birddata)
mod_intervention <- clmm(PAImpact ~ Interventions + (1 | SiteName) + (1 | Species), data = birddata)
mod_speciesfocus <- clmm(PAImpact ~ SpeciesFocus + (1 | SiteName) + (1 | Species), data = birddata)
summary(mod_primary)
summary(mod_intervention)
summary(mod_speciesfocus)

# 3. Trait Models (Research Question 2)
mod_diet <- clmm(PAImpact ~ Diet + (1 | SiteName) + (1 | Species), data = birddata, link = "logit")
mod_migration <- clmm(PAImpact ~ MigStatus + (1 | SiteName) + (1 | Species), data = birddata)
mod_wbt <- clmm(PAImpact ~ WaterbirdType + (1 | SiteName) + (1 | Species), data = birddata)
summary(mod_diet)
summary(mod_migration)
summary(mod_wbt)

# 4. Target Models (Research Question 3)
mod_target <- clmm(PAImpact ~ Target + (1 | SiteName) + (1 | Species), data = birddata)
mod_status <- clmm(PAImpact ~ SpeciesStatus + (1 | SiteName) + (1 | Species), data = birddata)
summary(mod_target)
summary(mod_status)

# partial prop odds model
mod_stsppom <- clm(PAImpact ~ SpecificTargetSp, data = birddata, link = "logit", parallel = FALSE)
summary(mod_stsppom)

# 5. Full Models

mod_full <- clmm(PAImpact ~ PrimaryObjective + Interventions + SpeciesFocus + Diet + MigStatus +
                      Target  + WaterbirdType + SpeciesStatus + SpecificTargetSp +
                      (1 | SiteName) + (1 | Species), data = birddata)
summary(mod_full)
 # doesnt run due to complexity

# so instead running full mods for each rq 
mod_full_managementstrats <- clmm(PAImpact ~ PrimaryObjective + Interventions + SpeciesFocus +
                                    (1 | SiteName) + (1 | Species), data = birddata)
mod_full_traits <- clmm(PAImpact ~ Diet + MigStatus + WaterbirdType +
                          (1 | SiteName) + (1 | Species), data = birddata)

mod_full_targets <- clmm(PAImpact ~ Target + SpeciesStatus + SpecificTargetSp +
                           (1 | SiteName) + (1 | Species), data = birddata)
summary(mod_full_managementstrats)
summary(mod_full_traits)
summary(mod_full_targets)


## test for multicollineraity between strats and traits (bc interacting)

# Create the design matrix for the fixed effects
design_matrix <- model.matrix(~ PrimaryObjective + Interventions + SpeciesFocus + Diet + MigStatus + WaterbirdType, data = birddata)

# Compute the condition number
condition_number <- kappa(design_matrix)
print(condition_number)

# cn= 16.1 ----> low/moderate collinearity- though not an issue

# collinearity between management strats (bc sites employed all 3 and i found spatial autocorrelation between all strats)
design_matrixstrat <- model.matrix(~ PrimaryObjective + Interventions + SpeciesFocus, data = birddata)
condition_numberstrat <- kappa(design_matrixstrat)
print(condition_numberstrat)

# cn = 18.7 ----> some degree of collinearity, though not severe
#-----------------------------------------------
# Interaction Models
#-----------------------------------------------
# Test interactions between management strategies and species traits

mod_primary_diet <- clmm(PAImpact ~ PrimaryObjective * Diet + (1 | SiteName) + (1 | Species), data = birddata) # doesnt converge
mod_primary_migration <- clmm(PAImpact ~ PrimaryObjective * MigStatus + (1 | SiteName) + (1 | Species), data = birddata) # doesnt converge
mod_primary_wbt <- clmm(PAImpact ~ PrimaryObjective * WaterbirdType + (1 | SiteName) + (1 | Species), data = birddata)

mod_interv_diet <- clmm(PAImpact ~ Interventions * Diet + (1 | SiteName) + (1 | Species), data = birddata)
mod_interv_mig <- clmm(PAImpact ~ Interventions * MigStatus + (1 | SiteName) + (1 | Species), data = birddata)
mod_interv_wbt <- clmm(PAImpact ~ Interventions * WaterbirdType + (1 | SiteName) + (1 | Species), data = birddata)

mod_spfocus_mig <- clmm(PAImpact ~ SpeciesFocus * MigStatus + (1 | SiteName) + (1 | Species), data = birddata)
mod_spfocus_target <- clmm(PAImpact ~ SpeciesFocus * Target + (1 | SiteName) + (1 | Species), data = birddata) # doesnt converge
mod_spfocus_wbt <- clmm(PAImpact ~ SpeciesFocus * WaterbirdType + (1 | SiteName) + (1 | Species), data = birddata)
summary(mod_primary_diet)
summary(mod_primary_migration)
summary(mod_primary_wbt)
summary(mod_interv_diet)
summary(mod_interv_mig)
summary(mod_interv_wbt)
summary(mod_spfocus_mig)
summary(mod_spfocus_target)
summary(mod_spfocus_wbt)


#-----------------------------------------------
# Random Slopes Testing
#-----------------------------------------------

# Management Strategy Predictors
mod_primary_random_slopes <- clmm(PAImpact ~ PrimaryObjective + (PrimaryObjective | SiteName) + (PrimaryObjective | Species), data = birddata) # doesn't run
mod_intervention_random_slopes <- clmm(PAImpact ~ Interventions + (Interventions | SiteName) + (Interventions | Species), data = birddata)
mod_speciesfocus_random_slopes <- clmm(PAImpact ~ SpeciesFocus + (SpeciesFocus | SiteName) + (SpeciesFocus | Species), data = birddata)

summary(mod_primary_random_slopes)
summary(mod_intervention_random_slopes)
summary(mod_speciesfocus_random_slopes)

anova(mod_primary, mod_primary_random_slopes)
anova(mod_intervention, mod_intervention_random_slopes)
anova(mod_speciesfocus, mod_speciesfocus_random_slopes)

# Species Trait Predictors
mod_diet_random_slopes <- clmm(PAImpact ~ Diet + (Diet | SiteName) + (Diet | Species), data = birddata)
mod_migration_random_slopes <- clmm(PAImpact ~ MigStatus + (MigStatus | SiteName) + (MigStatus | Species), data = birddata)
mod_wbt_random_slopes <- clmm(PAImpact ~ WaterbirdType + (WaterbirdType | SiteName) + (WaterbirdType | Species), data = birddata)

summary(mod_diet_random_slopes)
summary(mod_migration_random_slopes)
summary(mod_wbt_random_slopes)

anova(mod_diet, mod_diet_random_slopes)
anova(mod_migration, mod_migration_random_slopes)
anova(mod_wbt, mod_wbt_random_slopes)

# Target Predictors
mod_target_random_slopes <- clmm(PAImpact ~ Target + (Target | SiteName) + (Target | Species), data = birddata)
mod_status_random_slopes <- clmm(PAImpact ~ SpeciesStatus + (SpeciesStatus | SiteName) + (SpeciesStatus | Species), data = birddata)

summary(mod_target_random_slopes)
summary(mod_status_random_slopes)

anova(mod_target, mod_target_random_slopes)
anova(mod_status, mod_status_random_slopes)

#-----------------------------------------------
# Model Comparison Using AICc
#-----------------------------------------------

## management strats
# Create a named vector of your model AICc values
aicc_values <- c(
  mod_null = AICc(mod_null),
  mod_primary = AICc(mod_primary),
  mod_primary_random_slopes = AICc(mod_primary_random_slopes),
  mod_intervention = AICc(mod_intervention),
  mod_intervention_random_slopes = AICc(mod_intervention_random_slopes),
  mod_speciesfocus = AICc(mod_speciesfocus),
  mod_speciesfocus_random_slopes = AICc(mod_speciesfocus_random_slopes),
  mod_full_managementstrats = AICc(mod_full_managementstrats)
)

# Convert to data frame
aicc_df <- data.frame(
  Model = names(aicc_values),
  AICc = aicc_values
)

# Calculate delta AICc and weights
aicc_df$Delta_AICc <- aicc_df$AICc - min(aicc_df$AICc)
aicc_df$Weight <- exp(-0.5 * aicc_df$Delta_AICc)
aicc_df$Weight <- aicc_df$Weight / sum(aicc_df$Weight)

# Print results
print(aicc_df[order(aicc_df$Delta_AICc), ])

# traits
# Create a named vector with AICc values for trait-related models
aicc_traits <- c(
  mod_null = AICc(mod_null),
  mod_diet = AICc(mod_diet),
  mod_diet_random_slopes = AICc(mod_diet_random_slopes),
  mod_migration = AICc(mod_migration),
  mod_migration_random_slopes = AICc(mod_migration_random_slopes),
  mod_wbt = AICc(mod_wbt),
  mod_wbt_random_slopes = AICc(mod_wbt_random_slopes),
  mod_full_traits = AICc(mod_full_traits)
)

# Convert to data frame
aicc_traits_df <- data.frame(
  Model = names(aicc_traits),
  AICc = aicc_traits
)

# Calculate ΔAICc and weights
aicc_traits_df$Delta_AICc <- aicc_traits_df$AICc - min(aicc_traits_df$AICc)
aicc_traits_df$Weight <- exp(-0.5 * aicc_traits_df$Delta_AICc)
aicc_traits_df$Weight <- aicc_traits_df$Weight / sum(aicc_traits_df$Weight)

# Print ranked results
print(aicc_traits_df[order(aicc_traits_df$Delta_AICc), ])




# targets
# Create named vector of AICc values for target-related models
aicc_targets <- c(
  mod_null = AICc(mod_null),
  mod_target = AICc(mod_target),
  mod_target_random_slopes = AICc(mod_target_random_slopes),
  mod_stsppom = AICc(mod_stsppom),
  mod_status = AICc(mod_status),
  mod_status_random_slopes = AICc(mod_status_random_slopes),
  mod_full_targets = AICc(mod_full_targets)
)

# Convert to a data frame
aicc_targets_df <- data.frame(
  Model = names(aicc_targets),
  AICc = aicc_targets
)

# Calculate delta AICc and model weights
aicc_targets_df$Delta_AICc <- aicc_targets_df$AICc - min(aicc_targets_df$AICc)
aicc_targets_df$Weight <- exp(-0.5 * aicc_targets_df$Delta_AICc)
aicc_targets_df$Weight <- aicc_targets_df$Weight / sum(aicc_targets_df$Weight)

# Print ranked model comparison
print(aicc_targets_df[order(aicc_targets_df$Delta_AICc), ])


### ANOVA for Full Management Strategy Model

# since the three management strategies are employed simultaneously 
#and all three showed spatial autocorrelation — meaning nearby PAs tend to use similar approaches.
# want to see whether any of these strats have a distinct effect when considered together
Anova.clmm(mod_full_managementstrats)
## none sig, likely due to shared variance or overlapping explanatory power among strategies


#-----------------------------------------------
# Odds Ratios Extraction & Visualisation
#-----------------------------------------------
# see appendix 

# Create a named list of models
model_list <- list(
  "Primary Objective" = mod_primary,
  "Intervention" = mod_intervention,
  "Species Focus" = mod_speciesfocus,
  "Diet" = mod_diet,
  "Migration" = mod_migration,
  "Waterbird Type" = mod_wbt,
  "Target" = mod_target,
  "Species Status" = mod_status
)

# Extract odds ratios
or_all <- bind_rows(lapply(names(model_list), function(name) {
  extract_or(model_list[[name]], name)
})) %>% filter(Variable != "(Intercept)")

print(or_all)

  ggplot(or_all, aes(x = reorder(Variable, OddsRatio), 
                     y = OddsRatio, 
                     ymin = Lower_CI, 
                     ymax = Upper_CI, 
                     colour = Significance)) +
    geom_pointrange(linewidth = 0.8) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    coord_flip() +
    facet_wrap(~ Model, scales = "free_y") +
    labs(
      x = "Predictor",
      y = "Odds Ratio",
      colour = "Significance"
    ) +
    scale_colour_manual(values = c(
      "***" = "red",
      "**"  = "orange",
      "*"   = "blue",
      "NS"  = "grey60"
    )) +
    theme_classic() +
    theme( 
      strip.background = element_blank())            
  
#-----------------------------------------------
# End of 03_modelling.R
################################################################################




