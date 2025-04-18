################################################################################
# 04.2_posthoc_and_plots.R
# Purpose:
#   - Extract model predictions using ggpredict() from fitted CLMM models.
#   - Generate ggplot figures for predictive probability plots.
#   -  Arrange multiple plots with shared legends.
#
# Assumptions:
#   - The dataset 'birddata' is already loaded (see 01_data_preprocessing.R).
#   - The models (mod_primary, mod_intervention, mod_speciesfocus, mod_diet,
#     mod_migration, mod_target, mod_wbt, mod_ssppom, mod_stppom) have been fitted 
#     in the modelling script (03_modelling.R).

## 
################################################################################

# Load required libraries (if not already loaded)
library(ggplot2)
library(ggeffects)
library(dplyr)
library(ggpubr)    # For arranging plots (optional)
library(gridExtra) # For grid.arrange()
library(grid)      # For annotations
library(emmeans)   # For post-hoc comparisons

###########################################################################
# 1. Extract Predictions 
###########################################################################


# Extract and print model predictions for all models

# Primary Objective model
ModelPredictions_mod_primary <- ggpredict(mod_primary, terms = c("PrimaryObjective"), type = "re")
print(ModelPredictions_mod_primary)

# Interventions model
ModelPredictions_mod_intervention <- ggpredict(mod_intervention, terms = c("Interventions"), type = "re")
print(ModelPredictions_mod_intervention)

# Species Focus model
ModelPredictions_mod_speciesfocus <- ggpredict(mod_speciesfocus, terms = c("SpeciesFocus"), type = "re")
print(ModelPredictions_mod_speciesfocus)

# Diet model (using mod_diet, change the term if different)
ModelPredictions_mod_diet <- ggpredict(mod_diet, terms = c("Diet"), type = "re")
print(ModelPredictions_mod_diet)

# Migration Status model
ModelPredictions_mod_migration <- ggpredict(mod_migration, terms = c("MigStatus"), type = "re")
print(ModelPredictions_mod_migration)

# Waterbird Type model
ModelPredictions_mod_wbt <- ggpredict(mod_wbt, terms = c("WaterbirdType"), type = "re")
print(ModelPredictions_mod_wbt)

# Target model
ModelPredictions_mod_target <- ggpredict(mod_target, terms = c("Target"), type = "re")
print(ModelPredictions_mod_target)

# Specific Target Species model
ModelPredictions_mod_spectarg <- ggpredict(mod_stsppom, terms = c("SpecificTargetSp"), type = "re")
print(ModelPredictions_mod_spectarg)

ModelPredictions_mod_status <- ggpredict(mod_ssppom, terms = c("SpeciesStatus"), type = "re")
print(ModelPredictions_mod_status)

# --- A. Primary Objective ---
FitDat_primary <- data.frame(ggpredict(mod_primary, terms = c("PrimaryObjective"), type = "re")) %>%
  rename(PAImpact = group, PrimaryObjective = x, Predicted = predicted)
# Recode PAImpact from numeric to descriptive levels
FitDat_primary$PAImpact <- factor(FitDat_primary$PAImpact,
                                  levels = c(1, 2, 3),
                                  labels = c("Negative Impact", "No Impact", "Positive Impact"))
# In case the 'response.level' column is present, use it to refine:
FitDat_primary <- FitDat_primary %>%
  mutate(PAImpact = case_when(
    response.level == 1 ~ "Negative Impact",
    response.level == 2 ~ "No Impact",
    response.level == 3 ~ "Positive Impact"
  ))
# Inspect the data
glimpse(FitDat_primary)


# --- B. Interventions ---
FitDat_int <- data.frame(ggpredict(mod_intervention, terms = c("Interventions"), type = "re")) %>%
  rename(PAImpact = group, Interventions = x, Predicted = predicted)
FitDat_int$PAImpact <- factor(FitDat_int$PAImpact,
                              levels = c(1, 2, 3),
                              labels = c("Negative Impact", "No Impact", "Positive Impact"))
FitDat_int <- FitDat_int %>%
  mutate(PAImpact = case_when(
    response.level == 1 ~ "Negative Impact",
    response.level == 2 ~ "No Impact",
    response.level == 3 ~ "Positive Impact"
  ))
glimpse(FitDat_int)


# --- C. Species Focus ---
FitDat_spf <- data.frame(ggpredict(mod_speciesfocus, terms = c("SpeciesFocus"), type = "re")) %>%
  rename(PAImpact = group, SpeciesFocus = x, Predicted = predicted)
FitDat_spf$PAImpact <- factor(FitDat_spf$PAImpact,
                              levels = c(1, 2, 3),
                              labels = c("Negative Impact", "No Impact", "Positive Impact"))
FitDat_spf <- FitDat_spf %>%
  mutate(PAImpact = case_when(
    response.level == 1 ~ "Negative Impact",
    response.level == 2 ~ "No Impact",
    response.level == 3 ~ "Positive Impact"
  ))
glimpse(FitDat_spf)


# --- D. Diet ---
FitDat_diet <- data.frame(ggpredict(mod_diet, terms = c("Diet"), type = "re")) %>%
  rename(PAImpact = group, Diet = x, Predicted = predicted)
FitDat_diet$PAImpact <- factor(FitDat_diet$PAImpact,
                               levels = c(1, 2, 3),
                               labels = c("Negative Impact", "No Impact", "Positive Impact"))
FitDat_diet <- FitDat_diet %>%
  mutate(PAImpact = case_when(
    response.level == 1 ~ "Negative Impact",
    response.level == 2 ~ "No Impact",
    response.level == 3 ~ "Positive Impact"
  ))
glimpse(FitDat_diet)


# --- E. Migration Status ---
FitDat_migration <- data.frame(ggpredict(mod_migration, terms = c("MigStatus"), type = "re")) %>%
  rename(PAImpact = group, MigStatus = x, Predicted = predicted)
FitDat_migration$PAImpact <- factor(FitDat_migration$PAImpact,
                                    levels = c(1, 2, 3),
                                    labels = c("Negative Impact", "No Impact", "Positive Impact"))
FitDat_migration <- FitDat_migration %>%
  mutate(PAImpact = case_when(
    response.level == 1 ~ "Negative Impact",
    response.level == 2 ~ "No Impact",
    response.level == 3 ~ "Positive Impact"
  ))
glimpse(FitDat_migration)


# --- F. Target ---
FitDat_target <- data.frame(ggpredict(mod_target, terms = c("Target"), type = "re")) %>%
  rename(PAImpact = group, Target = x, Predicted = predicted)
FitDat_target$PAImpact <- factor(FitDat_target$PAImpact,
                                 levels = c(1, 2, 3),
                                 labels = c("Negative Impact", "No Impact", "Positive Impact"))
FitDat_target <- FitDat_target %>%
  mutate(PAImpact = case_when(
    response.level == 1 ~ "Negative Impact",
    response.level == 2 ~ "No Impact",
    response.level == 3 ~ "Positive Impact"
  ))
glimpse(FitDat_target)


# --- G. Waterbird Type ---
FitDat_wbt <- data.frame(ggpredict(mod_wbt, terms = c("WaterbirdType"), type = "re")) %>%
  rename(PAImpact = group, WaterbirdType = x, Predicted = predicted)
FitDat_wbt$PAImpact <- factor(FitDat_wbt$PAImpact,
                              levels = c(1, 2, 3),
                              labels = c("Negative Impact", "No Impact", "Positive Impact"))
FitDat_wbt <- FitDat_wbt %>%
  mutate(PAImpact = case_when(
    response.level == 1 ~ "Negative Impact",
    response.level == 2 ~ "No Impact",
    response.level == 3 ~ "Positive Impact"
  ))
glimpse(FitDat_wbt)


# --- H. Species Status ---
FitDat_status <- data.frame(ggpredict(mod_ssppom, terms = c("SpeciesStatus"), type = "re")) %>%
  rename(PAImpact = group, SpeciesStatus = x, Predicted = predicted)
FitDat_status$PAImpact <- factor(FitDat_status$PAImpact,
                                 levels = c(1, 2, 3),
                                 labels = c("Negative Impact", "No Impact", "Positive Impact"))
FitDat_status <- FitDat_status %>%
  mutate(PAImpact = case_when(
    response.level == 1 ~ "Negative Impact",
    response.level == 2 ~ "No Impact",
    response.level == 3 ~ "Positive Impact"
  ))
glimpse(FitDat_status)


# --- I. Specific Target Species ---
FitDat_spectarg <- data.frame(ggpredict(mod_stsppom, terms = c("SpecificTargetSp"), type = "re")) %>%
  rename(PAImpact = group, SpecificTargetSp = x, Predicted = predicted)
FitDat_spectarg$PAImpact <- factor(FitDat_spectarg$PAImpact,
                                   levels = c(1, 2, 3),
                                   labels = c("Negative Impact", "No Impact", "Positive Impact"))
FitDat_spectarg <- FitDat_spectarg %>%
  mutate(PAImpact = case_when(
    response.level == 1 ~ "Negative Impact",
    response.level == 2 ~ "No Impact",
    response.level == 3 ~ "Positive Impact"
  ))
glimpse(FitDat_spectarg)

###########################################################################
# 2. Generate Plots for Each Prediction
###########################################################################


# ------------------------------
# Plot 1: Primary Objective (grouped by PAImpact)
plot1test <- ggplot(FitDat_primary, aes(x = PrimaryObjective, y = Predicted, colour = PAImpact)) +
  geom_point(position = position_dodge(width = 0.5), alpha = 1, size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7,
                position = position_dodge(width = 0.5)) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 10, colour = "black"),
    axis.text.y  = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 12, colour = "black"),
    axis.title.y = element_blank(),
    legend.text  = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  ) +
  labs(x = "Primary Objective of Management", colour = "Conservation Impact", fill = "Impact") +
  scale_colour_manual(values = c("sienna3", "mediumpurple2", "#008080")) +
  scale_fill_manual(values  = c("sienna3", "mediumpurple2", "#008080")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1))


# ------------------------------
# Plot 2: Management Intervention (grouped by PAImpact)
plot2test <- ggplot(FitDat_int, aes(x = Interventions, y = Predicted, colour = PAImpact)) +
  geom_point(position = position_dodge(width = 0.5), alpha = 1, size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7,
                position = position_dodge(width = 0.5)) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 10, colour = "black"),
    axis.text.y  = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 12, colour = "black"),
    axis.title.y = element_blank(),
    legend.text  = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  ) +
  labs(x = "Management Intervention", colour = "Conservation Impact", fill = "Impact") +
  scale_colour_manual(values = c("sienna3", "mediumpurple2", "#008080")) +
  scale_fill_manual(values  = c("sienna3", "mediumpurple2", "#008080")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1))


# ------------------------------
# Plot 3: Species Focus (grouped by PAImpact)
plot3test <- ggplot(FitDat_spf, aes(x = SpeciesFocus, y = Predicted, colour = PAImpact)) +
  geom_point(position = position_dodge(width = 0.5), alpha = 1, size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7,
                position = position_dodge(width = 0.5)) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 10, colour = "black"),
    axis.text.y  = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 12, colour = "black"),
    axis.title.y = element_blank(),
    legend.text  = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  ) +
  labs(x = "Species Focus", colour = "Conservation Impact", fill = "Impact") +
  scale_colour_manual(values = c("sienna3", "mediumpurple2", "#008080")) +
  scale_fill_manual(values  = c("sienna3", "mediumpurple2", "#008080")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1))


# ------------------------------
# Arrange the three plots in one column and add labels & shared y-axis label
combined_plots <- grid.arrange(plot1test, plot2test, plot3test, ncol = 1)

grid.text("a)", x = 0.05, y = 0.99, gp = gpar(fontsize = 11, col = "black"))  # for plot1
grid.text("b)", x = 0.05, y = 0.67, gp = gpar(fontsize = 11, col = "black"))  # for plot2
grid.text("c)", x = 0.05, y = 0.36, gp = gpar(fontsize = 11, col = "black"))  # for plot3

grid.text("Predicted Probability of PA Impact on Waterbird Populations", 
          rot = 90, x = 0.02, y = 0.55, gp = gpar(fontsize = 14, col = "black"))

combined_plots <- ggarrange(
  plot1test + theme(legend.position = "right"), 
  plot2test + theme(legend.position = "none"),
  plot3test + theme(legend.position = "none"),
  ncol = 1,
  labels = c("a)", "b)", "c)"),
  label.x = 0.02,
  label.y = 0.98,
  common.legend = TRUE,
  legend = "right"
)

annotate_figure(combined_plots,
                left = text_grob("Predicted Probability of PA Impact on Waterbird Populations", 
                                 rot = 90, size = 12, color = "black"))

combined_plots <- ggarrange(
  plot1test + theme(legend.position = "right"), 
  plot2test + theme(legend.position = "none"),
  plot3test + theme(legend.position = "none"),
  ncol = 1,
  labels = c("a)", "b)", "c)"),
  font.label = list(size = 11, face = "plain"), 
  label.x = 0.05,
  label.y = c(1.00, 1.02, 1.02),
  common.legend = TRUE,
  legend = "right"
)

annotate_figure(combined_plots,
                left = text_grob("Predicted Probability of Conservation Impact on Waterbird Populations", 
                                 rot = 90, size = 13, colour = "black"))

# Add shared Y-axis label
annotate_figure(combined_plots,
                left = text_grob("Predicted Probability of Conservation Impact on Waterbird Populations",
                                 rot = 90, size = 12, vjust = 1))


# ------------------------------
# Plot 4: Diet
plot4 <- ggplot(FitDat_diet, aes(x = Diet, y = Predicted, color = PAImpact)) + 
  geom_point(position = position_dodge(width = 0.5), alpha = 1, size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7,
                position = position_dodge(width = 0.5)) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 10, colour = "black"),
    axis.title.x = element_text(size = 12, colour = "black"),
    axis.text.y  = element_text(size = 10, colour = "black"),
    axis.title.y = element_blank(),
    legend.text  = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  ) +
  labs(x = "Diet", colour = "Conservation Impact", fill = "Impact") +
  scale_colour_manual(values = c("sienna3", "mediumpurple2", "#008080")) +
  scale_fill_manual(values  = c("sienna3", "mediumpurple2", "#008080")) +
  coord_cartesian(ylim = c(0,1)) +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme(
    legend.text  = element_text(size = 10, colour = "black"), 
    legend.key.size = unit(0.5, "cm"), 
    axis.title.x = element_text(size = 12, colour = "black"), 
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  )




# ------------------------------
# Plot 5: Migration Status
plot5 <- ggplot(FitDat_migration, aes(x = MigStatus, y = Predicted, colour = PAImpact)) + 
  geom_point(position = position_dodge(width = 0.5), alpha = 1, size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7,
                position = position_dodge(width = 0.5)) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 10, colour = "black"),
    axis.text.y  = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 12, colour = "black"),
    axis.title.y = element_blank(),
    legend.text  = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  ) +
  labs(x = "Migration Status", colour = "Conservation Impact", fill = "Impact") +
  scale_colour_manual(values = c("sienna3", "mediumpurple2", "#008080")) +
  scale_fill_manual(values  = c("sienna3", "mediumpurple2", "#008080")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme(
    legend.text  = element_text(size = 10, colour = "black"), 
    legend.key.size = unit(0.5, "cm"), 
    axis.title.x = element_text(size = 12, colour = "black"), 
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  )

# ------------------------------
# Plot 6: Taxonomic Group

plot6 <- ggplot(FitDat_wbt, aes(x = WaterbirdType, y = Predicted, colour = PAImpact)) + 
  geom_point(position = position_dodge(width = 0.5), alpha = 1, size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7,
                position = position_dodge(width = 0.5)) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 10, colour = "black"),
    axis.title.x = element_text(size = 12, colour = "black"),
    axis.text.y  = element_text(size = 10, colour = "black"),
    axis.title.y = element_blank(),
    legend.text  = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  ) +
  labs(x = "Taxonomic Group", colour = "Conservation Impact", fill = "Impact") +
  scale_colour_manual(values = c("sienna3", "mediumpurple2", "#008080")) +
  scale_fill_manual(values  = c("sienna3", "mediumpurple2", "#008080")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme(
    legend.text  = element_text(size = 10, colour = "black"), 
    axis.title.x = element_text(size = 12, colour = "black"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  )





# ------------------------------
# Arrange Plot 4, Plot 5 and Plot 6 in one column with labels and shared y-axis label later
grid.arrange(plot4, plot5, plot6, ncol = 1, heights = c(1, 1, 1))

grid.text("a)", x = 0.05, y = 0.98, gp = gpar(fontsize = 11, col = "black"))  # label for plot4
grid.text("b)", x = 0.05, y = 0.68, gp = gpar(fontsize = 11, col = "black"))  # label for plot5
grid.text("c)", x = 0.05, y = 0.33, gp = gpar(fontsize = 11, col = "black"))  # label for plot9

grid.text("Predicted Probability of PA Impact on Waterbird Populations", 
          rot = 90, x = 0.02, y = 0.5, gp = gpar(fontsize = 12, col = "black"))

# ------------------------------
# Arrange plots 4, 5, 6 with a single shared legend
combined_traits_plots_456 <- ggarrange(
  plot4 + theme(legend.position = "right"),
  plot5 + theme(legend.position = "none"),
  plot6 + theme(legend.position = "none"),
  ncol = 1,
  labels = c("a)", "b)", "c)"),
  font.label = list(size = 11, face = "plain"),
  label.x = 0.025,
  label.y = c(1.00, 1.02, 1.02),
  common.legend = TRUE,
  legend = "right"
)

annotate_figure(combined_traits_plots_456,
                left = text_grob("Predicted Probability of Conservation Impact on Waterbird Populations",
                                 rot = 90, size = 12, vjust = 1, color = "black"))



# ------------------------------
# Plot 7: Target Group
plot7 <- ggplot(FitDat_target, aes(x = Target, y = Predicted, colour = PAImpact)) + 
  geom_point(position = position_dodge(width = 0.5), alpha = 1, size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7,
                position = position_dodge(width = 0.5)) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 10, colour = "black"),
    axis.title.x = element_text(size = 12, colour = "black"),
    axis.text.y  = element_text(size = 10, colour = "black"),
    axis.title.y = element_blank(),
    legend.text  = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  ) +
  labs(x = "Target Group", colour = "Conservation Impact", fill = "Impact") +
  scale_colour_manual(values = c("sienna3", "mediumpurple2", "#008080")) +
  scale_fill_manual(values  = c("sienna3", "mediumpurple2", "#008080")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme(
    legend.text  = element_text(size = 10, colour = "black"), 
    axis.title.x = element_text(size = 12, colour = "black"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  )

# ------------------------------
# Plot 8: Specific Target Species
plot8 <- ggplot(FitDat_spectarg, aes(x = SpecificTargetSp, y = Predicted, colour = PAImpact)) + 
  geom_point(position = position_dodge(width = 0.5), alpha = 1, size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7,
                position = position_dodge(width = 0.5)) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 10, colour = "black"),
    axis.title.x = element_text(size = 12, colour = "black"),
    axis.text.y  = element_text(size = 10, colour = "black"),
    axis.title.y = element_blank(),
    legend.text  = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  ) +
  labs(x = "Specific Target Species", colour = "Conservation Impact", fill = "Impact") +
  scale_colour_manual(values = c("sienna3", "mediumpurple2", "#008080")) +
  scale_fill_manual(values  = c("sienna3", "mediumpurple2", "#008080")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme(
    legend.text  = element_text(size = 10, colour = "black"), 
    axis.title.x = element_text(size = 12, colour = "black"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  )




plot9 <- ggplot(FitDat_status, aes(x = SpeciesStatus, y = Predicted, colour = PAImpact)) + 
  geom_point(position = position_dodge(width = 0.5), alpha = 1, size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7,
                position = position_dodge(width = 0.5), na.rm = TRUE) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 10, colour = "black"),
    axis.title.x = element_text(size = 12, colour = "black"),
    axis.text.y  = element_text(size = 10, colour = "black"),
    axis.title.y = element_blank(),
    legend.text  = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin  = unit(c(0.5, 0.5, 0.5, 1), "cm")
  ) +
  labs(x = "Species Conservation Status", colour = "Conservation Impact", fill = "Impact") +
  scale_colour_manual(values = c("sienna3", "mediumpurple2", "#008080")) +
  scale_fill_manual(values  = c("sienna3", "mediumpurple2", "#008080")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


# ------------------------------
# Arrange plots 7, 8,  9 in a grid with labels and shared y-axis label
grid.arrange(plot7, plot8, plot9, ncol = 1, heights = c(1, 1, 1))

grid.text("a)", x = 0.05, y = 0.98, gp = gpar(fontsize = 11, col = "black"))  # label for plot4
grid.text("b)", x = 0.05, y = 0.68, gp = gpar(fontsize = 11, col = "black"))  # label for plot5
grid.text("c)", x = 0.05, y = 0.33, gp = gpar(fontsize = 11, col = "black"))  # label for plot9


# ------------------------------
# Arrange plots with a single shared legend (Traits plots)
combined_targ_plots <- ggarrange(
  plot7 + theme(legend.position = "right"),
  plot8 + theme(legend.position = "none"),
  plot9 + theme(legend.position = "none"),
  ncol = 1, nrow = 3,
  labels = c("a)", "b)", "c)", "d)"),
  font.label = list(size = 11, face = "plain"),
  label.x = c(0.03, 0.025, 0.03, 0.025),
  label.y = c(1.0, 1.0, 0.99, 0.99), 
  common.legend = TRUE,
  legend = "right"
)

annotate_figure(combined_targ_plots,
                left = text_grob("Predicted Probability of Conservation Impact on Waterbird Populations",
                                 rot = 90, size = 12, vjust = 1, color = "black"))


###########################################################################
# 4. Post-Hoc Pairwise Comparisons # for non binary predictors
###########################################################################
# see appendix

#  Primary Objective:
emm_primary <- emmeans(mod_primary, pairwise ~ PrimaryObjective, adjust = "Tukey")
summary(emm_primary)

#none sig, but rec and ssc have the largest diff

#  for diet 
emm_diet <- emmeans(mod_diet, pairwise ~ Diet, adjust = "Tukey")
summary(emm_diet)

# none sig 

################################################################################
# End of 04_posthoc_and_plots.R
################################################################################
