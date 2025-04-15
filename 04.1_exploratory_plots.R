##############################################
# 04.1_exploratory_plots.R
# Purpose: 
#   To explore raw data and examine preliminary model predictions.
#   This script produces exploratory visualisations that help assess 
#   variable distributions, relationships, and model outputs.
##############################################


# Load required libraries (if not already loaded)
library(dplyr)
library(ggplot2)
# (Assuming any additional packages like gridExtra, ggpubr are loaded here or in 00_setup.R)

# ----- Bar Plots Section -----
# Figure 4: Count of Sites per Primary Objective
primary_counts <- birddata %>%
  distinct(SiteCode, PrimaryObjective) %>%
  count(PrimaryObjective)

primary_plot <- ggplot(primary_counts, aes(x = PrimaryObjective, y = n)) +
  geom_bar(stat = "identity", width = 0.7, fill = "darkolivegreen4") +
  geom_text(aes(label = n), vjust = -0.5, size = 4, color = "black") +
  labs(x = "Primary Objective", y = "Number of Sites", fill = "Conservation Objective") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), n.breaks = 10, breaks = seq(0, max(primary_counts$n) + 1, 1)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 11, color = "black"),
        legend.position = "none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.margin = margin(t = 40, r = 10, b = 10, l = 10)) +
  coord_cartesian(clip = "off")

print(primary_plot)

# Intervention and Species Focus Bar Plots
intervention_counts <- birddata %>%
  filter(!is.na(Interventions)) %>%
  distinct(SiteCode, Interventions) %>%
  count(Label = Interventions) %>%
  mutate(Category = "Intervention")

species_focus_counts <- birddata %>%
  filter(!is.na(SpeciesFocus)) %>%
  distinct(SiteCode, SpeciesFocus) %>%
  count(Label = SpeciesFocus) %>%
  mutate(Category = "Species Focus")

combined_counts <- bind_rows(intervention_counts, species_focus_counts)

combined_plotbar <- ggplot(combined_counts, aes(x = Label, y = n, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 4, color = "black") +
  facet_wrap(~ Category, scales = "free_x") +
  scale_fill_manual(name = "Management Strategy",
                    values = c("Intervention" = "coral", "Species Focus" = "#8DA0CB")) +
  scale_y_continuous(expand = c(0, 0), n.breaks = 8) +
  labs(x = "", y = "Number of Sites") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, color = "black"),
        axis.text.y = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 10, color = "black"),
        strip.text.x = element_blank(),
        legend.title = element_text(size = 9, color = "black"),
        legend.text = element_text(size = 9, color = "black"),
        plot.margin = margin(t = 40, r = 10, b = 10, l = 10)) +
  coord_cartesian(clip = "off")

print(combined_plotbar)


## code for flowchart (for my methods fig2)
# If you want to generate the management decision flowchart, uncomment the following lines:
# library(DiagrammeR)
# grViz("
# digraph flowchart {
#   graph [layout = dot, rankdir = TB]
#   node [shape = box, style = filled, fillcolour = LightBlue, fontname = Helvetica, fontsize=10]
#
#   A [label = 'Review Management Documents']
#   B [label = 'Record All Management Objectives']
#   C [label = 'Quantitative Assessment:\nCount Frequency of Mentions']
#   D [label = 'Is one objective mentioned in >50%?\n(Repeated across sections)']
#   E [label = 'Assign as Primary Management Objective']
#   F [label = 'Qualitative Assessment:\nEvaluate Prominence, Specificity,\nEvidence of Implementation']
#   G [label = 'Resolve Ambiguity:\nUse Contextual Factors\n(e.g., Site Designation)']
#   H [label = 'Designate Primary Management Objective']
#   I [label = 'Classify Site into:\n1. Biodiversity & Habitat Conservation\n2. Water Quality & Ecosystem Health\n3. (Waterbird) Species-Specific Conservation\n4. Sustainable Land Management & Forestry\n5. Recreational & Public Engagement']
#
#   A -> B -> C -> D;
#   D -> E [label = 'Yes', fontcolour = 'black'];
#   D -> F [label = 'No', fontcolour = 'black'];
#   F -> G;
#   G -> H;
#   E -> I;
#   H -> I;
# }
# ")


# extra exploratory viualisations not included in diss (but may still be of interest)

##############################################
# End of 04_posthoc_and_plots.R
##############################################
