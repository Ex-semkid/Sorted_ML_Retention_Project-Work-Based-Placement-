library(tidyr)
library(dplyr)
library(ggplot2)

model_data <- read_csv("model_data.csv")


# Pivot the data longer
# This regex splits the column name before the '0' or '2' at the end
df_long2 <- model_data |> 
  pivot_longer(
    cols = c(mood_anxiety_total0, mood_anxiety_total2, 
             mood_depression_total0, mood_depression_total2),
    names_to = c(".value", "timepoint"),
    names_pattern = "(.*)(\\d)$"
  ) |> 
  mutate(timepoint = ifelse(timepoint == "0", "Baseline (Wk 0)", "Follow-up (Wk 2)"))

# Visualize with a Boxplot
 p1 <- ggplot(df_long2, aes(x = timepoint, y = mood_depression_total, fill = retention)) +
  geom_boxplot(outlier.alpha = 0.2) +
  facet_wrap(~"Depression Scores") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Symptom Shift: Baseline vs. Week 2",
       y = "Score Value", 
       x = NULL) +
  scale_fill_manual(
    values = c("Retain" = "green4", "Churn" = "firebrick"))

# Visualize with a Boxplot
p2 <- ggplot(df_long2, aes(x = timepoint, y = mood_anxiety_total, fill = retention)) +
  geom_boxplot(outlier.alpha = 0.2) +
  facet_wrap(~"Anxiety Scores") +
  theme_minimal() +
  labs(title = "Symptom Shift: Baseline vs. Week 2",
       y = "Score Value", 
       x = NULL) +
  scale_fill_manual(
    values = c("Retain" = "green4", "Churn" = "firebrick"))


library(patchwork)
p1 + p2 +
  plot_annotation(
    title    = "Retention Vs. Churn Across Symptom Categories",
    subtitle = "Retainers & Chuners showed improvement in Depression & Anxiety over 2 weeks",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = element_text(colour = "grey30", size = 12, hjust = 0,
                                   margin = margin(b = 10))
    )
  )


# ---------------------------------------------------------------------
# Heatmap: Tracks Listened (Week 0 & Week 2) vs Retention Outcome
# Bins track counts into clinical ranges, shows proportion of each
# retention group falling in each bin.
# ----------------------------------------------------------------------

library(tidyverse)

# ---- Pivot both time points long -----------------------
heatmap_df <- model_data |>
  filter(
    tracks_listened0 != max(tracks_listened0, na.rm = TRUE),
    tracks_listened2 != max(tracks_listened2, na.rm = TRUE)
  ) |>
  select(retention, tracks_listened0, tracks_listened2) |>
  pivot_longer(
    cols      = c(tracks_listened0, tracks_listened2),
    names_to  = "timepoint",
    values_to = "tracks"
  ) |>
  mutate(
    timepoint = if_else(timepoint == "tracks_listened0", "Week 0", "Week 2"),
    # Bin into clinically meaningful engagement bands
    track_band = cut(
      tracks,
      breaks = c(0, 5, 10, 20, 40, 80, Inf),
      labels = c("1–5", "6–10", "11–20", "21–40", "41–80", "80+"),
      include.lowest = TRUE,
      right          = TRUE
    )
  ) |>
  drop_na(tracks, retention, track_band)

# ----- Compute proportion within each retention × timepoint × band cell --------
# Using proportion (not raw count) so Retain/Churn groups are comparable
# even if they differ in sample size.

heatmap_agg <- heatmap_df |>
  count(retention, timepoint, track_band) |>
  group_by(retention, timepoint) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

# -- Plot --
ggplot(heatmap_agg,
       aes(x = track_band, y = retention, fill = prop)) +
  
  geom_tile(colour = "white", linewidth = 0.8) +
  
  # Percentage label inside each tile
  geom_text(
    aes(label = scales::percent(prop, accuracy = 1)),
    size   = 3.8,
    colour = "white",
    fontface = "bold"
  ) +
  
  # Separate Week 0 and Week 2 side by side
  facet_wrap(~ timepoint, nrow = 1) +
  
  # Teal → indigo gradient — darker = higher concentration of users
  scale_fill_gradient(
    low      = "grey7",
    high     = "cyan3",
    labels   = scales::percent,
    name     = "% of group"
  ) +
  
  scale_x_discrete(
    expand = c(0, 0)
  ) +
  
  scale_y_discrete(
    expand = c(0, 0)
  ) +
  
  labs(
    title    = "Where Do Retained vs Churned Users Concentrate?",
    subtitle = "Each cell shows % of retention group in each Listening Band",
    x        = "Tracks Listened",
    y        = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey30", size = 11,
                                    margin = margin(b = 14)),
    strip.text       = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey70", colour = NA),
    panel.grid       = element_blank(),
    axis.text.y      = element_text(face = "bold", size = 12),
    axis.text.x        = element_text(size = 11, angle = 45, hjust = 1),
    legend.position  = "right",
    legend.title     = element_text(size = 10),
    plot.margin      = margin(16, 16, 16, 16)
  )


# ggsave("heatmap_tracks_retention.png", width = 12, height = 4.5)