library(tidyverse)

# --- Prepare the data ----
slope_data <- model_data |>
  select(retention, 
         mood_anxiety_total0, mood_anxiety_total2,
         mood_depression_total0, mood_depression_total2) |>
  group_by(retention) |>
  summarise(
    anxiety_week0    = median(mood_anxiety_total0,    na.rm = TRUE),
    anxiety_week2    = median(mood_anxiety_total2,    na.rm = TRUE),
    depression_week0 = median(mood_depression_total0, na.rm = TRUE),
    depression_week2 = median(mood_depression_total2, na.rm = TRUE)
  ) |>
  # Pivot to long format for ggplot
  pivot_longer(
    cols      = -retention,
    names_to  = c("mood", "week"),
    names_sep = "_week",
    values_to = "median_score"
  ) |>
  mutate(
    week     = ifelse(week == "0", "Week 0", "Week 2"),
    week     = factor(week, levels = c("Week 0", "Week 2")),
    mood     = str_to_title(mood),
    retained = ifelse(retention == "Retain", "Retained", "Churned")
  )


# --- Plot -------------
ggplot(slope_data, aes(x = week, y = median_score, 
                       group = interaction(retained, mood),
                       colour = retained,
                       linetype = mood)) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  
  # Label the values at each point
  geom_text(aes(label = round(median_score, 1)),
            vjust = -1, size = 3.5, fontface = "bold") +
  
  scale_colour_manual(values = c("Retained"     = "seagreen",
                                 "Churned" = "red")) +
  scale_linetype_manual(values = c("Anxiety"    = "solid",
                                   "Depression" = "dashed")) +
  
  labs(
    title    = "Median Mood Scores at Week 0 & Week 2 by Retention Status",
    subtitle = "Slopegraph comparing anxiety and depression trajectories",
    x        =  NULL,
    y        = "Median Score",
    colour   = "Retention Status",
    linetype = "Mood Measure"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(colour = "grey30", size = 11),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()   # remove vertical gridlines for cleaner slope
  )

