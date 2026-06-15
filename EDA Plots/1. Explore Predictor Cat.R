library(tidyverse) # for data manipulation and plotting

model_data <- read_csv("model_data.csv") # Load the dataset

model_data |>
  count(retention) |>
  mutate(
    prop  = n / sum(n),
    label = paste0(retention, "\n", scales::percent(prop, accuracy = 1))
  ) |>
  ggplot(aes(x = "", y = prop, fill = retention)) +
  geom_col(width = 1, colour = "white", linewidth = 0.8) +  # white border between slices
  coord_polar(theta = "y", start = 0) +                     # ← flips bar into pie
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),                 # centres label in slice
    colour   = "black",
    fontface = "bold",
    size     = 5
  ) +
  scale_fill_manual(
    values = c("Retain" = "green4", "Churn" = "firebrick"),
    name   = "Outcome"
  ) +
  labs(
    title    = "Retention Class Distribution: Retain (24%) vs Churn (76%)",
    subtitle = "Proportion of user retention at 7 weeks: Retain (22,234) vs Churn (69,819)",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 13) +          # ← removes axes/grid (not needed for pie)
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(colour = "grey23", size = 10, hjust = 0.5),
    legend.position = "bottom"
  )


## -----------------  Function to plot retention by categorical variable  -------------------

plot_retention_by_cat <- function(data, var, title_label = NULL) {
  
  # Use tidy evaluation to allow passing variable name as string
  var_sym <- sym(var)
 
  if (is.null(title_label)) {
    title_label <- paste("Retention by", str_to_title(str_replace_all(var, "_", " ")))
  }
  
  # Pre-compute proportions so we can label them accurately
  # geom_bar(position = "fill") calculates proportions internally but
  # doesn't expose them easily for labelling, so we compute manually.
  label_df <- data |>
    filter(!is.na(!!var_sym), !is.na(retention)) |>
    count(!!var_sym, retention) |>
    group_by(!!var_sym) |>
    mutate(prop = n / sum(n)) |>
    ungroup()
  
  label_df |>
    ggplot(aes(x = !!var_sym, y = prop, fill = retention)) +
    geom_col(width = 0.5) +
    
    # Percentage label — only shown when segment is wide enough to read
    geom_text(
      aes(label = ifelse(prop >= 0.04,
                         scales::percent(prop, accuracy = 1), "")),
      position = position_stack(vjust = 0.55),
      colour   = "black",
      fontface = "bold",
      size     = 2.7
    ) +
    
    scale_fill_manual(
      values = c("Retain" = "green4", "Churn" = "firebrick"),
      name   = "Outcome"
    ) +
    scale_y_continuous(
      labels = scales::percent,
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title    = title_label,
      subtitle = "Labels show % within each category",
      x        = NULL,
      y        = "Percent"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(colour = "grey23", size = 10,
                                        margin = margin(b = 12)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(size = 11, angle = 45, hjust = 1),
      legend.position    = "top",
      legend.title       = element_text(face = "bold")
    )
}

# -- Run for all four variables --
vars <- c("ethnic_cat", "age_cat", "access_type", "code_cat", "gender_cat",
          "easy_to_use2", "would_use_again2", "how_has_life_improved2", "reader")

plots <- map(vars, ~ plot_retention_by_cat(model_data, .x))

# ---- View individually --------
plots[[1]]  # ethnic_cat
plots[[2]]  # age_cat
plots[[3]]  # access_type
plots[[4]]  # code_cat
plots[[5]]  # gender_cat
plots[[6]]  # easy_to_use2to
plots[[7]]  # would_use_again2
plots[[8]]  # how_has_life_improved2


# -- Or view all four in a 2×2 panel --
 library(patchwork)
(plots[[1]] | plots[[2]]) / (plots[[3]] | plots[[7]]) +
plot_annotation(
  title    = "Retention and Churn Across Demographic/Questionaire Categories",
  subtitle = "Comparing retention rates by Ethnicity, Age category, Access type & How life improved",
  theme    = theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(colour = "grey17", size = 12, hjust = 0,
                                 margin = margin(b = 10))
  )
)


(plots[[5]] | plots[[6]]) / (plots[[4]] | plots[[8]]) +
  plot_annotation(
    title    = "Retention and Churn Across Questionaire/Engagement Categories",
    subtitle = "Comparing retention rates by how easy to use, re-use intent, code category & reader",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = element_text(colour = "grey30", size = 12, hjust = 0,
                                   margin = margin(b = 10))
    )
  )

# ggsave("retention_by_demographics.png", width = 14, height = 10, dpi = 300, bg = "white")



## -----------------------  Handling high cardinality Variables ------------------------

model_data_lump <- model_data |> 
  mutate(track_id_lump = fct_lump_prop(track_id, prop = 0.03, other_level = "Other"),
         module_id_lump = fct_lump_prop(module_id, prop = 0.02, other_level = "Other"))

table(model_data_lump$track_id_lump)
table(model_data_lump$module_id_lump)

# ── Run for all four variables ────────────────────────────────────────────────
vars2 <- c("track_id_lump", "module_id_lump")

plots2 <- map(vars2, ~ plot_retention_by_cat(model_data_lump, .x))

# ── View individually ─────────────────────────────────────────────────────────
plots2[[1]]  # track_id
plots2[[2]]  # module_id


(plots2[[1]]) / (plots2[[2]])  +
  plot_annotation(
    title    = "Retention and Churn Across Track & Module Id",
    subtitle = "Comparing retention rates by te type Track & Module listened to",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = element_text(colour = "grey30", size = 12, hjust = 0,
                                   margin = margin(b = 10))
    )
  )


# Check the number of unique track_id and module_id values. Just Rough!
model_data |> count(track_id)
model_data |> distinct(module_id)
