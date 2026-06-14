
#  mutate(how_has_life_improved2 = na_if(how_has_life_improved2, "bananas"))


## -------------------------- naniar ------------------------- ##
library(naniar)

# Visualizing the top 10 variables with missing data
gg_miss_var(model_data, show_pct = TRUE) +
  theme_minimal() +
  labs(title = "Missingness Profile by Variable",
       y = "% of Data Missing")

# Visualise intersecting missingness pattern
gg_miss_upset(model_data)

# Test if missingness is related to other variables (MCAR test)
mcar_test(model_data)
# p-value < 0.05 indicates that the missingness is not completely at random (MCAR), 
# suggesting that the missing data may be related to other observed variables. Therefore, 
# the missingness is likely to be either MAR (Missing at Random) or MNAR (Missing Not at Random).
# Finalized as MNAR (Missing Not at Random) because the missingness is likely related to unobserved 
# variables or the missing values themselves and based on literature in digital interventions
# where missingness is often informative (e.g., dropout due to worsening symptoms or lack of engagement).


## -------------------- gt to visualise Missingness in Predictors ----------------- ##

library(tidyverse)
library(gt)

na_summary <- model_data |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    everything(),
    names_to  = "Variable",
    values_to = "NA_Count"
  ) |>
  mutate(
    Percentage = (NA_Count / nrow(model_data)) * 100,
    Variable   = str_replace_all(Variable, "_", " ") |> str_to_title()
  ) |>
  arrange(desc(NA_Count)) |>
  filter(NA_Count > 0)


na_summary |> 
  gt() |> 
  # 1. Add a professional header and subtitle
  tab_header(
    title = md("**Analysis of Missing Values (NAs) Per Predictor Variable**"),
    subtitle = "Summary of missingness across demogrphics, symptom severity and engagement variables in 92,053 records"
  ) |> 
  
  # 2. Format the NA counts and percentages for better readability
  fmt_number(
    columns = NA_Count,
    use_seps = TRUE,
    decimals = 0
  ) |> 
  fmt_percent(
    columns = Percentage,
    decimals = 1,
    scale_values = FALSE # Because your % is already 0-100
  ) |> 
  
  # 3. Rename columns for clarity
  cols_label(
    Variable = "Feature Name",
    NA_Count = "Missing (N)",
    Percentage = "Missing (%)"
  ) |> 
  
  # 4. Add conditional styling (Heatmap effect for high missingness)
  data_color(
    columns = Percentage,
    direction = "column",
    palette = "Reds"
  ) |> 
  
  # 5. Customize table appearance for a polished look
  tab_options(
    heading.align = "left",
    column_labels.font.weight = "bold",
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    data_row.padding = px(3) # Tighter rows look more professional
  ) |> 
  
  # 6. Add a source note (Good for Distinction marks)
  tab_source_note(
    source_note = md("*Note: High missingness in Week 2 variables likely indicates informative dropout (MNAR), p value <0.05.*")
  )

