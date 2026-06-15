# using the random forest model evaluation result (rf_m2 & rf_m1) to compare train vs test performance metrics
# Check Full Model Code.R file. and run the script to get rf_m2 and rf_m1 objects before running this code snippet.

rf_m2 |> select(.metric, Train = .estimate) |>
  left_join(rf_m1 |> select(.metric, Test = .estimate), by = ".metric") |>
  mutate(
    Metric   = c("Accuracy","Sensitivity","Specificity","Precision",
                 "Recall","MCC","F1 Score","ROC-AUC","PR-AUC"),
    Category = c("Overall","Class Performance","Class Performance",
                 "Class Performance","Class Performance",
                 "Imbalance-Robust","Imbalance-Robust",
                 "Discrimination","Discrimination"),
    Diff     = Train - Test,
    across(c(Train, Test, Diff), ~ scales::percent(., accuracy = 0.1)) # Format as percentage with 1 decimal place
  ) |>
  select(Category, Metric, Train, Test, Diff) |>
  gt(groupname_col = "Category") |>
  tab_header(
    title    = md("**Table: Random Forest — Train vs Test Performance**"),
    subtitle = "Final model metrics evaluated on training set vs held-out test set"
  ) |>
  cols_label(Train = "Train (CV)", Test = "Test (Unseen Data)", Diff = "Difference") |>
  tab_style(
    style     = cell_text(color = "red3"),
    locations = cells_body(columns = Diff)
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = Metric %in% c("Accuracy", "MCC", "F1 Score", "PR-AUC"))
  ) |>
  tab_style(
    style     = list(cell_fill(color = "seagreen"),
                     cell_text(color = "white", weight = "bold")),
    locations = cells_row_groups()
  ) |>
  tab_source_note(md(
    "*Note: Train-Test overall __accuaracy__ differences of ≤5% indicate good generalisation with minimal overfitting.*"
  )) |>
  tab_options(
    heading.align             = "left",
    data_row.padding          = px(5), # Reduce padding for a more compact table
    table.border.top.color    = "black",
    column_labels.font.weight = "bold"
  )
