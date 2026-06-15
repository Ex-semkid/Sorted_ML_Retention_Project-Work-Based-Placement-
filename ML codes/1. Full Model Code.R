
# Load libraries
library(tidyverse)  # For data manipulation and visualization
library(tidymodels) # Includes parsnip, recipes, workflows, etc.
library(xgboost)    # Engine for XGBoost
library(ranger)     # Engine for Random Forest
library(glmnet)     # Engine for Regularized Logistic Regression
library(vip)        # For variable importance plots
library(gt)         # For elegant tables
library(doFuture)   # For parallel processing with tidymodels
library(doParallel) # For parallel processing with tidymodels
library(themis)     # For step_downsample to handle class imbalance
library(embed)      # For step_lencode_glm or step_lencode_mixed 
library(patchwork)  # For combining ggplots into a single layout
tidymodels_prefer() # Avoid conflicts with dplyr and other tidyverse packages
options(tidymodels.dark = TRUE) # Use dark theme for tidymodels plots 


model_data <- read_csv("model_data.csv") # Load your dataset (adjust the path as needed)
str(model_data)


# Remove 'id' and convert 'retention' to factor with levels "Retain" and "Churn"
model_data <- model_data |> 
  select(-id) |> 
  mutate(retention = factor(retention, levels = c("Retain", "Churn"))) # Retain as positive class


# Initial data exploration
str(model_data)
summary(model_data)
model_data |> count(retention, sort = T) |> mutate(prop = n/sum(n))
colSums(is.na(model_data))
table(model_data$age_cat)

model_data |> distinct(reader) |>  print(n = Inf)



# Enable parallel processing for faster tuning
cores <- max(1, parallel::detectCores(logical = FALSE) - 1) # Auto Detect number of physical cores
cl <- makePSOCKcluster(cores) # Create cluster
registerDoFuture() # Register doFuture for parallel processing
plan(cluster, workers = cl) # Set future plan to use cluster
# foreach will now use doparallel with future backend


# Set seed for reproducibility
set.seed(77)

# Prepare data for modeling: Split into training and testing sets (70-30 split, stratified by retention)
split <- initial_split(model_data , prop = 0.70, strata = retention)
train <- training(split)
test <- testing(split)


# Create preprocessing recipe 
model_recipe <-  recipe(retention ~ ., data = train) |> 

  # 1. Handle Surprises (Novel levels in Punjabi tracks, etc.)
  step_novel(all_nominal_predictors()) |>  
  
  
  # 2. --------- Numeric NA Handling ---------------
  # Imputation of low Missing numeric and categorical
  step_impute_median(progress_percent) |> 
  step_impute_knn(code_cat) |> 
  
  # Impute high missing num. vars. (bag impute is more robust
  # because in predicts missing values based on other predictors, 
  # rather than just using a single statistic like mean or median)
  step_impute_bag(all_numeric_predictors()) |>
  
  
  
  # 3. --------- Categorical NA & High cardinality Handling -------------
  # High-Cardinality Encoding (Mixed/Shrinkage) including high missing values
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(retention)) |> 
  
  
  # ------- Continue other feature Engineering ------ #
  # 4. Handle Skewness (Yeo-Johnson can handle zero and negative values)
  step_YeoJohnson(all_numeric_predictors()) |> 
  
  # 5.  Remove extro zero variance
  step_zv(all_predictors()) |> 
  
  # 6. Normalize on same scale
  step_normalize(all_numeric_predictors()) |>
  
  # 7. Step down "Churn" by 1 to balance "Retain"
  step_downsample(retention, under_ratio = 1) # Handles class imbalance


# ----------------- Check the recipe steps ----------
p <- prep(model_recipe) # Preprocess the recipe on the training data
jp <- juice(p) # Extract the preprocessed training data as a tibble

str(jp) # Check structure of preprocessed data
table(jp$retention) # Check class balance after downsampling
# --------------------------------------------------- #


# ------------ Model Continues from here-------------- #
# Define model specifications
log_spec <- logistic_reg(
  penalty = tune(),       # Regularization penalty (L1/L2 regularization)
  mixture = tune()) |>    # Mixture between L1 (lasso) and L2 (ridge)
  set_engine("glmnet") |> # Use glmnet engine for regularized logistic regression
  set_mode("classification")


rf_spec <- rand_forest(
  mtry  = tune(), # Number of predictors sampled for splitting at each node
  min_n = tune(), # Minimum number of data points in a node
  trees = tune()  # Number of trees
) |> 
  set_engine("ranger", # Use ranger engine for Random Forest
             importance = "permutation") |>  # For better variable importance measures
  set_mode("classification")


xgb_spec <- boost_tree(
  trees          = tune(), # Number of trees (iterations)
  mtry           = tune(), # Number of predictors sampled for splitting at each node (max_features in xgboost) 
  min_n          = tune(), # Minimum number of data points in a node
  tree_depth     = tune(), # Typical range: 3-8
  learn_rate     = tune(), # Typical range: 0.01-0.3
  loss_reduction = tune()  # Minimum loss reduction (gamma)
) |> 
  set_engine("xgboost",                       # Use xgboost engine for XGBoost
             objective   = "binary:logistic", # "multi:softprob" for multi level prediction
             tree_method = "hist") |>         # Use GPU-accelerated histogram for faster training on larger datasets
set_mode("classification")



# Create workflow set
models <- workflow_set(
  preproc = list(model_recipe),
  models  = list(
    glmnet  = log_spec,
    rf      = rf_spec,
    xgboost = xgb_spec
  )
)


# Cross validation
set.seed(23)
folds <- vfold_cv(train, v = 5, strata = retention)

# Control hyper parameter tuning with Bayesian optimization
bayes_control <- control_bayes(no_improve = 10L,  # Stop after 10 iterations without improvement
                               time_limit = 30,   # Time limit in minutes
                               save_pred  = TRUE, # Save predictions for best model
                               verbose    = TRUE, # Print progress
                               allow_par  = TRUE) # Allow parallel processing during tuning


# Loop over Tuned hyper parameters
results <- models |> 
  workflow_map(
    resamples = folds,
    verbose = TRUE,
    metrics = metric_set(mcc, pr_auc, f_meas), # Focus on PR AUC, MCC and F1 Score for imbalanced classification
    control = bayes_control
  ) 


# Collect metrics
model_metrics <- results |> 
  collect_metrics()

# Print results
print(model_metrics, n = Inf)


# Rank metrics 
rk <- rank_results(results,
             rank_metric = "f_meas",
             select_best = TRUE)

# Visualize mean metrics across models
rk |>
  select(wflow_id, .metric, mean) |>
  ggplot(aes(x = reorder(wflow_id, mean), y = mean, fill = .metric)) +
  geom_col(position = "dodge", width = 0.7) + theme_classic() +
  
  # Percentage label on top of each bar
  geom_text(
    aes(label = scales::percent(mean, accuracy = 0.1)),
    position = position_dodge(width = 0.7),
    vjust    = -0.4,
    size     = 3.2,
    fontface = "bold",
    colour   = "grey30"
  ) +
  
  scale_fill_manual(
    values = c("f_meas" = "red3", "mcc" = "green4", "pr_auc" = "purple2"),
    labels = c("f_meas" = "F1 Score", "mcc" = "MCC", "pr_auc" = "PR-AUC"),
    name   = "Metric"
  ) +
  
  labs(
    title    = "Model Comparison: Imbalance-Focused Metrics",
    subtitle = "F1, MCC and PR-AUC across cross-validation folds",
    x        = "Model",
    y        = "Mean Score"
  ) 



# Vizualise workflow ranking across metrics
results |>
  autoplot() +
  scale_colour_manual(
    values = c("rand_forest"  = "dodgerblue3",
               "boost_tree"   = "coral1",
               "logistic_reg" = "seagreen3"),
    labels = c("rand_forest"  = "Random Forest",
               "boost_tree"   = "XGBoost",
               "logistic_reg" = "Elastic Net")
  ) +
  labs(
    title    = "Model Comparison: Workflow Ranking Across Metrics",
    subtitle = "Ranked by F1 Score, MCC & PR-AUC: Error bars show 95% confidence intervals",
    x        = "Workflow Rank",
    y        = "Metric Value",
    colour   = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey23", size = 10),
    strip.text       = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "violet", colour = NA),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold")
  )



# Generate PR curves
pr_results <- results |> 
  collect_predictions() |> 
  group_by(wflow_id) |> 
  pr_curve(retention, .pred_Retain)


# Plot PR curves
pr_plot <- pr_results |> 
  ggplot(aes(x = recall, y = precision, colour = wflow_id)) +
  geom_path(linewidth = 1) +
  geom_abline(lty = 3, colour = "grey50") +
  coord_equal() +
  scale_colour_manual(
    values = c("recipe_rf"     = "dodgerblue3",   # blue
               "recipe_xgboost" = "coral1",  # orange
               "recipe_glmnet" = "seagreen3"),  # green
    labels = c("recipe_rf"     = "Random Forest",
               "recipe_xgboost" = "XGBoost",
               "recipe_glmnet" = "Elastic Net")
  ) +
  labs(
    title    = "Precision-Recall Curves for Different Models",
    subtitle = "Higher curve = better performance on imbalanced data",
    x        = "Recall",
    y        = "Precision",
    colour   = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title     = element_text(face = "bold", size = 14),
    plot.subtitle  = element_text(colour = "grey23", size = 10),
    legend.position = "bottom",
    legend.title   = element_text(face = "bold")
  )

print(pr_plot)



# Final model evaluation on test set
final_results <- results |> 
  workflowsets::extract_workflow_set_result("recipe_rf") |> 
  select_best(metric = "pr_auc") # Select best hyperparameters based on PR-AUC

final_wf <- workflowsets::extract_workflow(results, "recipe_rf") |> 
  finalize_workflow(final_results)


# Train final model
final_model <- final_wf |> 
  fit(data = train)


# save model matrices and model fit
saveRDS(final_model, "final_model.rds")


# Evaluate on test set
set.seed(33)
test_predictions <- final_model |> 
  augment(new_data = test) 

# Evaluate on train set
train_predictions <- final_model |> 
  augment(new_data = train) 

# Classification metrics test
test_metrics <- metric_set(accuracy, sensitivity, specificity, precision, recall, roc_auc, pr_auc, mcc, f_meas)
rf_m1 <- test_predictions |> test_metrics(truth = retention, estimate = .pred_class, .pred_Retain)
rf_m1

# Classification metrics train
test_metrics <- metric_set(accuracy, sensitivity, specificity, precision, recall, roc_auc, pr_auc, mcc, f_meas)
rf_m2 <- train_predictions |> test_metrics(truth = retention, estimate = .pred_class, .pred_Retain)
rf_m2



# Confusion matrix on test
conf_matrix <- test_predictions |>  
  conf_mat(truth = retention, estimate = .pred_class)

# Step 9: Visualize Results
autoplot(conf_matrix, type = "heatmap") +
  scale_fill_gradient(low = "azure", high = "aquamarine4") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Confusion Matrix - Random Forest Model",
    subtitle = "Held-out test set (n = 27,617) | Positive class: Retain",
    x = "True class",
    y = "Predicted class",
    fill = "Count"
  )



# 10. Feature importance 
rf_imp <- extract_fit_engine(final_model)
vip::vi(rf_imp) |>
  mutate(
    Variable   = gsub("_", " ", Variable) |> str_to_title(),
    Importance = round(Importance / sum(Importance) * 100, 1),
    Category   = case_when(
      Variable %in% c("Mood Depression Total0", "Mood Anxiety Total0",
                      "Mood Anxiety Total2",   "Mood Depression Total2") ~ "Symptom Severity",
      Variable %in% c("Would Use Again2", "Tracks Listened2", "Easy To Use2",
                      "Tracks Listened0", "How Has Life Improved2", "Module Id",
                      "Reader", "Track Id", "Track Duration",
                      "Progress Percent")                                 ~ "Engagement",
      Variable %in% c("Code Cat", "Ethnic Cat", "Gender Cat",
                      "Access Type", "Age Cat")                           ~ "Demographics",
      TRUE ~ "Category"
    )
  ) |>
  arrange(Category, desc(Importance)) |>
  gt(groupname_col = "Category") |>
  tab_header(
    title    = md("**Random Forest Feature Importance**"),
    subtitle = "Categoried based on Baseline Demographics, Symptom Severity & Early Engagement Predictors"
  ) |>
  cols_label(Variable = "Variable", Importance = "Importance (%)") |>
  fmt_number(columns = Importance, decimals = 1) |>
  data_color(columns = Importance, palette = "Greens") |>
  tab_style(
    style     = list(cell_fill(color = "steelblue"),
                     cell_text(color = "white", weight = "bold")),
    locations = cells_row_groups()
  ) |>
  tab_source_note(md(
    "*Note: Importance = Mean decrease based on permution, ranked in descending order. Total importance sum up to 100%*"
  )) |>
  tab_options(
    heading.align             = "left",
    data_row.padding          = px(1),
    column_labels.font.weight = "bold"
  )


# View top 10 predictors in a bar plot
vip::vi(rf_imp) |>
  slice_max(Importance, n = 10) |>
  mutate(Variable = gsub("_", " ", Variable) |> str_to_lower()) |>
  ggplot(aes(x = Importance, y = reorder(Variable, Importance), fill = Importance)) +
  geom_col(width = 0.8, colour = "black", alpha = 0.9) +
  scale_fill_gradient(low = "darkseagreen1", high = "seagreen4",   # light → dark teal
                      name = "Importance") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Feature Importance: Random Forest Model",
    subtitle = "Top 10 predictors (≥5%  Importance) based on permutation",
    x        = "Importance Score",
    y        = "Features"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(colour = "grey50", size = 10),
    panel.grid.major.y = element_blank(),
    legend.position    = "right"
  )


#-- Generate 'PR Curve' Data
pr_curve_data <- test_predictions |> 
  pr_curve(truth = retention, .pred_Retain)


# Assuming pr_curve_data was created using pr_curve()
pr_curve_data |> 
  ggplot(aes(x = recall, y = precision)) +
  geom_path(size = 1.2, color = "aquamarine3") + # Professional Orange for PR
  coord_equal() +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  ) + 
  labs(
    title = "Precision-Recall Curve",
    subtitle = paste0("Model Focus: Ability to Identifying Retainers (PR-AUC: ", 
                      round(rf_m1$.estimate[[9]], 3), ")"),
    x = "Recall (Sensitivity)",
    y = "Precision"
  )



