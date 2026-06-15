

# Load libraries
library(tidyverse)
library(tidymodels)  # Includes parsnip, recipes, workflows, etc.
library(ranger)     # Engine for randomforest
library(vip)
library(doFuture)
library(doParallel)
library(themis)
library(embed) # For step_lencode_glm and step_lencode_mixed


model_data <- read_csv("model_data.csv")
colSums(is.na(model_data))
str(model_data)


model_data <- model_data |> 
  select(-id) |> 
  mutate(across(where(is.character), as.factor)) |> 
  mutate(retention = factor(retention, levels = c("Retain", "Churn"))) # Retain as positive class

str(model_data)
summary(model_data)
table(model_data$would_use_again2)

model_data |> 
  count(retention) |> 
  mutate(prop = prop.table(n))

sum(is.na(model_data$retention))

# Enable parallel processing for faster tuning
cores <- max(1, parallel::detectCores(logical = FALSE) - 1) # Auto Detect number of physical cores
cl    <- makePSOCKcluster(cores) # Create cluster
registerDoFuture() # Register doFuture for parallel processing
plan(cluster, workers = cl) # Set future plan to use cluster
# foreach will now use doparallel with future backend


# Set seed for reproducibility
set.seed(23)

# 1. Prepare data (using iris dataset)
split <- initial_split(model_data , prop = 0.70, strata = retention)
train <- training(split)
test <- testing(split)


# 2. Create preprocessing recipe
model_recipe <-  recipe(retention ~ ., data = train) |>  

  # 1. Handle Surprises (Novel levels in Punjabi tracks, etc.)
  step_novel(all_nominal_predictors()) |>  
  
  
  # 2. --------- Numeric NA Handling ---------------
  # Imputation of low Missing numeric and categorical
  step_impute_median(progress_percent) |> 
  step_impute_knn(code_cat) |> 
  
  # Handle Numeric Missingness (Indicator + Imputation), NA = 1, Not NA = 0.
  step_indicate_na(all_numeric_predictors()) |> 
  
  # Imputation of Missing numerics with Bagged Trees to filling in the original missingness
  step_impute_bag(all_numeric_predictors()) |> 
  
  
  # 3. --------- Categorical NA Handling --------------
  # Handle Missing Categories
  step_unknown(all_nominal_predictors(), new_level = "Missing") |> 
  
  
  # 4. High-Cardinality Encoding (Mixed/Shrinkage) - This is often better than one-hot for tree-based models, especially with many categories
  step_lencode_mixed(track_id, module_id, code_cat, reader, outcome = vars(retention)) |> 
  
  # Alternative: Dummy variables, tree-based models can handle categorical variables without encoding, but if you want to use one-hot:
   step_dummy(all_nominal_predictors(), one_hot = TRUE, sparse = "auto") |> 
  
  # 5. Handle Skewness in numerics (Yeo-Johnson can handle zero and negative values)
  step_YeoJohnson(all_numeric_predictors()) |> 
  
  # 6. Remove extro zero variance
  step_zv(all_predictors()) |> 
  
  # 7. Normalize on same scale
  step_normalize(all_numeric_predictors()) |>
  
  # 8. Step down "Churn" by 1 to balance "Retain"
  step_downsample(retention, under_ratio = 1) # Handles class imbalance


p   <- prep(model_recipe)
jp <- juice(p)
str(jp)
table(jp$retention)



# 3. Define XGBoost model specification
rf_spec <- rand_forest(
  mtry = tune(),          # Number of predictors sampled for splitting at each node
  min_n = tune(),         # Minimum number of data points in a node
  trees = tune()         # Number of trees
) |> 
  set_mode("classification") |>
  set_engine("ranger", 
             importance = "permutation")

# 4. Create workflow0
rf_workflow <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(rf_spec)


# 5. Create tuning grid
tune_grid <- grid_space_filling(
  mtry(range = c(1, 20)), # Adjust based on number of predictors
  min_n(),
  trees(range = c(200, 1000)),
  size  = 10
) # use grid_regular for manageable datasets and use levels instead of size 

# 6. Perform cross-validation tuning
set.seed(23)
folds <- vfold_cv(train, v = 5, strata = retention)

tune_results <- rf_workflow %>%
  tune_grid(
    resamples = folds,
    grid = tune_grid,
    metrics = metric_set(f_meas, pr_auc, mcc),
    control = control_grid(
      verbose = TRUE, 
      allow_par = TRUE
#     parallel_over = "everything", # Most efficient for your size=10 grid
    )
  )


show_best(tune_results, metric = "pr_auc")
autoplot(tune_results) + theme_bw()


# Example: Visualization of aggregated metrics
tune_results %>%
  collect_metrics() %>%
  ggplot(aes(x = .metric, y = mean, colour = .metric)) +
  geom_step() +
  labs(
    title = "Aggregated Metrics After Tuning",
    x = "Metric",
    y = "Mean Value"
  )


# 7. Select best model and finalize workflow
best_params <- select_best(tune_results, metric = "pr_auc")
final_workflow <- finalize_workflow(rf_workflow, best_params)

# 8. Train final model
final_model <- final_workflow %>%
  fit(data = train)

# 9. Evaluate on test set
test_predictions <- test %>%
  bind_cols(
    predict(final_model, test),
    predict(final_model, test, type = "prob")
  )

# or 

test_predictions <- final_model %>%
  augment(new_data = test)

# Classification metrics
test_metrics <- metric_set(accuracy, sensitivity, specificity, mcc, f_meas, recall, roc_auc, pr_auc)
xgb_m <- test_predictions %>% test_metrics(truth = retention, estimate = .pred_class, .pred_Retain)
print(xgb_m)

# Confusion matrix
conf_matrix <- test_predictions %>% 
  conf_mat(truth = retention, estimate = .pred_class)


# Step 9: Visualize Results
autoplot(conf_matrix, type = "heatmap") +
  scale_fill_gradient(low = "white", high = "seagreen") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Confusion Matrix - XGBoost Model",
    x = "True Class",
    y = "Predicted Class",
    fill = "Count"
  )


# 10. Feature importance
xgb_imp <- extract_fit_engine(final_model) 

vip::vi(xgb_imp) |> print(n=Inf)

vip(xgb_imp, num_features = 23)


#--- Generate 'ROC Curve' Data
roc_curve_data <- test_predictions %>%
  roc_curve(truth = retention, .pred_Retain) # Class probabilities

#-- Plot the ROC Curve
autoplot(roc_curve_data) 


# 1. Generate 'PR Curve' Data
pr_curve_data <- test_predictions %>%
  pr_curve(truth = retention, .pred_Retain)

#-- Plot the ROC Curve
autoplot(pr_curve_data)




# Stop parallel processing
parallel::stopCluster(cl)
plan(sequential)   # reset back to sequential processing
################## ---- End of Parallization in Clusters---- #################



library(vip)
library(ggplot2)

vip(xgb_imp, 
    num_features = 47,           # Show top 20
    geom = "col",                # Use columns
    aesthetics = list(
      fill = "aquamarine3",          # Professional "Sorted Blue"
      color = "white",           # White border for crispness
      alpha = 0.9,               # Slight transparency
      width = 0.8                # Thicker bars
    )) +
  theme_minimal(base_size = 12) + 
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
    
  ) +  scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
  
  labs(
    title = "Feature Importance: RF Model",
    subtitle = "All predictors including NA categories based on Gain",
    x = "Features",
    y = "Importance Score"
  )




# Assuming roc_curve_data was created using roc_curve()
roc_curve_data %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  # The "Chance" line (diagonal)
  geom_abline(lty = 3, color = "grey50", linewidth = 0.8) +
  # The actual ROC curve
  geom_path(linewidth = 1.2, color = "aquamarine3") + 
  # Force the plot to be a perfect square
  coord_equal() + 
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "italic")
  ) +
  labs(
    title = "ROC Curve: Mental Health App Retention",
    subtitle = paste0("Final XGBoost Model (AUC: ", round(0.981, 3), ")"),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  )


# Assuming pr_curve_data was created using pr_curve()
pr_curve_data %>%
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
    subtitle = paste0("Model Focus: Identifying User Churn (PR-AUC: ", 
                      round(xgb_m$.estimate[[8]], 3), ")"),
    x = "Recall (Sensitivity)",
    y = "Precision (PPV)"
  )

