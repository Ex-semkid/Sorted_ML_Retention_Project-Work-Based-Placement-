
library(DALEXtra) # for explain_tidymodels and model_profile (XAi)
library(tidyverse)  # For data manipulation and visualization
library(tidymodels) # Includes parsnip, recipes, workflows, etc.


model_data <- read_csv("model_data.csv") # Load your dataset (adjust the path as needed)
final_model <- readRDS("final_model.rds") # Load Model

# Remove 'id' and convert 'retention' to factor with levels "Retain" and "Churn"
model_data <- model_data |> 
  select(-id) |> 
  mutate(retention = factor(retention, levels = c("Retain", "Churn"))) # Retain as positive class


# Prepare data for modeling: Split into training and testing sets (70-30 split, stratified by retention)
split <- initial_split(model_data , prop = 0.70, strata = retention)
train <- training(split)
test <- testing(split)



# ------Experimenting with Partial Dependence Plots (PDP) for the Random Forest model -----

# 1. Create an 'explainer' for RF model
# 'train_processed' should be the training data
rf_explainer <- explain_tidymodels(
  final_model, 
  data = test |>  select(-retention), 
  y = as.numeric(train$retention == "Retain"),
  label = "Random Forest",
  verbose = FALSE
)

# 2. Calculate the Partial Dependence Profile
pdp_depression <- model_profile(
  explainer = rf_explainer,
  variables = "tracks_listened2",
  type = "partial"
)

# 3. Plot it
plot(pdp_depression) +
  ggtitle("Partial Dependence Plot: tracks listened by week 2 vs. Retention") +
  theme_minimal()
# ----------------------------------------------------------------------------# 



# ---------------------- FUNCTION with Loop ------------------------- #

# Define your highly predictive variables based on the variable importance plot
top4_predictors <- c("would_use_again2", "mood_depression_total0", 
                     "mood_anxiety_total0", "code_cat")

next6_predictors <- c("tracks_listened2", "mood_depression_total2", "mood_anxiety_total2", 
                      "gender_cat", "ethnic_cat", "tracks_listened0") 


# Function to compute and plot PDP for multiple variables
plot_pdp_profiles <- function(explainer, variables, ncol = 2) {
  
  plots <- list()
  
  for (var in variables) {
    pdp <- model_profile(
      explainer = explainer,
      variables = var
    )
    
    p <- plot(pdp) +
      ggtitle(paste("PDP:", var, "vs. Retention")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    # Apply special formatting for code_cat
    if (var == "code_cat") {
      
      # Define your desired category order here
      cat_order <- c("CHARITY", "EDUCATION", "FREE", "NHS", "EMPLOYER",
                     "INTERNAL", "PROMOTION", "RESEARCH", "PURCHASE", "MARKETING")
      
      p <- p +
        scale_x_discrete(limits = cat_order) +   # reorder bars
        theme(
          axis.text.x = element_text(
            angle = 45,       # slant angle
            hjust = 1,        # right-align text to the tick
            vjust = 1,        # vertical nudge
            size  = 8
          )
        )
    }
    
    plots[[var]] <- p
  }
  
  combined <- wrap_plots(plots, ncol = ncol) +
    plot_annotation(title = "Partial Dependence Profiles - Top 4 Predictors")
  
  print(combined)
  return(invisible(plots))
}


set.seed(369) # for reproducibility

# top4_predictors
pdp_plots1 <- plot_pdp_profiles(
  explainer  = rf_explainer,
  variables  = top4_predictors,
  ncol       = 2
)


# Note: Change title to next 6 predictors and change ncol to 3 and re-run the function for 
# the next 6 predictors to get the next set of PDP plots.

# --------------------------------------------------------------------------------------- #


