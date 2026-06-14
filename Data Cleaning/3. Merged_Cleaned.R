library(tidyverse) # For data manipulation and visualization
library(readxl)    # For reading Excel files

# Load Dataset
new_responses <- read_csv("new_responses.csv")
final_tracks <- read_csv("final_tracks.csv")
referral_codes <- read_xlsx("referral_codes.xlsx")


# Merge datasets
responses_tracks <- new_responses |> 
  left_join(final_tracks, by = "id") 


# Explore the merged dataset
colSums(is.na(responses_tracks)) # Check for missing values
str(responses_tracks) # Check variable types
table(responses_tracks$access_type) # Check distribution of access types
colSums(is.na(responses_tracks)) # Check for missing values 
table(responses_tracks$reader, useNA = "ifany") # Check distribution of reader types, including NAs#


# Merge with referral codes
responses_tracks_codes <- responses_tracks |> 
  left_join(referral_codes, by = "code_id")

# Explore the variable names merged dataset
names(model_data)


# -------------------------- ML Dataset ----------------------------- #
# 1. First, define the cohort of users who have been around long enough
model_data <- responses_tracks_codes  |> 
  filter(days_since_first_open >= 49) |> # people who have been available for 7 weeks+
  
  # 2. Define retention based on activity
  mutate(retention = 
           case_when(
             tracks_listened7 > 0 | 
               !is.na(would_use_again7) | 
               !is.na(how_has_life_improved7) ~ "Retain", 
             TRUE ~ "Churn"  # Handles NAs and 0s in one go
           )
  ) |> 
  
  # 3. Remove irrelevant and week 7 variables
  select(-code_id, 
         -tracks_listened7, 
         -would_use_again7,
         -days_since_first_open, 
         -how_has_life_improved7) |> 
  select(1:3, 13, 14, 20, everything()) |> distinct()



names(model_data) # Check variable names
colSums(is.na(model_data)) # Check for missing values
table(model_data$retention) # Check distribution of retention classes
str(model_data) # Check variable types, especially the target variable

# Check class balance
model_data |> count(retention) |> mutate(prop = n/sum(n))



# Save cleaned dataset
write_csv(model_data, "model_data.csv")

