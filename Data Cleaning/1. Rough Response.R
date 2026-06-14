library(tidyverse) # Data Manipulation
library(janitor)   # Data Cleaning



# Load dataset
raw_responses <- read_csv("responses.csv")


########################### RESPOSE CLEANING & WRANGGLING ###########################
names(raw_responses)
colSums(is.na(raw_responses))
table(raw_responses$age)


# Select relevant variables
responses <- raw_responses |> 
  select(-c(6:7), -c(12,19,20), -c(28:54), -c(58,61), how_has_life_improved7, -improved_life2, 
         would_use_again7, -mood_anxiety_total7, -mood_depression_total7, -how_app_was_helpful2, 
         -how_app_was_helpful7, -easy_to_use7)  


names(responses)
colSums(is.na(responses))
responses |> distinct(code_id) |> pull()
table(responses$code_id, useNA = "ifany")
sum(is.na(responses$code_id))

responses |> count(code_id, sort = T) |> print(n=20)
responses |> count(gender, sort = T)

# Check unique values
unique(responses$would_use_again2)
unique(responses$how_has_life_improved2)[1:50]


# clean dataset 
edit_response <- responses |> 
  mutate(access_type = str_remove(access_type, stringr::fixed("AccessType."))) |> 
  mutate(age = case_when(age %in% c("65 i powyżej", "10000", "100") ~ "65 & over", 
                         age %in% c("15-24", "16-24") ~ "16-24",
                         age == "NaN" ~ NA, 
                         TRUE ~ age)) |> 
  mutate(age = str_trim(age)) |> 
  mutate(age = if_else(str_detect(age, regex("65 and over", ignore_case = TRUE)),
                       "65 & over", age)) |> 
  filter(easy_to_use2 %in% c("No", "Yes", NA)) |> 
  filter(is.na(how_has_life_improved2) | how_has_life_improved2 != "bananas") |> 
  filter(would_use_again2 %in% c("No", "Yes", NA))


# Check cleaned dataset
edit_response |> distinct(how_has_life_improved2)
table(edit_response$ethnic_group, useNA = "ifany")
colSums(is.na(edit_response))
table(edit_response$age)


# Clean ethnic_group
ethnic_response <- edit_response |> 
  mutate(ethnic_cat = str_to_lower(str_trim(ethnic_group))) |> 
  mutate(
    ethnic_cat = case_when(
      # Mixed first: look for "mixed" or explicit "multiple ethnic groups"
      str_detect(ethnic_cat, regex("mixed|multiple ethnic groups|white & (asian|black)")) ~ "Mixed",
      
      # Black: african, caribbean, black british
      str_detect(ethnic_cat, regex("black|african|caribbean|black & (american|welsh|scottish|british)")) ~ "Black",
      
      # Asian: asian, indian, pakistani, chinese, japanese, korean, filipino
      str_detect(ethnic_cat, regex("asian|indian|pakistani|chinese|japanese|korean|filipino")) ~ "Asian",
      
      # White: Majority european groups
      str_detect(ethnic_cat, regex("white|scottish|english|american|welsh|northern irish|canadian|greek|french|dutch|spanish
                                   british|polish|irish|european|australian|italian|hungarian|gypsy|czech|portuguese|bulgarian")) ~ "White",
      
      # Missing values
      is.na(ethnic_cat) ~ NA,
      
      # Everything else like: Middle east, Latinos etc.
      TRUE ~ "Other"
    )
  ) 

ethnic_response |> 
  count(ethnic_cat, sort = TRUE)



# Clean gender 
gender_response <- ethnic_response |> 
  mutate(gender_cat = str_to_lower(str_trim(gender))) |> 
  mutate(
    gender_cat = case_when(
      # Check for strings that ONLY contain male terms (not also female terms)
      str_detect(gender_cat, regex("\\bmale\\b|\\bman\\b|mężczyzna|my sex is male")) ~ "Male",
      
      # Check for strings that ONLY contain female terms (not also male terms)  
      str_detect(gender_cat, regex("\\bfemale\\b|\\bwoman\\b|kobieta|my sex is female")) ~ "Female",
      
      # First check for NAs
      is.na(gender_cat) ~ NA,
      
      # Default for anything else
      TRUE ~ "Other"
    )
  ) 

gender_response |> 
  count(gender_cat, sort = TRUE)



# Final cleaned dataset
new_responses <- gender_response |> 
  rename(age_cat = age) |> 
  select(-gender, -open_time, -ethnic_group,
         -last_open_time, -user_created_on_time)


names(new_responses)
colSums(is.na(new_responses))
new_responses |> distinct(access_type)
table(new_responses$access_type, useNA = "ifany")


# Unique values
sum(duplicated(new_responses)) # check sum of duplicated rows



# Save New dataset
write_csv(new_responses, "new_responses.csv")
