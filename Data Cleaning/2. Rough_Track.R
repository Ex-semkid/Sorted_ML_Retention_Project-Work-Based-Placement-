library(tidyverse) # For data manipulation and visualization
library(janitor)   # For cleaning column names and data frames


raw_tracks <- read_csv("tracks.csv") |> clean_names()

########################### TRACKS CLEANING & WRANGLING ############################

names(raw_tracks)
colSums(is.na(raw_tracks))


# Identify rows with parsing issues
raw_tracks %>%
  mutate(row_num = row_number()) |> 
  filter(date_time_of_listening == "Invalid date") |> 
  select(row_num, date_time_of_listening) # Just show the row number and the error


# Explore unique values in dateTimeOfListening
raw_tracks |> 
  count(date_time_of_listening, sort = TRUE) |> 
  head(20)


# Check for hidden spaces or formatting issues
raw_tracks |> 
  mutate(cleaned_text = str_trim(date_time_of_listening)) |> # Removes hidden spaces
  count(cleaned_text, sort = TRUE) |> 
  head(20)


# find rows that do not match expected pattern
raw_tracks %>%
  filter(!str_detect(date_time_of_listening, "-at-")) |> 
  distinct(date_time_of_listening)


# Process the tracks, skipping the invalid rows
final_tracks <- raw_tracks |> 
  # 1. Drop the "Invalid date" rows immediately
  filter(date_time_of_listening != "Invalid date") |> 
  select(-date_time_of_listening)

  

# It counts the occurrences and picks the top one for each group. It also handles ties 
#   (if two hours are equally popular) better than a custom function.
final_tracks |> 
  group_by(time_of_day) |> 
  tally(name = "frequency") |> 
  slice_max(frequency, n = 1) |> 
  ungroup()


final_tracks |>
distinct(module_id) |> print(n=Inf)

final_tracks |> 
count(track_id, sort = T) |> print(n=Inf)


# Unique values
sum(duplicated(final_tracks)) # check sum of duplicated rows
final_tracks <- distinct(final_tracks)

# Save final dataset
write_csv(final_tracks, "final_tracks.csv")
