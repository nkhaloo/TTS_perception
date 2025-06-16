library(tidyverse)

set.seed(456)  # Explicitly set seed for reproducibility

# Load the dataset explicitly and remove 'if_ex_y' column
voices <- read_csv("data/metadata/voices_metadata.csv") %>%
  select(-if_ex_y)

# Explicitly remove excluded voices
voices_clean <- voices %>%
  filter(excluded != 'y' | is.na(excluded))

# Count explicitly available black males
num_black_males <- voices_clean %>%
  filter(race == 'b', gender == 'm') %>%
  nrow()

# Randomly sample black females explicitly matching black males
selected_black_females <- voices_clean %>% 
  filter(race == 'b', gender == 'f') %>%
  slice_sample(n = num_black_males)

# Include explicitly all black males
selected_black_males <- voices_clean %>%
  filter(race == 'b', gender == 'm')

# Combine explicitly balanced black voices
selected_black_voices <- bind_rows(selected_black_females, selected_black_males)

# Final gender counts explicitly
num_black_females_final <- nrow(selected_black_females)
num_black_males_final <- nrow(selected_black_males)

# Randomly sample white voices explicitly matching black voice counts
selected_white_females <- voices_clean %>%
  filter(race == 'w', gender == 'f') %>%
  slice_sample(n = num_black_females_final)

selected_white_males <- voices_clean %>%
  filter(race == 'w', gender == 'm') %>%
  slice_sample(n = num_black_males_final)

# Combine explicitly balanced white voices
selected_white_voices <- bind_rows(selected_white_females, selected_white_males)

# Combine races explicitly into final dataset
final_selected_voices <- bind_rows(selected_black_voices, selected_white_voices) %>%
  select(-excluded) %>%  
  arrange(race, gender) %>%       # Arrange explicitly by race and gender
  group_by(race, gender) %>%  
  mutate(
    token = row_number(),  # explicitly reset token per race/gender
    voice_code = paste0(toupper(race), toupper(gender), token)  # e.g., BF1, WM2
  ) %>%  
  ungroup() %>%
  select(voice_code, everything(), -token) %>%  # clearly order columns
  mutate(id = row_number()) %>%  
  relocate(id, voice_code)                     # move explicitly to front

# Replace explicitly "shimmer" with "alex"
final_selected_voices <- final_selected_voices %>%
  mutate(voice_id = if_else(voice_id == "shimmer", "alex", voice_id))

# Check explicitly balanced final counts
final_counts <- final_selected_voices %>%
  count(race, gender)

cat("Final balanced voice counts (race × gender):\n")
print(final_counts)

cat("\nFinal selected voices with voice_code:\n")
print(final_selected_voices)

# Save explicitly the final balanced dataset
write_csv(final_selected_voices, "data/metadata/balanced_selected_voices_corrected.csv")
