library(tidyverse)

set.seed(456)  # Explicitly set seed for reproducibility

# Load the dataset explicitly and remove 'if_ex_y' column
voices <- read_csv("data/metadata/voices_metadata.csv") %>%
  select(-if_ex_y)

# Explicitly remove excluded voices
voices_clean <- voices %>%
  filter(excluded != 'y' | is.na(excluded))


# Sample 8 from each group:w,M; w,F; b,M; b,F
voices_sampled <- voices_clean %>%
  filter(race %in% c("w", "b"), gender %in% c("M", "F")) %>%
  group_by(race, gender) %>%
  slice_sample(n = 8) %>%
  arrange(race, gender) %>% 
  mutate(code = paste0(toupper(race), gender, row_number())) %>%
  ungroup()

# 
# Save explicitly the final balanced dataset
write_csv(voices_sampled, "data/metadata/balanced_selected_voices_corrected.csv")
