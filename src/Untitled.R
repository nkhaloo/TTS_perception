library(tidyverse)
library(FactoMineR)
library(factoextra)
library(caret)
library(CAvariants)
library(lme4)
library(broom.mixed)
library(lmerTest)

# --------------------------------------------------
# Load data
# --------------------------------------------------
df <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/survey_results/survey_results.csv")

# --------------------------------------------------
# Add speaker-level % REPORTED BLACK EARLY (0–100)
# --------------------------------------------------
df <- df %>%
  group_by(speaker) %>%
  mutate(percent_reported_black = mean(perceived_race == "Black", na.rm = TRUE) * 100) %>%
  ungroup()

# --------------------------------------------------
# RACE AGREEMENT
# --------------------------------------------------
agreement_by_category_and_speaker <- df %>%
  filter(!is.na(speaker)) %>%
  filter(block == "Race") %>%
  mutate(
    # Extract the two-letter speaker category
    speaker_category = str_sub(speaker, 1, 2),
    
    # Expected race of the speaker based on naming system
    true_race = case_when(
      str_starts(speaker, "B") ~ "Black",
      str_starts(speaker, "W") ~ "White",
      TRUE ~ NA_character_
    ),
    
    # Agreement with expected race
    agreement = case_when(
      perceived_race == "Black" & true_race == "Black" ~ TRUE,
      perceived_race == "White" & true_race == "White" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  group_by(speaker_category, speaker) %>%
  summarise(percent_agreement = mean(agreement) * 100, .groups = "drop")

# Category-level summary
agreement_by_category <- agreement_by_category_and_speaker %>%
  group_by(speaker_category) %>%
  summarise(category_percent_agreement = mean(percent_agreement), .groups = "drop")

# View results if desired
agreement_by_category
agreement_by_category_and_speaker

# ---- Visualization: overall agreement by category ----
ggplot(agreement_by_category, aes(x = speaker_category, y = category_percent_agreement, fill = speaker_category)) +
  geom_col(width = 0.6, color = "black", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", category_percent_agreement)), vjust = -0.5, size = 4.5) +
  scale_fill_manual(values = c("BF" = "#c23b23", "BM" = "#6f42c1", "WF" = "#2b7de9", "WM" = "#28a745")) +
  labs(
    x = "Speaker Category",
    y = "Percent Agreement"
  ) +
  ylim(0, 100) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(color = "black")
  )

# ---- Visualization: agreement per speaker ----
ggplot(agreement_by_category_and_speaker,
       aes(x = speaker, y = percent_agreement)) +
  geom_col(fill = "steelblue", color = "black", width = 0.6, alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%", percent_agreement)),
            vjust = -0.5, size = 4) +
  facet_wrap(~ speaker_category, ncol = 2, scales = "free_x") +
  labs(
    x = "Speaker",
    y = "Percent Agreement"
  ) +
  ylim(0, 100) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.spacing = unit(1, "lines")
  )

# --------------------------------------------------
# GENDER AGREEMENT (using gender_scale)
# --------------------------------------------------
gender_agreement_race_gender <- df %>%
  filter(!is.na(gender_scale), !is.na(speaker)) %>%
  mutate(
    # TRUE gender from speaker code (2nd letter)
    true_gender = if_else(str_sub(speaker, 2, 2) == "F", "Female", "Male"),
    
    # TRUE race from speaker code (1st letter)
    true_race = if_else(str_sub(speaker, 1, 1) == "B", "Black", "White"),
    
    # PERCEIVED gender from 1–7 scale
    perceived_gender = if_else(gender_scale >= 4, "Male", "Female"),
    
    # AGREEMENT: 1 = matched, 0 = did not match
    agree = perceived_gender == true_gender
  ) %>%
  group_by(true_race, true_gender) %>%
  summarise(
    percent_agreement = mean(agree) * 100,
    sd_agreement = sd(as.numeric(agree)) * 100,
    n = n(),
    .groups = "drop"
  )

gender_agreement_race_gender

# --------------------------------------------------
# PERSONALITY SCATTER PLOTS
# --------------------------------------------------
plot_personality_scatter <- function(personality_col) {
  
  df %>%
    mutate(
      true_gender = if_else(str_sub(speaker, 2, 2) == "F", "Female", "Male")
    ) %>%
    group_by(speaker, true_gender) %>%
    summarise(
      # use speaker-level % reported Black already computed in df
      percent_reported_black = first(percent_reported_black),
      personality_score = mean(.data[[personality_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(percent_reported_black),
           !is.na(personality_score)) %>%
    
    ggplot(aes(
      x = percent_reported_black,
      y = personality_score,
      color = true_gender
    )) +
    geom_point(size = 4, alpha = 0.9) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
    facet_wrap(~ true_gender, scales = "free_x") +
    labs(
      x = "% Reported Black",
      y = paste("Mean", personality_col),
      color = "True Gender",
      title = paste("Relationship Between % Reported Black and", personality_col)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text = element_text(color = "black"),
      strip.text = element_text(size = 14, face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

# Example calls (comment out if you don’t want to plot every time)
plot_personality_scatter("competent")
plot_personality_scatter("trustworthy")
plot_personality_scatter("friendly")
plot_personality_scatter("funny")
plot_personality_scatter("professional")
plot_personality_scatter("pleasant")
plot_personality_scatter("human_like")

# --------------------------------------------------
# MCA ON PARTICIPANT-LEVEL DEMOGRAPHICS
# --------------------------------------------------
demo <- df %>%
  filter(block == "Demographics") %>%   # USE ONLY THIS BLOCK
  select(
    prolific_id,
    age, ethnicity, dialect, first_language, other_languages,
    gender, technologies_used, ai_use_frequency, ai_attitude
  ) %>%
  distinct(prolific_id, .keep_all = TRUE)

# Convert to factors except prolific_id
demo_factor <- demo %>%
  mutate(across(-prolific_id, as.factor))

# Run MCA
mca_real <- MCA(demo_factor %>% select(-prolific_id), graph = FALSE)

# Number of active variables (not total columns)  <-- FIXED
Q <- ncol(demo_factor) - 1

# Raw eigenvalues from MCA
lambda <- mca_real$eig[, 1]

# Benzécri correction
lambda_benzecri <- (Q / (Q - 1)) * (lambda - (1 / Q))

# Replace negatives with 0 (standard Benzecri step)
lambda_benzecri[lambda_benzecri < 0] <- 0

# Calculate percent variance explained
percent_benzecri <- lambda_benzecri / sum(lambda_benzecri) * 100
percent_benzecri

# Build Benzécri DF for scree plot (FIRST 30 DIMENSIONS)
benzecri_df <- tibble(
  dim = seq_along(percent_benzecri),
  corrected_variance = percent_benzecri
) %>%
  dplyr::filter(dim <= 30)

ggplot(benzecri_df, aes(x = dim, y = corrected_variance)) +
  geom_col(fill = "steelblue") +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Benzécri-Corrected Scree Plot (First 30 Dimensions)",
    x = "Dimension",
    y = "Corrected % Variance Explained"
  ) +
  scale_x_continuous(breaks = 1:30) +
  theme_minimal(base_size = 14)

# Extract MCA dimensions (same N as demo)
mca_scores <- as.data.frame(mca_real$ind$coord[, 1:2])
colnames(mca_scores) <- c("MCA1", "MCA2")

# Attach MCA dims to participant IDs
demo_with_mca <- bind_cols(
  demo_factor %>% select(prolific_id),
  mca_scores
)

# ---- Merge MCA dims back into full df ----
df <- df %>%
  left_join(demo_with_mca, by = "prolific_id")

# --------------------------------------------------
# BUILD MODELING DATASET FOR PERSONALITY
# --------------------------------------------------
# 2. Add true speaker gender
df_with_demo <- df %>%
  mutate(
    true_gender = if_else(str_sub(speaker, 2, 2) == "F", "Female", "Male")
  )

# 3. Build personality dataset (summarized per participant × speaker)
personality_df <- df_with_demo %>%
  filter(block == "Personality") %>%
  group_by(prolific_id, speaker) %>%
  summarise(
    competent    = mean(competent,    na.rm = TRUE),
    trustworthy  = mean(trustworthy,  na.rm = TRUE),
    friendly     = mean(friendly,     na.rm = TRUE),
    funny        = mean(funny,        na.rm = TRUE),
    professional = mean(professional, na.rm = TRUE),
    pleasant     = mean(pleasant,     na.rm = TRUE),
    human_like   = mean(human_like,   na.rm = TRUE),
    
    percent_reported_black = first(percent_reported_black), # speaker-level (0–100)
    true_gender = first(true_gender),                       # speaker gender
    MCA1 = first(MCA1),                                     # participant-level
    MCA2 = first(MCA2),
    
    .groups = "drop"
  )

# --------------------------------------------------
# MODEL FUNCTION
# --------------------------------------------------
fit_personality_model <- function(personality_col) {
  
  message("Fitting model for: ", personality_col)
  
  # 1. Select needed columns
  data_model <- personality_df %>%
    select(
      prolific_id, speaker,
      percent_reported_black, true_gender,
      MCA1, MCA2, all_of(personality_col)
    ) %>%
    drop_na()  # Drop rows missing MCA or ratings
  
  # 2. Build formula
  model_formula <- as.formula(
    paste0(
      personality_col,
      " ~ percent_reported_black * true_gender + MCA1 + MCA2 + ",
      "(1 | prolific_id) + (1 | speaker)"
    )
  )
  
  # 3. Fit model
  model <- lmer(model_formula, data = data_model)
  
  # 4. Get p-values + CIs via lmerTest + broom.mixed
  tidy_fixed <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
  
  # 5. Return a list with all the useful stuff
  list(
    summary = summary(model),     # <--- just like lmer output
    tidy = tidy_fixed,            # <--- table w/ p-values + CI
    model = model,                # model object
    formula = model_formula,
    n_obs = nrow(data_model),
    n_participants = length(unique(data_model$prolific_id)),
    n_speakers = length(unique(data_model$speaker))
  )
}

# Call models:
result_competent       <- fit_personality_model("competent")
result_funny           <- fit_personality_model("funny")
result_trust           <- fit_personality_model("trustworthy")
result_professional    <- fit_personality_model("professional")
results_friendly       <- fit_personality_model("friendly")
result_pleasant        <- fit_personality_model("pleasant")
result_human_likeness  <- fit_personality_model("human_like")
