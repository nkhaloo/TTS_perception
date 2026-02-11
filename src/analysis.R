library(tidyverse)
library(factoextra)
library(ggridges)
library(patchwork)
library(FactoMineR)
library(factoextra)
library(caret)
library(CAvariants)
library(lme4)
library(broom.mixed)
library(lmerTest)
library(cluster)
library(forcats)


# Load data
df <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/survey_results/survey_results.csv")

# Standard error function
se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))


# clean df to keep only participants who completetd the study 
required_blocks <- c("Personality", "Race", "Demographics")

complete_pids <- df %>%
  filter(block %in% required_blocks) %>%
  distinct(prolific_id, block) %>%
  count(prolific_id) %>%
  filter(n == length(required_blocks)) %>%
  pull(prolific_id)

df <- df %>%
  filter(prolific_id %in% complete_pids)

df %>% 
  summarise(n_unique_prolific_ids = n_distinct(prolific_id))

# -------------------------------
# RACE AGREEMENT
# -------------------------------
agreement_by_category_and_speaker <- df %>%
  filter(!is.na(speaker), block == "Race") %>%
  mutate(
    speaker_category = str_sub(speaker, 1, 2),
    true_race = case_when(
      str_starts(speaker, "B") ~ "Black",
      str_starts(speaker, "W") ~ "White"
    ),
    agreement = perceived_race == true_race
  ) %>%
  group_by(speaker_category, speaker) %>%
  summarise(
    percent_agreement = mean(agreement, na.rm = TRUE) * 100,
    se = se(agreement) * 100,
    .groups = "drop"
  )

agreement_by_category <- agreement_by_category_and_speaker %>%
  group_by(speaker_category) %>%
  summarise(
    category_percent_agreement = mean(percent_agreement),
    se = se(percent_agreement),
    .groups = "drop"
  )

# Plot 1
ggplot(agreement_by_category,
       aes(x = speaker_category,
           y = category_percent_agreement,
           fill = speaker_category)) +
  
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = category_percent_agreement - se,
                    ymax = category_percent_agreement + se),
                width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", category_percent_agreement),
                y = category_percent_agreement + se + 3),
            vjust = 0, size = 4.5) +
  
  scale_fill_manual(values = c("BF" = "#c23b23",
                               "BM" = "#6f42c1",
                               "WF" = "#2b7de9",
                               "WM" = "#28a745")) +
  
  labs(x = "Speaker Category", y = "Agreement with label") +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  coord_cartesian(ylim = c(0, 100)) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )



# -------------------------------
# GENDER AGREEMENT
# -------------------------------

gender_agreement_race_gender <- df %>%
  filter(!is.na(gender_scale), !is.na(speaker)) %>%
  mutate(
    true_gender = if_else(str_sub(speaker, 2, 2) == "F", "Female", "Male"),
    true_race = if_else(str_sub(speaker, 1, 1) == "B", "Black", "White"),
    perceived_gender = if_else(gender_scale >= 4, "Male", "Female"),
    agree = perceived_gender == true_gender
  ) %>%
  group_by(true_race, true_gender) %>%
  summarise(
    percent_agreement = mean(agree) * 100,
    sd_agreement = sd(as.numeric(agree)) * 100,
    n = n(),
    .groups = "drop"
  )


gender_agreement_trials <- df %>%
  filter(!is.na(gender_scale), !is.na(speaker)) %>%
  mutate(
    true_gender = if_else(str_sub(speaker, 2, 2) == "F", "Female", "Male"),
    true_race = if_else(str_sub(speaker, 1, 1) == "B", "Black", "White"),
    perceived_gender = if_else(gender_scale >= 3.5, "Male", "Female"),
    agree = perceived_gender == true_gender
  )


gender_agreement_by_race <- gender_agreement_trials %>%
  group_by(true_race, true_gender) %>%
  summarise(
    percent_agreement = mean(agree) * 100,
    sd_agreement = sd(as.numeric(agree)) * 100,
    n = n(),
    .groups = "drop"
  )

gender_agreement_total <- gender_agreement_trials %>%
  mutate(true_race = "Total") %>%
  group_by(true_race, true_gender) %>%
  summarise(
    percent_agreement = mean(agree) * 100,
    sd_agreement = sd(as.numeric(agree)) * 100,
    n = n(),
    .groups = "drop"
  )


# -------------------------------
# Add %Reported Black
# -------------------------------

df <- df %>%
  group_by(speaker) %>%
  mutate(percent_reported_black =
           mean(perceived_race == "Black", na.rm = TRUE) * 100) %>%
  ungroup()



# -------------------------------
# Density plot of %Reported Black
# -------------------------------
df %>% 
  distinct(speaker, percent_reported_black) %>%
  filter(!is.na(percent_reported_black)) %>%
  mutate(category = substr(speaker, 1, 2)) %>%
  ggplot(aes(x = percent_reported_black,
             fill = category,
             color = category)) +
  
  geom_density(alpha = 0.3, linewidth = 1.2) +
  
  scale_fill_manual(
    name = "Platform-Assigned Label",
    values = c(
      "BF" = "#c23b23",
      "BM" = "#6f42c1",
      "WF" = "#2b7de9",
      "WM" = "#28a745"
    ),
    labels = c(
      "BF" = "Black Female",
      "BM" = "Black Male",
      "WF" = "White Female",
      "WM" = "White Male"
    )
  ) +
  
  scale_color_manual(
    name = "Platform-Assigned Label",
    values = c(
      "BF" = "#c23b23",
      "BM" = "#6f42c1",
      "WF" = "#2b7de9",
      "WM" = "#28a745"
    ),
    labels = c(
      "BF" = "Black Female",
      "BM" = "Black Male",
      "WF" = "White Female",
      "WM" = "White Male"
    )
  ) +
  
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  labs(x = "% Reported Black", y = "Density") +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )




# -------------------------------
# K-MEANS CLUSTERING
# -------------------------------
speaker_features <- df %>%
  group_by(speaker) %>%
  summarise(
    percent_reported_black = mean(perceived_race == "Black", na.rm = TRUE) * 100,
    mean_gender_scale = mean(gender_scale, na.rm = TRUE)
  ) %>%
  drop_na()

scaled_features <- speaker_features %>%
  select(percent_reported_black) %>%
  scale()

set.seed(123)

wss <- sapply(1:10, function(k) {
  kmeans(scaled_features, centers = k, nstart = 50)$tot.withinss
})

plot(1:10, wss, type = "b",
     pch = 19, frame = FALSE,
     xlab = "Number of clusters (K)",
     ylab = "Within-cluster sum of squares")


sil_width <- sapply(2:6, function(k) {
  km <- kmeans(scaled_features, centers = k, nstart = 50)
  ss <- silhouette(km$cluster, dist(scaled_features))
  mean(ss[, 3])
})

plot(2:6, sil_width, type = "b",
     pch = 19,
     xlab = "Clusters",
     ylab = "Average silhouette width")


km <- kmeans(scaled_features, centers = 3, nstart = 50)

speaker_clusters <- speaker_features %>%
  mutate(cluster = factor(km$cluster))

# map clusters → labels
cluster_labels <- speaker_clusters %>%
  mutate(
    ground_truth_label = case_when(
      cluster == 1 ~ "White",
      cluster == 2 ~ "Black",
      cluster == 3 ~ "Ambiguous"
    )
  ) %>%
  select(speaker, ground_truth_label)

# merge back
df <- df %>% left_join(cluster_labels, by = "speaker")


# -------------------------------
# Density plot with ground truth label
# -------------------------------
ggplot(speaker_clusters,
       aes(x = percent_reported_black,
           fill = cluster,
           color = cluster)) +
  
  geom_density(alpha = 0.3, linewidth = 1.2) +
  
  scale_fill_manual(values = c(
    "2"      = "#c23b23",
    "1"      = "#2b7de9",
    "3"  = "gray40"
  )) +
  scale_color_manual(values = c(
    "2"      = "#c23b23",
    "1"      = "#2b7de9",
    "3"  = "gray40"
  )) +
  
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  labs(x = "% Reported Black", y = "Density") +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )



# -------------------------------
# Cluster Assignment Plot 
# -------------------------------
df_plot <- df %>%
  distinct(speaker, percent_reported_black, ground_truth_label) %>%
  filter(!is.na(ground_truth_label)) %>%
  mutate(
    ground_truth_label = factor(
      ground_truth_label,
      levels = c("White", "Ambiguous", "Black")
    )
  )

ggplot(df_plot,
       aes(x = ground_truth_label,
           y = percent_reported_black,
           color = ground_truth_label)) +
  
  geom_jitter(width = 0.15, size = 4, alpha = 0.9) +
  
  scale_color_manual(values = c(
    "Black"      = "#c23b23",
    "White"      = "#2b7de9",
    "Ambiguous"  = "gray40"
  )) +
  
  labs(
    x = "Ground Truth Label",
    y = "% Reported Black",
    color = "Ground Truth Label"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid = element_blank()
  )


speaker_clusters2 <- speaker_features %>%
  left_join(speaker_clusters %>% select(speaker, cluster),
            by = "speaker") %>%
  mutate(
    category = substr(speaker, 1, 2),
    cluster = factor(cluster)
  ) %>%
  mutate(category = case_when(
    category == "BF" ~ "Black Female",
    category == "BM" ~ "Black Male",
    category == "WF" ~ "White Female",
    category == "WM" ~ "White Male")) %>%
  mutate(perceptual_category = case_when(
    cluster == 1 ~ "White", 
    cluster == 2 ~ "Black", 
    cluster == 3 ~ "Ambiguous"
  ))


ggplot(speaker_clusters2,
       aes(x = percent_reported_black,
           y = fct_reorder(speaker, percent_reported_black),
           color = cluster)) +
  
  geom_point(size = 5, alpha = 0.9) +
  
  scale_color_manual(values = c(
    "1" = "#2b7de9",  # White cluster
    "2" = "#c23b23",  # Black cluster
    "3" = "gray40"    # Ambiguous cluster
  )) +
  
  facet_wrap(~ category, scales = "free_y") +
  
  labs(
    x = "% Reported Black",
    y = "TTS voice",
    color = "Cluster"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    strip.text = element_text(face = "bold", size = 14)
  )


# -------------------------------
# Create true gender
# -------------------------------
df <- df %>%
  mutate(
    true_gender = case_when(
      !is.na(speaker) & str_sub(speaker, 2, 2) == "F" ~ "Female",
      !is.na(speaker) & str_sub(speaker, 2, 2) == "M" ~ "Male",
      TRUE ~ NA_character_   # demographic rows
    )
  )


# -------------------------------
# Individual speaker agreement 
# -------------------------------
listener_info <- df %>%
  filter(block == "Demographics") %>%
  select(prolific_id, ethnicity) %>%
  distinct()

df <- df %>%
  left_join(listener_info, by = "prolific_id")

df_race <- df %>%
  filter(block == "Race") %>%
  select(prolific_id, speaker, perceived_race)


df_race <- df_race %>%
  left_join(listener_info, by = "prolific_id")


df_race <- df_race %>%
  filter(!is.na(ethnicity)) %>%    
  mutate(
    ethnicity_group = case_when(
      ethnicity == "Asian American" ~ "Asian American",
      ethnicity == "Black American" ~ "Black American",
      ethnicity == "White American" ~ "White American",
      ethnicity == "Hispanic/Latino American" ~ "Hispanic/Latino American",
      str_detect(ethnicity, ";") & str_detect(ethnicity, "Black") ~ 
        "Multiracial (Black included)",
      str_detect(ethnicity, ";") ~ 
        "Multiracial (Black not included)",
      
      TRUE ~ NA_character_
    )
  )



ethnicity_percentages <- df_race %>%
  filter(!is.na(ethnicity_group)) %>%
  distinct(prolific_id, ethnicity_group) %>%   # one row per participant
  count(ethnicity_group, name = "n_participants") %>%
  mutate(
    percent = 100 * n_participants / sum(n_participants)
  )


df_race <- df_race %>%
  mutate(
    true_race = if_else(str_starts(speaker, "B"), "Black", "White"),
    agreement = as.numeric(perceived_race == true_race)
  )


agreement_by_ethnicity <- df_race %>%
  filter(!is.na(ethnicity_group)) %>%      
  group_by(ethnicity_group) %>%
  summarise(
    percent_agreement = mean(agreement, na.rm = TRUE) * 100,
    se = se(agreement) * 100,
    n = n(),
    .groups = "drop"
  )


ggplot(agreement_by_ethnicity,
       aes(x = ethnicity_group,
           y = percent_agreement,
           fill = ethnicity_group)) +
  
  geom_col(width = 0.6, color = "black") +
  
  geom_errorbar(aes(ymin = percent_agreement - se,
                    ymax = percent_agreement + se),
                width = 0.15, linewidth = 0.8) +
  
  geom_text(aes(label = sprintf("%.1f%%", percent_agreement),
                y = percent_agreement + se + 3),
            vjust = 0, size = 4.5) +
  
  labs(
    x = "Listener Ethnicity Group",
    y = "Agreement with Speaker Race Label (%)"
  ) +
  
  # MAKE BARS TOUCH X-AXIS (same trick as earlier plot)
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  coord_cartesian(ylim = c(0, 100)) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 25, hjust = 1, color = "black"),
    axis.title = element_text(color = "black")
  )


df_race$ethnicity_group <- as.factor(df_race$ethnicity_group)
df_race$ethnicity_group <- relevel(df_race$ethnicity_group, ref = "White American")


model_ethnicity <- glmer(
  agreement ~ ethnicity_group + (1 | prolific_id),
  data = df_race,
  family = binomial
)


summary(model_ethnicity)




# -------------------------------
# Personality analysis
# -------------------------------
personality_cols <- c("competent", "trustworthy", "friendly",
                      "funny", "professional", "pleasant")


speaker_personality <- df %>%
  filter(block == "Personality") %>%          # ensure correct subset
  group_by(speaker) %>%
  summarise(
    
    # Means for each personality variable
    across(
      all_of(personality_cols),
      ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    ),
    
    # Standard errors for each trait (computed only on non-NA values)
    across(
      all_of(personality_cols),
      ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
      .names = "se_{.col}"
    ),
    
    # Optional extras from your dataset
    percent_reported_black = first(percent_reported_black),
    true_gender = first(true_gender),
    ground_truth_label = first(ground_truth_label),
    
    .groups = "drop"
  )


speaker_long <- speaker_personality %>%
  pivot_longer(
    cols = matches("^(mean|se)_"),
    names_to = c(".value", "trait"),
    names_pattern = "(mean|se)_(.*)"
  )

speaker_long$ground_truth_label <- factor(
  speaker_long$ground_truth_label,
  levels = c("Black", "White", "Ambiguous")
)


plot_personality_by_gender <- function(data, gender_choice) {
  
  df_gender <- data %>%
    filter(true_gender == gender_choice)
  
  df_summary <- df_gender %>%
    group_by(trait, ground_truth_label) %>%
    summarise(
      mean = mean(mean, na.rm = TRUE),
      se   = mean(se, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(df_summary, aes(
    x = ground_truth_label,
    y = mean,
    fill = ground_truth_label
  )) +
    geom_col(color = "black") +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.25,
      linewidth = 0.8
    ) +
    facet_wrap(~ trait, scales = "free") +
    
    scale_fill_manual(
      name = "Perceptually-assigned Race",
      values = c(
        "White"     = "#2b7de9",
        "Black"     = "#c23b23",
        "Ambiguous" = "gray40"
      )
    ) +
    
    labs(
      x = NULL,
      y = "Mean Rating"
    ) +
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) +
    
    theme_bw() +
    theme(
      legend.position = "right",
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text   = element_text(size = 12, face = "bold")
    )
}


plot_personality_by_gender(speaker_long, "Female")

plot_personality_by_gender(speaker_long, "Male")



# human like plot 
human_like <- ("human_like")
hl_df <- df %>%
  filter(block == "Personality") %>%        
  group_by(speaker) %>%
  summarise(
    across(
      all_of(human_like),
      ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    ),
    across(
      all_of(human_like),
      ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
      .names = "se_{.col}"
    ),
    percent_reported_black = first(percent_reported_black),
    true_gender = first(true_gender),
    ground_truth_label = first(ground_truth_label),
    
    .groups = "drop"
  )

hl_df$ground_truth_label <- factor(hl_df$ground_truth_label,
  levels = c("Black", "White", "Ambiguous")
)

plot_human_like <- function(data, gender_choice) {
  
  df_gender <- data %>%
    filter(true_gender == gender_choice)
  
  # Summarize human-like by ground_truth_label
  df_summary <- df_gender %>%
    group_by(ground_truth_label) %>%
    summarise(
      mean = mean(mean_human_like, na.rm = TRUE),
      se   = se(mean_human_like),
      .groups = "drop"
    )
  
  ggplot(df_summary, aes(
    x = ground_truth_label,
    y = mean,
    fill = ground_truth_label
  )) +
    geom_col(color = "black") +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.25,
      linewidth = 0.8
    ) +
    scale_fill_manual(values = c(
      "White"     = "#2b7de9",
      "Black"     = "#c23b23",
      "Ambiguous" = "gray40"
    )) +
    labs(
      #title = paste("Human-Likeness Ratings for", gender_choice),
      x = "Ground Truth Label",
      y = "Human-Likeness Rating"
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 12, face = "bold")
    )
}

plot_human_like(hl_df, "Female")
plot_human_like(hl_df, "Male")


hl_df_mod_male <- df %>%
  filter(block == "Personality") %>%
  filter(true_gender == "Male") 

hl_df_mod_male$ground_truth_label <- factor(hl_df_mod_male$ground_truth_label,
                                   levels = c("White", "Black", "Ambiguous"))

lm_hl_male <- lmer(
  human_like ~ ground_truth_label + 
    (1 | speaker) + (1 | prolific_id),
  data = hl_df_mod_male
)

summary(lm_hl_male)


hl_df_mod_female <- df %>%
  filter(block == "Personality") %>%
  filter(true_gender == "Female")

hl_df_mod_female$ground_truth_label <- factor(hl_df_mod_female$ground_truth_label,
                                            levels = c("White", "Black", "Ambiguous"))


lm_hl_female <- lmer(
  human_like ~ ground_truth_label + 
    (1|speaker) + (1|prolific_id),
  data = hl_df_mod_female
)

summary(lm_hl_female)

# --------------------------------------------------
# BUILD MODELING DATASET FOR PERSONALITY
# --------------------------------------------------
personality_df <- df %>%
  filter(block == "Personality") %>%
  mutate(
    # Ground-truth race labels for voices
    ground_truth_label = factor(
      ground_truth_label,
      levels = c("White", "Black", "Ambiguous")
    ),
    
    # Simplified participant ethnicity
    ethnicity_group = case_when(
      ethnicity.y == "White American" ~ "White American",
      ethnicity.y == "Black American" ~ "Black American",
      ethnicity.y == "Asian American" ~ "Asian American",
      ethnicity.y == "Hispanic/Latino American" ~ "Hispanic/Latino American",
      
      str_detect(ethnicity.y, ";") & str_detect(ethnicity.y, "Black") ~ 
        "Multiracial (Black included)",
      
      str_detect(ethnicity.y, ";") ~ 
        "Multiracial (Black not included)",
      
      TRUE ~ NA_character_
    ),
    
    ethnicity_group = factor(
      ethnicity_group,
      levels = c(
        "White American",
        "Black American",
        "Asian American",
        "Hispanic/Latino American",
        "Multiracial (Black included)",
        "Multiracial (Black not included)"
      )
    ),
    
    # Speaker gender and centered human-likeness
    true_gender   = factor(true_gender),
    human_like_c  = scale(human_like, center = TRUE, scale = FALSE)
  ) %>%
  select(
    prolific_id, speaker,
    competent, trustworthy, friendly, funny,
    professional, pleasant,
    human_like_c,
    ground_truth_label, true_gender, ethnicity_group
  ) %>%
  filter(!is.na(ethnicity_group))


run_personality_models <- function(data, gender_choice) {
  
  traits <- c(
    "competent", "trustworthy", "friendly",
    "funny", "professional", "pleasant"
  )
  
  df_gender <- data %>%
    filter(true_gender == gender_choice)
  
  model_results <- list()
  
  for (trait in traits) {
    
    form <- as.formula(
      paste0(
        trait,
        " ~ ground_truth_label + human_like_c + ethnicity_group + ",
        "(1 | prolific_id) + (1 | speaker)"
      )
    )
    
    mod <- lmer(
      formula = form,
      data = df_gender,
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa")
    )
    
    model_results[[trait]] <- summary(mod)
  }
  
  return(model_results)
}

female_models <- run_personality_models(personality_df, "Female")
male_models   <- run_personality_models(personality_df, "Male")


# save df for acoustic analysis 
df_save <- df %>%
  group_by(speaker) %>%
  summarise(
    ground_truth_label = first(ground_truth_label),   
    mean_gender_scale  = mean(gender_scale, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct(speaker, ground_truth_label, mean_gender_scale)

write_csv(
  df_save,
  "/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/formants/ground_truth_label.csv"
)


