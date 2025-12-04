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


# Load data
df <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/survey_results/survey_results.csv")

# Standard error function
se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

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
  
  scale_fill_manual(values = c(
    "BF" = "#c23b23",
    "BM" = "#6f42c1",
    "WF" = "#2b7de9",
    "WM" = "#28a745"
  )) +
  scale_color_manual(values = c(
    "BF" = "#c23b23",
    "BM" = "#6f42c1",
    "WF" = "#2b7de9",
    "WM" = "#28a745"
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
     xlab = "Number of clusters K",
     ylab = "Total within-cluster sum of squares")


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
df_density <- df %>%
  distinct(speaker, percent_reported_black, ground_truth_label) %>%
  filter(!is.na(percent_reported_black),
         !is.na(ground_truth_label))

ggplot(df_density,
       aes(x = percent_reported_black,
           fill = ground_truth_label,
           color = ground_truth_label)) +
  
  geom_density(alpha = 0.3, linewidth = 1.2) +
  
  scale_fill_manual(values = c(
    "Black"      = "#c23b23",
    "White"      = "#2b7de9",
    "Ambiguous"  = "gray40"
  )) +
  scale_color_manual(values = c(
    "Black"      = "#c23b23",
    "White"      = "#2b7de9",
    "Ambiguous"  = "gray40"
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
# Cluster Assignment Plot (ordered)
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


# -------------------------------
# Personality analysis
df_filtered <- df %>%
  filter(ground_truth_label != "Ambiguous") 


# plotting function 
plot_personality <- function(data, trait_col) {
  
  trait_col <- rlang::ensym(trait_col)   # allow unquoted column names
  
  ggplot(data,
         aes(x = ground_truth_label,
             y = !!trait_col,
             fill = ground_truth_label,
             color = ground_truth_label)) +
    
    geom_violin(alpha = 0.5, trim = FALSE) +
    geom_jitter(width = 0.15, size = 1.7, alpha = 0.7) +
    
    scale_fill_manual(values = c(
      "Black"      = "#c23b23",
      "White"      = "#2b7de9",
      "Ambiguous"  = "gray40"
    )) +
    scale_color_manual(values = c(
      "Black"      = "#c23b23",
      "White"      = "#2b7de9",
      "Ambiguous"  = "gray40"
    )) +
    
    labs(
      x = "Ground Truth Label",
      y = paste0(rlang::as_string(trait_col), " (Likert 1–7)"),
      fill = "Ground Truth Label",
      color = "Ground Truth Label"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
}

plot_personality(df_filtered, friendly)

plot_personality(df_filtered, trustworthy)

plot_personality(df_filtered, competent)

plot_personality(df_filtered, professional)

plot_personality(df_filtered, funny)

plot_personality(df_filtered, pleasant)

plot_personality(df_filtered, human_like)




# --------------------------------------------------
# BUILD MODELING DATASET FOR PERSONALITY
# --------------------------------------------------
# 2. Add true speaker gender
df_filtered <- df_filtered %>%
  mutate(
    true_gender = if_else(str_sub(speaker, 2, 2) == "F", "Female", "Male")
  )

speaker_gender_scale <- df_filtered %>%
  group_by(speaker) %>%
  summarise(
    mean_gender_scale = mean(gender_scale, na.rm = TRUE),
    .groups = "drop"
  )

df_filtered <- df_filtered %>%
  left_join(speaker_gender_scale, by = "speaker")


# 3. Build personality dataset (summarized per participant × speaker)
personality_df <- df_filtered %>%
  filter(block == "Personality") %>%
  mutate(
    ground_truth_label = factor(ground_truth_label, levels = c("White", "Black"))
  ) %>%
  select(
    prolific_id, speaker,
    competent, trustworthy, friendly, funny,
    professional, pleasant, human_like,
    ground_truth_label, mean_gender_scale
  )

# model function 
fit_personality_model_clean <- function(personality_col) {
  
  message("Fitting model for: ", personality_col)
  
  # Build modeling dataset
  data_model <- personality_df %>%
    select(
      prolific_id, speaker,
      ground_truth_label, mean_gender_scale,
      human_like,
      all_of(personality_col)
    ) %>%
    drop_na()
  
  # Model formula using ground truth label
  model_formula <- as.formula(
    paste0(
      personality_col,
      " ~ ground_truth_label * mean_gender_scale + 
         human_like +
         (1 | prolific_id) + (1 | speaker)"
    )
  )
  
  # Fit mixed-effects model
  model <- lmer(model_formula, data = data_model)
  
  # Confidence intervals
  ci <- confint(model, method = "Wald", level = 0.95)
  ci_df <- as.data.frame(ci)
  ci_df$term <- rownames(ci_df)
  
  # Fixed effects with p-values
  fixed <- summary(model)$coef
  fixed_df <- as.data.frame(fixed)
  fixed_df$term <- rownames(fixed_df)
  
  # Merge CI
  fixed_with_ci <- fixed_df %>%
    left_join(ci_df, by = "term") %>%
    select(
      term,
      Estimate,
      `Std. Error`,
      df,
      `t value`,
      `Pr(>|t|)`,
      `2.5 %`,
      `97.5 %`
    )
  
  list(
    summary = summary(model),        
    fixed_effects = fixed_with_ci    
  )
}


# run models 
result_competent     <- fit_personality_model_clean("competent")
result_funny         <- fit_personality_model_clean("funny")
result_trust         <- fit_personality_model_clean("trustworthy")
result_professional  <- fit_personality_model_clean("professional")
results_friendly     <- fit_personality_model_clean("friendly")
results_pleasant <- fit_personality_model_clean("pleasant")


# human likeness ratings 
#create df with averages for personality ratings for plotting 
speaker_personality <- df_filtered %>%
  group_by(speaker) %>%
  summarise(
    competent    = mean(competent, na.rm = TRUE),
    trustworthy  = mean(trustworthy, na.rm = TRUE),
    friendly     = mean(friendly, na.rm = TRUE),
    funny        = mean(funny, na.rm = TRUE),
    professional = mean(professional, na.rm = TRUE),
    pleasant     = mean(pleasant, na.rm = TRUE),
    human_like   = mean(human_like, na.rm = TRUE),
    
    percent_reported_black = first(percent_reported_black),
    true_gender = first(true_gender),
    ground_truth_label = first(ground_truth_label),  # <-- important
    .groups = "drop"
  )


plot_human_likeness <- function(data, trait_col) {
  
  trait_sym  <- rlang::ensym(trait_col)
  trait_name <- rlang::as_string(trait_sym)
  
  ggplot(data,
         aes(x = human_like,
             y = !!trait_sym,
             color = ground_truth_label)) +
    
    # POINTS COLORED BY GROUP
    geom_point(alpha = 0.6, size = 2.2) +
    
    # ONE SINGLE REGRESSION LINE (NO GROUPING)
    geom_smooth(
      data = data,
      aes(x = human_like, y = !!trait_sym),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      color = "black",     # one line, one color
      se = TRUE,
      linewidth = 1.2
    ) +
    
    scale_color_manual(values = c(
      "White" = "#2b7de9",
      "Black" = "#c23b23"
    )) +
    
    labs(
      x = "Human-Likeness Rating",
      y = paste0(str_to_title(trait_name), " Rating"),
      color = "Ground Truth Label",
      title = paste0(str_to_title(trait_name),
                     " ~ Human-Likeness (single regression line)")
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.line  = element_line(color = "black"),
      axis.text  = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
}


plot_human_likeness(speaker_personality, competent)
plot_human_likeness(speaker_personality, trustworthy)
plot_human_likeness(speaker_personality, friendly)
plot_human_likeness(speaker_personality, funny)
plot_human_likeness(speaker_personality, professional)
plot_human_likeness(speaker_personality, pleasant)


# save df for acoustic analysis 
df_save <- df %>%
  group_by(speaker) %>%
  summarise(
    ground_truth_label = first(ground_truth_label),   # or most frequent, if needed
    mean_gender_scale  = mean(gender_scale, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct(speaker, ground_truth_label, mean_gender_scale)

write_csv(
  df_save,
  "/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/formants/ground_truth_label.csv"
)


