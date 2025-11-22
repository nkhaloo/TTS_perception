library(tidyverse)
library(factoextra)
library(ggridges)
library(patchwork)

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
km <- kmeans(scaled_features, centers = 3, nstart = 50)

speaker_clusters <- speaker_features %>%
  mutate(cluster = factor(km$cluster))

# map clusters → labels
cluster_labels <- speaker_clusters %>%
  mutate(
    ground_truth_label = case_when(
      cluster == 1 ~ "White",
      cluster == 2 ~ "Ambiguous",
      cluster == 3 ~ "Black"
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



