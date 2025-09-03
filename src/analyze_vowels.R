library(tidyverse)
library(mgcv)
library(itsadug)

## --- REPLACE your current data-generation section with this ---

set.seed(123)

# 32 speakers: 8 BF / 8 BM / 8 WF / 8 WM (as you already have)
speakers <- c(paste0('BF_', 1:8),
              paste0('BM_', 1:8),
              paste0('WF_', 1:8),
              paste0('WM_', 1:8))

# 3 tokens per vowel
vowel_word_list <- list(
  'i'  = c('beat','heat','seat'),
  'eɪ' = c('bait','hate','fate'),
  'ɪ'  = c('bit','hit','sit'),
  'ɛ'  = c('bet','set','pet'),
  'æ'  = c('bat','hat','sat'),
  'u'  = c('boot','hoot','suit'),
  'ɑ'  = c('cot','hot','pot')
)

# Base grid
base_grid <- expand.grid(
  speaker = speakers,
  vowel = names(vowel_word_list),
  word_num = 1:3,
  normalized_time = seq(0, 100, length.out = 10)
) %>%
  mutate(
    word = purrr::map2_chr(vowel, word_num, ~ vowel_word_list[[.x]][.y]),
    race = ifelse(grepl('^B', speaker), 'Black', 'White'),
    sex  = ifelse(grepl('F_', speaker), 'female', 'male'),
    t    = normalized_time / 100
  )

# --- NEW: speaker-level random curves (intercept + slope + mild curvature) ---
# Tunable SDs (Bark units): keep modest to preserve convergence
re_speaker <- tibble::tibble(
  speaker = factor(speakers),
  u0_F1 = rnorm(length(speakers), sd = 0.30),  # intercept
  u1_F1 = rnorm(length(speakers), sd = 0.20),  # linear time slope
  u2_F1 = rnorm(length(speakers), sd = 0.10),  # quadratic time curvature
  u0_F2 = rnorm(length(speakers), sd = 0.60),
  u1_F2 = rnorm(length(speakers), sd = 0.40),
  u2_F2 = rnorm(length(speakers), sd = 0.20)
)

data <- base_grid %>%
  dplyr::left_join(re_speaker, by = "speaker") %>%
  mutate(
    # Base means by vowel (Bark) + group offsets
    F1_base = c('i'=3.5,'eɪ'=4.5,'ɪ'=4.7,'ɛ'=5.2,'æ'=6.5,'u'=3.7,'ɑ'=6.0)[vowel] +
      ifelse(sex == 'male', 0.3, 0),
    F2_base = c('i'=13.5,'eɪ'=12.5,'ɪ'=12.8,'ɛ'=12.0,'æ'=11.5,'u'=7.0,'ɑ'=8.5)[vowel] +
      ifelse(race == 'Black', -0.2, 0.2),
    
    # Deterministic time trends + speaker-specific random curve
    mu_F1 = F1_base + 0.5 * t + (u0_F1 + u1_F1 * t + u2_F1 * t^2),
    mu_F2 = F2_base - 0.5 * t + (u0_F2 + u1_F2 * t + u2_F2 * t^2),
    
    # Observation noise
    F1_bark = rnorm(n(), mean = mu_F1, sd = 0.20),
    F2_bark = rnorm(n(), mean = mu_F2, sd = 0.40)
  ) %>%
  select(-starts_with("u"), -mu_F1, -mu_F2) %>%
  mutate(
    across(c(speaker, race, sex, vowel, word), factor),
    race_sex = interaction(race, sex, drop = TRUE)
  )


# --- Fit per-vowel models ---
for(v in unique(data$vowel)){
  
  cat("\n\n==================================\n")
  cat("Now analyzing vowel:", v, "\n")
  cat("==================================\n\n")
  
  vowel_data <- filter(data, vowel == v)
  
  model_F1 <- bam(
    F1_bark ~ race * sex +
      s(normalized_time, by = race_sex, k = 4) +
      s(normalized_time, speaker, bs = "fs", k = 5, m = 1),  # smaller k for fs
    data = vowel_data,
    method = "fREML",
    select = TRUE,
    control = gam.control(trace = TRUE)
  )
  
  model_F2 <- bam(
    F2_bark ~ race * sex +
      s(normalized_time, by = race_sex, k = 4) +
      s(normalized_time, speaker, bs = "fs", k = 5, m = 1),
    data = vowel_data,
    method = "fREML",
    select = TRUE,
    control = gam.control(trace = TRUE)
  )
  
  cat("\n--- Updated F1 Model Summary for Vowel", v, "---\n")
  print(summary(model_F1))
  cat("\n--- Updated F2 Model Summary for Vowel", v, "---\n")
  print(summary(model_F2))
  
  # Quick convergence readout (outer optimizer)
  conv_flag <- function(m) if (!is.null(m$outer.info$conv)) m$outer.info$conv else NA
  cat("\nConvergence (outer): F1 =", conv_flag(model_F1), " | F2 =", conv_flag(model_F2), "\n")
  
  # Predictions excluding random effects; include a placeholder speaker to satisfy model frame
  new_data <- expand.grid(
    normalized_time = seq(0, 100, length.out = 20),
    race  = factor(c("Black","White"), levels = levels(vowel_data$race)),
    sex   = factor(c("female","male"), levels = levels(vowel_data$sex)),
    speaker = factor(vowel_data$speaker[1], levels = levels(vowel_data$speaker))
  ) %>% mutate(race_sex = interaction(race, sex, drop = TRUE))
  
  new_data <- new_data %>%
    mutate(
      F1_pred = predict(model_F1, newdata = ., exclude = c("s(normalized_time,speaker)"), newdata.guaranteed = TRUE),
      F2_pred = predict(model_F2, newdata = ., exclude = c("s(normalized_time,speaker)"), newdata.guaranteed = TRUE)
    )
  
  plot <- ggplot(new_data, aes(x = F2_pred, y = F1_pred, color = sex, linetype = race, group = interaction(race,sex))) +
    geom_path(arrow = arrow(angle = 20, length = unit(0.1, "inches"), type = "closed"), linewidth = 1) +
    scale_x_reverse(name = "F2 (Bark)") +
    scale_y_reverse(name = "F1 (Bark)") +
    theme_classic() +
    ggtitle(paste("Formant trajectory (Race × Sex) for vowel /", v, "/", sep = "")) +
    theme(legend.position = "right")
  
  print(plot)

}
