library(tidyverse)
library(lme4)
library(lmerTest)
library(xgboost)
library(purrr)

# set seed
set.seed(555)

# load ground truth label from perception 
ground_truth <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/ground_truth_label.csv") %>%
  select(speaker, ground_truth_label)

# load acoustic df 
df <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/output.csv") %>%
  mutate(speaker = substr(Filename, 1, 3)) %>%
  left_join(ground_truth, by = "speaker") %>%
  select(
    Filename, Label, seg_Start, seg_End,
    H1c, H2H4c, H42Kc, H2KH5Kc, CPP,
    sF0, sF1, sF2, sF3, sF4, SHR, Energy,
    speaker, ground_truth_label
  ) %>%
  mutate(
    modality = case_when(
      str_ends(Label, "_hc") ~ "breathy_creaky",
      str_ends(Label, "_c")  ~ "creaky",
      str_ends(Label, "_h")  ~ "breathy",
      TRUE                   ~ "modal"
    ),
    Label = str_remove(Label, "_(hc|c|h)$")
  ) %>%
  filter(str_detect(Filename, "central|mono|raising|fronting"))

# load vowel codes
vowel_codes <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/metadata/vowel_codes.csv") %>%
  rename(Label = "code") %>%
  mutate(Label = as.character(Label))

#pre-processing 
df <- df %>%
  left_join(vowel_codes, by = "Label") %>%
  mutate(
    gender = substr(Filename, 2, 2),
    gender = case_when(
      gender == "F" ~ "female",
      gender == "M" ~ "male",
      TRUE ~ NA_character_
    ),
    vowel = case_when(
      vowel %in% c("ɑh", "ah") ~ "ɑ",
      vowel == "ih"            ~ "ɪ",
      vowel == "ai"            ~ "æi",
      vowel %in% c("ɑ'", "a")  ~ "æ",
      TRUE ~ vowel
    )
  ) %>%
  drop_na(vowel) %>%
  filter(ground_truth_label != "Ambiguous")

acoustic_vars <- c(
  "H1c", "H2H4c", "H42Kc", "H2KH5Kc",
  "CPP", "Energy", "SHR",
  "sF0", "sF1", "sF2", "sF3", "sF4"
)

df <- df %>%
  filter(if_all(all_of(acoustic_vars), ~ is.finite(.) & . != 0))

#filter out outliers 
is_not_outlier_mad <- function(x, k = 3.5) {
  med <- median(x, na.rm = TRUE)
  mad_val <- mad(x, constant = 1, na.rm = TRUE)
  abs(x - med) < k * mad_val
}

df <- df %>%
  group_by(vowel) %>%
  mutate(across(all_of(acoustic_vars), ~ is_not_outlier_mad(.x), .names = "keep_{col}")) %>%
  ungroup() %>%
  mutate(is_outlier = if_any(starts_with("keep_"), ~ .x == FALSE))

df_clean <- df %>%
  filter(!is_outlier) %>%
  select(-starts_with("keep_"), -is_outlier)

# create residual h1
df_clean <- df_clean %>%
  mutate(Energy = pmax(Energy, .Machine$double.eps))

mod_h1 <- lmer(
  H1c ~ log(Energy) + (log(Energy) | speaker),
  data = df_clean
)

beta_energy <- fixef(mod_h1)["log(Energy)"]

df_clean <- df_clean %>%
  mutate(H1Res = H1c - beta_energy * log(Energy))

df_clean <- df_clean %>%
  mutate(DF = ((sF2 - sF1) + (sF3 - sF2) + (sF4 - sF3)) / 3)

# normalize variables by max/min 
norm_vars <- c("CPP","SHR","H1Res","H2H4c","H42Kc","H2KH5Kc","sF0","sF1","sF2","sF3","sF4","DF")

minmax_norm <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

df_clean <- df_clean %>%
  group_by(gender) %>%
  mutate(across(all_of(norm_vars), minmax_norm)) %>%
  ungroup()

analysis_vars <- c("CPP","SHR","H1Res","H2H4c","H42Kc","H2KH5Kc","sF0","sF1","sF2","sF3","sF4","DF")

# Delta formants
df_delta <- df_clean %>%
  group_by(Filename, Label, seg_Start, seg_End) %>%
  mutate(
    rel_pos = row_number() / n(),
    chunk = case_when(
      rel_pos <= 1/3 ~ "beg",
      rel_pos >  2/3 ~ "end",
      TRUE           ~ NA_character_
    )
  ) %>%
  filter(!is.na(chunk)) %>%
  group_by(Filename, Label, seg_Start, seg_End, chunk) %>%
  summarise(
    F1 = mean(sF1, na.rm = TRUE),
    F2 = mean(sF2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(Filename, Label, seg_Start, seg_End),
    names_from = chunk,
    values_from = c(F1, F2),
    names_glue = "{.value}_{chunk}"
  ) %>%
  mutate(
    dF1 = F1_end - F1_beg,
    dF2 = F2_end - F2_beg
  ) %>%
  select(Filename, Label, seg_Start, seg_End, dF1, dF2)

#cov generation function
cov_safe <- function(x) {
  m <- mean(x, na.rm = TRUE)
  if (is.na(m) || m == 0) return(NA_real_)
  sd(x, na.rm = TRUE) / m
}


# create df with vowel onset and offset for testing 
df_chunked_long <- df_clean %>%
  group_by(Filename, Label, seg_Start, seg_End) %>%
  mutate(
    rel_pos = row_number() / n(),
    chunk = case_when(
      rel_pos <= 1/3 ~ "beg",
      rel_pos <= 2/3 ~ "mid",
      TRUE           ~ "end"
    )
  ) %>%
  ungroup() %>%
  group_by(Filename, speaker, gender, ground_truth_label, vowel, Label, modality, seg_Start, seg_End, chunk) %>%
  summarise(
    across(
      all_of(analysis_vars),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        cov  = ~ cov_safe(.x)
      ),
      .names = "{col}_{fn}"
    ),
    .groups = "drop"
  )

df_chunked <- df_chunked_long %>%
  pivot_wider(
    id_cols = c(Filename, speaker, gender, ground_truth_label, modality, vowel, Label, seg_Start, seg_End),
    names_from = chunk,
    values_from = matches("_(mean|cov)$"),
    names_glue = "{.value}_{chunk}"
  ) %>%
  filter(gender == "male") %>%
  mutate(y = ifelse(ground_truth_label == "Black", 1, 0))

feature_cols_chunked <- df_chunked %>%
  select(matches("_(mean|cov)_(beg|mid|end)$")) %>%
  colnames()

df_chunked_male <- df_chunked %>%
  group_by(modality) %>%
  mutate(across(all_of(feature_cols_chunked), ~ .x - mean(.x, na.rm = TRUE))) %>%
  ungroup()

# create speaker-wise folds so models share the same splits
make_speaker_folds <- function(df, group_col = "speaker", nfold = 5, seed = 123) {
  set.seed(seed)
  speakers <- df %>% distinct(.data[[group_col]]) %>% pull(.data[[group_col]])
  k <- min(nfold, length(speakers))
  fold_id <- sample(rep(1:k, length.out = length(speakers)))
  tibble(!!group_col := speakers, fold = fold_id)
}


# create xgboost function that does speaker-wise cv
fit_xgb_groupcv_folds <- function(df, features, group_col = "speaker", nfold = 5, seed = 123) {
  set.seed(seed)
  
  if (!("fold" %in% names(df))) {
    stop("Data frame must already contain a 'fold' column.")
  }
  
  df2 <- df
  k <- length(unique(df2$fold))
  
  X <- as.matrix(df2[, features])
  y <- df2$y
  
  folds <- map(1:k, ~ which(df2$fold == .x))
  
  fold_acc <- map_dbl(1:k, function(f) {
    test_idx  <- folds[[f]]
    train_idx <- setdiff(seq_len(nrow(df2)), test_idx)
    
    dtrain <- xgb.DMatrix(X[train_idx, , drop = FALSE], label = y[train_idx])
    dtest  <- xgb.DMatrix(X[test_idx,  , drop = FALSE], label = y[test_idx])
    
    mod <- xgboost(
      data = dtrain,
      nrounds = 50,
      objective = "binary:logistic",
      eval_metric = "error",
      max_depth = 3,
      eta = 0.1,
      subsample = 0.8,
      colsample_bytree = 0.8,
      verbose = 0
    )
    
    p <- predict(mod, dtest)
    pred <- ifelse(p >= 0.5, 1, 0)
    mean(pred == y[test_idx])
  })
  
  fold_levels <- sort(unique(df2$fold))
  tibble(fold = fold_levels, accuracy = fold_acc)
  
}


# KITCHEN SINK MODEL
# Prefer df_chunked (raw male chunked, before  modality centering)
df_chunked_base <- df_chunked

# center by per-vowel modality means
add_vowel_modality_centered <- function(df, feature_cols,
                                        vowel_col = "vowel",
                                        modality_col = "modality",
                                        suffix = "_vmc") {
  df %>%
    group_by(.data[[vowel_col]], .data[[modality_col]]) %>%
    mutate(across(
      all_of(feature_cols),
      ~ .x - mean(.x, na.rm = TRUE),
      .names = paste0("{col}", suffix)
    )) %>%
    ungroup()
}

# one-hot encode vowel to use as feature 
add_vowel_onehot <- function(df, vowel_col = "vowel", prefix = "vowel_") {
  mm <- model.matrix(stats::as.formula(paste0("~", vowel_col, " - 1")), data = df)
  mm <- as_tibble(mm)
  names(mm) <- gsub(paste0("^", vowel_col), prefix, names(mm))
  bind_cols(df, mm)
}


# Create kitchen-sink df
df_chunked_kitchensink <- df_chunked_base %>%
  add_vowel_modality_centered(feature_cols_chunked,
                              vowel_col = "vowel",
                              modality_col = "modality",
                              suffix = "_vmc") %>%
  add_vowel_onehot(vowel_col = "vowel", prefix = "vowel_")

# Add a zero-valued negative control feature
df_chunked_kitchensink <- df_chunked_kitchensink %>%
  mutate(zero_feature = 0)


# Final feature list: raw + modality-centered + vowel dummies
vowel_cols <- names(df_chunked_kitchensink) %>% stringr::str_subset("^vowel_")

feature_cols_kitchensink <- c(
  paste0(feature_cols_chunked, "_vmc"),       
  vowel_cols,
  "zero_feature" 
)

# ensure finite numeric features
df_chunked_kitchensink <- df_chunked_kitchensink %>%
  mutate(across(all_of(feature_cols_kitchensink), ~ ifelse(is.finite(.x), .x, NA_real_)))


# Speaker-wise CV on kitchen sink features 
run_one_trial_kitchensink <- function(seed, nfold = 5) {
  fold_map <- make_speaker_folds(df_chunked_kitchensink, group_col = "speaker", nfold = nfold, seed = seed)
  df_cv <- df_chunked_kitchensink %>% left_join(fold_map, by = "speaker")
  fit_xgb_groupcv_folds(df_cv, feature_cols_kitchensink) %>%
    mutate(model = "chunked_kitchen_sink", seed = seed)
}

n_trials <- 50
seeds <- 1:n_trials

ks_out <- map(seeds, ~ run_one_trial_kitchensink(seed = .x, nfold = 5))
ks_results <- bind_rows(ks_out)


# summarise across folds within each seed, then across seeds
ks_seed_level <- ks_results %>%
  group_by(seed) %>%
  summarise(mean_acc = mean(accuracy), .groups = "drop")

ks_seed_level %>%
  summarise(
    mean_acc = mean(mean_acc),
    sd_acc   = sd(mean_acc),
    n_trials = dplyr::n(),
    .groups = "drop"
  )

# inspect feature count
length(feature_cols_kitchensink)


# final kitchen sink model
dmat_all <- xgb.DMatrix(
  data  = as.matrix(df_chunked_kitchensink[, feature_cols_kitchensink]),
  label = df_chunked_kitchensink$y
)

model_kitchensink_all <- xgboost(
  data = dmat_all,
  nrounds = 50,
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 3,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)


importance_all <- xgb.importance(
  feature_names = feature_cols_kitchensink,
  model = model_kitchensink_all
)

# Look at top features
importance_pretty <- importance_all %>%
  mutate(
    Feature_label = Feature %>%
      # identify centered features
      str_replace("_vmc$", "") %>%
      
      # remove mean marker
      str_replace("_mean_", "_") %>%
      str_replace("_mean$", "") %>%
      
      # CoV formatting
      str_replace("_cov_", " CoV ") %>%
      str_replace("_cov$", " CoV") %>%
      
      # acoustic renames
      str_replace("^H1Res", "Residual H1*") %>%
      str_replace("^H2KH5Kc", "H2kHz*–H5kHz*") %>%
      str_replace("^H42Kc", "H4*–H2kHz*") %>%
      str_replace("^H2H4c", "H2*–H4*") %>%
      str_replace("^CPP", "CPP") %>%
      str_replace("^DF", "Formant Dispersion") %>%
      str_replace("^s", "") %>%
      
      # chunk labels
      str_replace("_beg$", " (onset)") %>%
      str_replace("_mid$", " (midpoint)") %>%
      str_replace("_end$", " (offset)") %>%
      
      # vowel dummies
      str_replace("^vowel_", "Vowel = ") %>%
      
      # cleanup
      str_replace_all("_", " ") %>%
      str_squish()
  )


top15_pretty <- importance_pretty %>%
  slice_max(Gain, n = 15) %>%
  arrange(Gain)

ggplot(
  top15_pretty,
  aes(x = Gain, y = reorder(Feature_label, Gain))
) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Gain",
    y = "Feature"
  ) +
  theme_minimal(base_size = 13)


# top 15 model 
top15_features <- importance_all %>%
  slice_max(Gain, n = 15) %>%
  pull(Feature)


# TOP-15 MODEL
# define the CV runner 
run_one_trial_top15 <- function(seed, nfold = 5) {
  fold_map <- make_speaker_folds(
    df_chunked_kitchensink,
    group_col = "speaker",
    nfold = nfold,
    seed = seed
  )
  
  df_cv <- df_chunked_kitchensink %>%
    left_join(fold_map, by = "speaker")
  
  fit_xgb_groupcv_folds(df_cv, top15_features) %>%
    mutate(model = "top15", seed = seed)
}

# run 50 trials 
n_trials <- 50
seeds <- 1:n_trials

top15_results <- purrr::map_df(seeds, ~ run_one_trial_top15(seed = .x, nfold = 5))

# summarize accuracy
top15_seed_level <- top15_results %>%
  group_by(seed) %>%
  summarise(mean_acc = mean(accuracy), .groups = "drop")

top15_seed_level %>%
  summarise(
    mean_acc = mean(mean_acc),
    sd_acc   = sd(mean_acc),
    n_trials = n(),
    .groups = "drop"
  )







######plotting mean structure #####

# look at F1 and F2 mean structure 
vars_formant <- c("sF1", "sF2")
n_bins <- 9

# reload acoustic df 
df_formant <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/output.csv") %>%
  mutate(speaker = substr(Filename, 1, 3)) %>%
  left_join(ground_truth, by = "speaker") %>%
  select(
    Filename, Label, seg_Start, seg_End,
    sF1, sF2,
    speaker, ground_truth_label
  ) %>%
  mutate(
    modality = case_when(
      str_ends(Label, "_hc") ~ "breathy_creaky",
      str_ends(Label, "_c")  ~ "creaky",
      str_ends(Label, "_h")  ~ "breathy",
      TRUE                   ~ "modal"
    ),
    Label = str_remove(Label, "_(hc|c|h)$")
  ) %>%
  filter(str_detect(Filename, "central|mono|raising|fronting")) %>%
  left_join(vowel_codes, by = "Label") %>%
  mutate(
    gender = substr(Filename, 2, 2),
    gender = case_when(
      gender == "F" ~ "female",
      gender == "M" ~ "male",
      TRUE ~ NA_character_
    ),
    vowel = case_when(
      vowel %in% c("ɑh", "ah") ~ "ɑ",
      vowel == "ih"            ~ "ɪ",
      vowel == "ai"            ~ "æi",
      vowel %in% c("ɑ'", "a")  ~ "æ",
      TRUE ~ vowel
    )
  ) %>%
  drop_na(vowel) %>%
  filter(
    gender == "male",
    ground_truth_label != "Ambiguous",
    is.finite(sF1),
    is.finite(sF2)
  )



df_norm9_F12 <- df_formant %>%
  group_by(Filename, Label, seg_Start, seg_End) %>%
  mutate(
    n_ms = n(),
    t_norm = ifelse(
      n_ms == 1,
      0.5,
      (row_number() - 1) / (n_ms - 1)
    ),
    t_bin = pmin(n_bins, floor(t_norm * n_bins) + 1)
  ) %>%
  ungroup()


df_token_F12 <- df_norm9_F12 %>%
  pivot_longer(
    all_of(vars_formant),
    names_to = "feature",
    values_to = "value"
  ) %>%
  group_by(
    Filename, Label, seg_Start, seg_End,
    vowel, ground_truth_label, feature, t_bin
  ) %>%
  summarise(
    token_mean = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

df_mean9_F12 <- df_token_F12 %>%
  group_by(vowel, ground_truth_label, feature, t_bin) %>%
  summarise(
    mean_value = mean(token_mean, na.rm = TRUE),
    sd_value   = sd(token_mean, na.rm = TRUE),
    n_tokens   = sum(!is.na(token_mean)),
    se_value   = sd_value / sqrt(n_tokens),
    ci_low     = mean_value - 1.96 * se_value,
    ci_high    = mean_value + 1.96 * se_value,
    .groups = "drop"
  ) %>%
  mutate(
    t_norm_center = (t_bin - 0.5) / n_bins,
    feature_label = recode(feature, sF1 = "F1", sF2 = "F2")
  )

ggplot(
  df_mean9_F12,
  aes(
    x = t_norm_center,
    y = mean_value,
    color = ground_truth_label,
    fill  = ground_truth_label
  )
) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    alpha = 0.2,
    color = NA,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_grid(feature_label ~ vowel, scales = "free_y") +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.25),
    limits = c(0, 1),
    labels = scales::label_number(
      accuracy = 0.1
    )
  ) +
  labs(
    x = "Normalized Time",
    y = "Mean formant value",
    color = "Perceptually-Assigned Race"
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 13) +   # ← FIRST
  theme(
    ## VOWEL labels (top strips)
    strip.text.x = element_text(
      size = 18,
      face = "bold"
    ),
    
    panel.spacing.x = unit(1.5, "lines"),
    
    strip.text.y = element_text(
      size = 18,
      face = "bold",
      angle = 0
    ),
    
    strip.placement = "outside",
    
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 14)
  )


# plot top voice quality features 
df_clean_male <- df_clean %>%
  filter(gender == "male")

vars_to_plot <- c("H1Res", "H2KH5Kc", "H42Kc", "sF4", "H2H4c")
n_bins <- 9

df_norm9 <- df_clean_male %>%
  select(Filename, speaker, ground_truth_label, vowel, Label, modality, seg_Start, seg_End,
         all_of(vars_to_plot)) %>%
  group_by(Filename, Label, seg_Start, seg_End) %>%
  mutate(
    n_ms = n(),
    t_norm = ifelse(n_ms == 1, 0.5, (row_number() - 1) / (n_ms - 1)),
    t_bin = pmin(n_bins, floor(t_norm * n_bins) + 1)
  ) %>%
  ungroup()


df_mean9 <- df_norm9 %>%
  pivot_longer(all_of(vars_to_plot), names_to = "feature", values_to = "value") %>%
  group_by(
    Filename, Label, seg_Start, seg_End,
    ground_truth_label, feature, t_bin
  ) %>%
  summarise(
    token_mean = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ground_truth_label, feature, t_bin) %>%
  summarise(
    mean_value = mean(token_mean, na.rm = TRUE),
    sd_value   = sd(token_mean, na.rm = TRUE),
    n_tokens   = sum(!is.na(token_mean)),
    se_value   = sd_value / sqrt(n_tokens),
    ci_low     = mean_value - 1.96 * se_value,
    ci_high    = mean_value + 1.96 * se_value,
    .groups = "drop"
  ) %>%
  mutate(
    t_norm_center = (t_bin - 0.5) / n_bins
  ) %>%
  mutate(
    feature_label = feature %>%
      str_replace("^H1Res$", "Residual H1*") %>%
      str_replace("^H2KH5Kc$", "H2kHz*–H5kHz*") %>%
      str_replace("^H42Kc$", "H4*–H2kHz*") %>%
      str_replace("^H2H4c$", "H2*–H4*")
  )




ggplot(
  df_mean9,
  aes(
    x = t_norm_center,
    y = mean_value,
    color = ground_truth_label,
    fill  = ground_truth_label
  )
) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    alpha = 0.2,
    color = NA,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ feature_label, scales = "free_y") +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.25),
    limits = c(0, 1)
  ) +
  labs(
    x = "Normalized Time",
    y = "Mean normalized value",
    color = "Perceptually-Assigned Race"
  ) +
  guides(fill = "none") +
  theme_minimal()

