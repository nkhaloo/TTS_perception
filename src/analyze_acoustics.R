library(tidyverse)
library(lme4)
library(lmerTest)
library(xgboost)
library(purrr)

set.seed(555)

# -----------------------
# Load + join ground truth
ground_truth <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/ground_truth_label.csv") %>%
  select(speaker, ground_truth_label)

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

# -----------------------
# Add vowel metadata
vowel_codes <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/metadata/vowel_codes.csv") %>%
  rename(Label = "code") %>%
  mutate(Label = as.character(Label))

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

# -----------------------
# Filter impossible values
acoustic_vars <- c(
  "H1c", "H2H4c", "H42Kc", "H2KH5Kc",
  "CPP", "Energy", "SHR",
  "sF0", "sF1", "sF2", "sF3", "sF4"
)

df <- df %>%
  filter(if_all(all_of(acoustic_vars), ~ is.finite(.) & . != 0))

# -----------------------
# MAD outlier filtering within vowel
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

# -----------------------
# Compute H1Res (Energy residualization)
df_clean <- df_clean %>%
  mutate(Energy = pmax(Energy, .Machine$double.eps))

mod_h1 <- lmer(
  H1c ~ log(Energy) + (log(Energy) | speaker),
  data = df_clean
)

beta_energy <- fixef(mod_h1)["log(Energy)"]

df_clean <- df_clean %>%
  mutate(H1Res = H1c - beta_energy * log(Energy))

# -----------------------
# Formant dispersion
df_clean <- df_clean %>%
  mutate(DF = ((sF2 - sF1) + (sF3 - sF2) + (sF4 - sF3)) / 3)

# -----------------------
# Min-max normalize within gender (as in your earlier pipeline)
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

# =========================================================
# Whole-vowel features (mean + CoV) and male-only model
# =========================================================

analysis_vars <- c("CPP","SHR","H1Res","H2H4c","H42Kc","H2KH5Kc","sF0","sF1","sF2","sF3","sF4","DF")

cov_safe <- function(x) {
  m <- mean(x, na.rm = TRUE)
  if (is.na(m) || m == 0) return(NA_real_)
  sd(x, na.rm = TRUE) / m
}

df_whole <- df_clean %>%
  group_by(Filename, speaker, gender, ground_truth_label, vowel, Label, modality, seg_Start, seg_End) %>%
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
  ) %>%
  filter(gender == "male") %>%
  mutate(y = ifelse(ground_truth_label == "Black", 1, 0))

feature_cols_whole <- df_whole %>%
  select(matches("_(mean|cov)$")) %>%
  colnames()

# Modality-centering (same as before)
df_whole <- df_whole %>%
  group_by(modality) %>%
  mutate(across(all_of(feature_cols_whole), ~ .x - mean(.x, na.rm = TRUE))) %>%
  ungroup()

# -----------------------
# Speaker-wise CV (grouped by speaker)
fit_xgb_groupcv <- function(df, features, group_col = "speaker", nfold = 5, seed = 123) {
  set.seed(seed)
  
  # use the column indicated by group_col (default "speaker")
  groups <- df %>% distinct(.data[[group_col]]) %>% pull(.data[[group_col]])
  n_g <- length(groups)
  
  k <- min(nfold, n_g)
  if (k < 2) return(NA_real_)
  
  fold_id <- sample(rep(1:k, length.out = n_g))
  fold_map <- tibble(!!group_col := groups, fold = fold_id)
  
  df2 <- df %>% left_join(fold_map, by = group_col)
  
  X <- as.matrix(df2[, features])
  y <- df2$y
  dmat <- xgb.DMatrix(X, label = y)
  
  folds <- map(1:k, ~ which(df2$fold == .x))
  
  cv <- xgb.cv(
    data = dmat,
    nrounds = 50,
    folds = folds,
    early_stopping_rounds = 10,
    objective = "binary:logistic",
    eval_metric = "error",
    max_depth = 3,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    verbose = 0
  )
  
  1 - min(cv$evaluation_log$test_error_mean)
}

# Accuracy per vowel (speaker-wise CV)
results_whole_male <- df_whole %>%
  group_split(vowel) %>%
  map_df(~ tibble(
    vowel = unique(.x$vowel),
    accuracy = fit_xgb_groupcv(.x, feature_cols_whole, group_col = "speaker", nfold = 5, seed = 123)
  ))

results_whole_male

results_whole_male %>%
  summarise(mean_new = mean(accuracy, na.rm = TRUE))

# -----------------------
# Pooled model for feature importance (not CV)
dmat_all <- xgb.DMatrix(
  data = as.matrix(df_whole[, feature_cols_whole]),
  label = df_whole$y
)

model_all <- xgboost(
  data = dmat_all,
  nrounds = 50,
  objective = "binary:logistic",
  max_depth = 3,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)

importance_all <- xgb.importance(
  feature_names = feature_cols_whole,
  model = model_all
)

top15_whole <- importance_all %>%
  slice_max(Gain, n = 15) %>%
  arrange(Gain)

ggplot(
  top15_whole,
  aes(x = Gain, y = reorder(Feature, Gain))
) +
  geom_col(fill = "steelblue") +
  labs(x = "Gain", y = "Feature") +
  theme_minimal()

# -----------------------
# Cohen's d on top features (per vowel)
cohens_d <- function(x, group) {
  x1 <- x[group == "Black"]
  x2 <- x[group == "White"]
  
  m1 <- mean(x1, na.rm = TRUE)
  m2 <- mean(x2, na.rm = TRUE)
  s1 <- sd(x1, na.rm = TRUE)
  s2 <- sd(x2, na.rm = TRUE)
  
  sp <- sqrt(((length(x1) - 1) * s1^2 + (length(x2) - 1) * s2^2) /
               (length(x1) + length(x2) - 2))
  
  (m1 - m2) / sp
}

features_to_validate_whole <- importance_all %>%
  arrange(desc(Gain)) %>%     # <-- ensure top first
  slice_head(n = 7) %>%
  pull(Feature)

d_results_whole <- map_df(
  features_to_validate_whole,
  function(feat) {
    df_whole %>%
      group_by(vowel) %>%
      summarise(
        d = cohens_d(.data[[feat]], ground_truth_label),
        feature = feat,
        .groups = "drop"
      )
  }
)

d_results_whole

d_summary_whole <- d_results_whole %>%
  group_by(feature) %>%
  summarise(
    mean_d = mean(d, na.rm = TRUE),
    median_d = median(d, na.rm = TRUE),
    n_pos = sum(d > 0, na.rm = TRUE),
    n_neg = sum(d < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(mean_d)))

d_summary_whole



# linear regression 
library(tidyverse)
library(lme4)
library(lmerTest)
library(purrr)

df_whole <- df_whole %>%
  mutate(
    ground_truth_label = factor(ground_truth_label, levels = c("White", "Black"))
  )

features_lmm <- features_to_validate_whole   # top 7 features

lmm_results <- map_df(
  features_lmm,
  function(feat) {
    
    mod <- lmer(
      as.formula(
        paste(feat, "~ ground_truth_label + (1 | speaker) + (1 | vowel)")
      ),
      data = df_whole
    )
    
    cf <- summary(mod)$coefficients
    p  <- cf["ground_truth_labelBlack", "Pr(>|t|)"]
    
    tibble(
      feature  = feat,
      estimate = cf["ground_truth_labelBlack", "Estimate"],
      se       = cf["ground_truth_labelBlack", "Std. Error"],
      t        = cf["ground_truth_labelBlack", "t value"],
      p        = p,
      sig      = case_when(
        p < 0.001 ~ "***",
        p < 0.01  ~ "**",
        p < 0.05  ~ "*",
        p < 0.1   ~ ".",
        TRUE      ~ ""
      )
    )
  }
)

lmm_results








# compare mean model to onnset/offset model
feature_cols_chunked <- df_chunked_male %>%
  select(matches("_(mean|cov)_(beg|mid|end)$")) %>%
  colnames()

results_chunked_groupcv <- df_chunked_male %>%
  group_split(vowel) %>%
  map_df(~ tibble(
    vowel = unique(.x$vowel),
    accuracy_chunked = fit_xgb_groupcv(
      df = .x,
      features = feature_cols_chunked,
      group_col = "speaker",
      nfold = 5,
      seed = 123
    )
  ))

comparison_chunk_vs_whole <- results_chunked_groupcv %>%
  left_join(results_whole_male, by = "vowel") %>%
  rename(accuracy_whole = accuracy) %>%
  mutate(delta = accuracy_chunked - accuracy_whole)

comparison_chunk_vs_whole %>%
  summarise(
    mean_chunked = mean(accuracy_chunked, na.rm = TRUE),
    mean_whole   = mean(accuracy_whole, na.rm = TRUE),
    mean_delta   = mean(delta, na.rm = TRUE)
  )

t.test(
  comparison_chunk_vs_whole$accuracy_chunked,
  comparison_chunk_vs_whole$accuracy_whole,
  paired = TRUE
)

wilcox.test(
  comparison_chunk_vs_whole$accuracy_chunked,
  comparison_chunk_vs_whole$accuracy_whole,
  paired = TRUE
)




