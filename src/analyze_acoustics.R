library(tidyverse)
library(lme4)
library(xgboost)
library(purrr)


set.seed(123)

# ---------------------
# Load ground truth csv 
ground_truth <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/ground_truth_label.csv") %>%
  select(speaker, ground_truth_label)


# ---------------------
# Load acoustic csv 
df <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/output.csv") %>%
  mutate(speaker = substr(Filename, 1, 3))


# ---------------------
# join and build df 
df <- df %>%
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
      TRUE                  ~ "modal"
    ),
    Label = str_remove(Label, "_(hc|c|h)$")
  ) %>%
  filter(str_detect(Filename, "central|mono|raising|fronting")) %>%
  mutate(
    sentence_type = case_when(
      str_detect(Filename, "central")  ~ "central",
      str_detect(Filename, "mono")     ~ "mono",
      str_detect(Filename, "raising")  ~ "raising",
      str_detect(Filename, "fronting") ~ "fronting",
      TRUE ~ NA_character_
    )
  )


df <- df %>%
  select(
    Filename,
    Label,
    speaker,
    ground_truth_label,
    modality,
    sentence_type,
    everything()
  )


# ---------------------
# add vowel code  
vowel_codes <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/metadata/vowel_codes.csv") %>%
  rename(Label = "code") %>%
  mutate(Label = as.character(Label))

# join 
df <- df %>%
  left_join(vowel_codes, by = "Label")

# ---------------------
# create ground truth gender column 
df <- df %>%
  mutate(
    gender = substr(Filename, 2, 2),   
    gender = case_when(
      gender == "F" ~ "female",
      gender == "M" ~ "male",
      TRUE ~ NA_character_
    ) 
  )


# ---------------------
# fix vowel names 
df <- df %>%
  mutate(
    vowel = case_when(
      vowel %in% c("ɑh","ah") ~ "ɑ",   
      vowel == "ih"           ~ "ɪ",
      vowel == "ai"           ~ "æi",
      vowel == "ɑ'"           ~ "æ",   
      vowel == "a"            ~ "æ",
      TRUE ~ vowel
    )
  ) %>%
  drop_na(vowel)



acoustic_vars <- c(
  "H1c", "H2H4c", "H42Kc", "H2KH5Kc",
  "CPP", "Energy",
  "SHR",
  "sF0", "sF1", "sF2", "sF3", "sF4"
)


# filter out impossible 0's and NA's
df <- df %>%
  filter(if_all(all_of(acoustic_vars), ~ . != 0))

df <- df %>%
  filter(
    if_all(all_of(acoustic_vars), ~ is.finite(.) & . != 0)
  )


# filter out ambiguous 
df <- df %>%
  filter(ground_truth_label != "Ambiguous")


# ---------------------
# filter out outliers 
is_not_outlier_mad <- function(x, k = 3.5) {
  med <- median(x, na.rm = TRUE)
  mad_val <- mad(x, constant = 1, na.rm = TRUE)
  abs(x - med) < k * mad_val
}

df <- df %>%
  group_by(vowel) %>%
  mutate(
    across(all_of(acoustic_vars),
           ~ is_not_outlier_mad(.x),
           .names = "keep_{col}")
  ) %>%
  ungroup()


df <- df %>%
  mutate(
    is_outlier = if_any(starts_with("keep_"), ~ .x == FALSE)
  )

outlier_table <- df %>%
  summarise(across(starts_with("keep_"), ~ sum(.x == FALSE))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "n_outliers") %>%
  mutate(variable = str_replace(variable, "keep_", ""))

outlier_table

df_clean <- df %>%
  filter(is_outlier == FALSE) %>%
  select(-starts_with("keep_"), -is_outlier)



# --------------------------
# create res h1 
df_clean <- df_clean %>%
  mutate(Energy = pmax(Energy, .Machine$double.eps))


mod_h1 <- lmer(
  H1c ~ log(Energy) + (log(Energy) | speaker),
  data = df_clean
)

beta_energy <- fixef(mod_h1)["log(Energy)"]

df_clean <- df_clean %>%
  mutate(
    H1Res = H1c - beta_energy * log(Energy)
  )



# ------------------
# formant dispersion
df_clean <- df_clean %>%
  mutate(
    DF = ((sF2 - sF1) + (sF3 - sF2) + (sF4 - sF3)) / 3
  )



# ------------------
# normalize across min and max 
norm_vars <- c(
  "CPP", "SHR",
  "H1Res", "H2H4c", "H42Kc", "H2KH5Kc", "sF0",
  "sF1", "sF2", "sF3", "sF4", "DF"
)


minmax_norm <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}



df_clean <- df_clean %>%
  group_by(gender) %>%
  mutate(
    across(
      all_of(norm_vars),
      minmax_norm
    )
  ) %>%
  ungroup()



# ---------------------
# Define analysis variables
analysis_vars <- c(
  "CPP", "SHR",
  "H1Res", "H2H4c", "H42Kc", "H2KH5Kc","sF0",
  "sF1", "sF2", "sF3", "sF4", "DF"
)

# ---------------------
# Assign frames to 3 equidistant chunks within each vowel token
df_chunked <- df_clean %>%
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
  group_by(
    Filename,
    speaker,
    gender,
    ground_truth_label,
    vowel,
    Label,
    modality,
    seg_Start,
    seg_End,
    chunk
  ) %>%
  summarise(
    across(
      all_of(analysis_vars),
      list(
        mean = mean,
        cov  = ~ sd(.x) / mean(.x)
      ),
      .names = "{col}_{fn}"
    ),
    .groups = "drop"
  )


# ---------------------
# Summarise acoustics by vowel token × chunk
df_chunked <- df_chunked %>%
  pivot_wider(
    id_cols = c(
      Filename,
      speaker,
      gender,
      ground_truth_label,
      modality,
      vowel,
      Label,
      seg_Start,
      seg_End
    ),
    names_from = chunk,
    values_from = matches("_(mean|cov)$"),
    names_glue = "{.value}_{chunk}"
  )

# ------------------------
# xg boosted trees 
df_chunked_norm <- df_chunked %>%
  mutate(
    y = ifelse(ground_truth_label == "Black", 1, 0)
  )

feature_cols <- df_chunked_norm %>%
  select(matches("_(mean|cov)_(beg|mid|end)$")) %>%
  colnames()


# model function 
fit_xgb_model <- function(df, features) {
  
  X <- as.matrix(df[, features])
  y <- df$y
  
  dmat <- xgb.DMatrix(data = X, label = y)
  set.seed(123)
  
  cv <- xgb.cv(
    data = dmat,
    nrounds = 50,
    nfold = 5,
    early_stopping_rounds = 10,
    objective = "binary:logistic",
    eval_metric = "error",   # <-- classification error
    max_depth = 3,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    verbose = 0
  )
  
  best_error <- min(cv$evaluation_log$test_error_mean)
  
  list(
    accuracy = 1 - best_error,
    best_iteration = cv$best_iteration
  )
}


# baseline model 
results_model1 <- df_chunked_norm %>%
  group_split(vowel) %>%
  map_df(~ {
    out <- fit_xgb_model(.x, feature_cols)
    tibble(
      vowel = unique(.x$vowel),
      model = "raw",
      accuracy = out$accuracy
    )
  })



center_by_modality <- function(df, features) {
  df %>%
    group_by(modality) %>%
    mutate(
      across(
        all_of(features),
        ~ .x - mean(.x, na.rm = TRUE)
      )
    ) %>%
    ungroup()
}

# modality centered model 
results_model2 <- df_chunked_norm %>%
  group_split(vowel) %>%
  map_df(~ {
    
    df_centered <- center_by_modality(.x, feature_cols)
    out <- fit_xgb_model(df_centered, feature_cols)
    
    tibble(
      vowel = unique(.x$vowel),
      model = "modality_centered",
      accuracy = out$accuracy
    )
  })



# get results 
results_all <- bind_rows(results_model1, results_model2)

comparison <- results_all %>%
  pivot_wider(
    names_from = model,
    values_from = accuracy
  ) %>%
  mutate(
    delta = modality_centered - raw
  )


comparison %>%
  summarise(
    mean_raw = mean(raw),
    mean_centered = mean(modality_centered),
    mean_delta = mean(delta)
  )


# feature importance 
get_feature_importance <- function(df, features) {
  
  X <- as.matrix(df[, features])
  y <- df$y
  
  dmat <- xgb.DMatrix(data = X, label = y)
  
  model <- xgboost(
    data = dmat,
    nrounds = 50,
    objective = "binary:logistic",
    max_depth = 3,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    verbose = 0
  )
  
  xgb.importance(
    feature_names = features,
    model = model
  )
}

importance_by_vowel <- df_chunked_norm %>%
  group_split(vowel) %>%
  map_df(~ {
    
    df_centered <- .x %>%
      group_by(modality) %>%
      mutate(
        across(
          all_of(feature_cols),
          ~ .x - mean(.x, na.rm = TRUE)
        )
      ) %>%
      ungroup()
    
    imp <- get_feature_importance(df_centered, feature_cols)
    
    imp %>%
      mutate(vowel = unique(.x$vowel))
  })



# plot that shows all features 
importance_all <- importance_by_vowel %>%
  group_by(Feature) %>%
  summarise(
    mean_gain = mean(Gain),
    .groups = "drop"
  ) %>%
  arrange(mean_gain)


#save top 15 features (to compare later) 
top15_unnamed <- importance_all %>%
  slice_max(mean_gain, n = 15) %>%
  arrange(mean_gain)


#rename top 15 features for plot 
feature_labels <- c(
  sF1_mean_mid   = "F1 mean (mid point)",
  sF3_cov_beg    = "F3 CoV (vowel onset)",
  DF_mean_beg    = "DF mean (vowel onset)",
  sF1_mean_beg   = "F1 mean (vowel onset)",
  H42Kc_cov_end  = "H4–2kHz CoV (vowel offset)",
  H2KH5Kc_mean_beg = "H2k–5kHz mean (vowel onset)",
  H1Res_cov_end  = "H1Res CoV (vowel offset)",
  sF0_cov_mid    = "F0 CoV (mid vowel)",
  CPP_cov_end    = "CPP CoV (vowel offset)",
  sF1_cov_mid    = "F1 CoV (mid point)",
  sF1_cov_end    = "F1 CoV (vowel offset)",
  sF4_cov_mid    = "F4 CoV (mid point)",
  CPP_mean_beg   = "CPP mean (vowel onset)",
  sF2_mean_beg   = "F2 mean (vowel onset)",
  H42Kc_cov_beg  = "H4–2kHz CoV (vowel onset)"
)

#save top 15 renamed features in a new df
top15 <- top15_unnamed %>%
  mutate(
    Feature_label = recode(Feature, !!!feature_labels)
  )

ggplot(top15,
       aes(x = mean_gain,
           y = reorder(Feature, mean_gain))) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Mean Gain",
    y = "Feature"
  ) +
  theme_minimal()


# permutation test 
permutation_test <- function(df, features) {
  
  df_perm <- df %>%
    mutate(y = sample(y))
  
  fit_xgb_model(df_perm, features)$accuracy
}


perm_acc <- replicate(
  50,
  permutation_test(df_chunked_norm, feature_cols)
)

# chance accuracy = around 63%, matching that of random permutations
df_chunked_norm %>%
  count(y) %>%
  mutate(prop = n / sum(n))


# maybe run the same analysis only on males and see if you pull the same features 
# if the formant feature go away 
# f1 could just be a human size feature 
# if you're finding that there are vowel specific differences, then you can conclude that there isa vowel quality difference
# just look at the averages, plot the means, plot the scatterplot on an f1/f2 space

# Maybe just create a model with the 5 major features and see where the means fall

#try xgboost on male only df
df_chunked_male <- df_chunked_norm %>%
  filter(gender != "female")

results_model3 <- df_chunked_male %>%
  group_split(vowel) %>%
  map_df(~ {
    
    df_centered <- center_by_modality(.x, feature_cols)
    out <- fit_xgb_model(df_centered, feature_cols)
    
    tibble(
      vowel = unique(.x$vowel),
      model = "modality_centered_male_only",
      accuracy = out$accuracy
    )
  })

results_all <- bind_rows(
  results_model1,
  results_model2,
  results_model3
)

comparison <- results_all %>%
  pivot_wider(
    names_from = model,
    values_from = accuracy
  ) %>%
  mutate(
    delta_centered_vs_raw = modality_centered - raw,
    delta_male_vs_centered = modality_centered_male_only - modality_centered
  )

comparison %>%
  summarise(
    mean_raw = mean(raw, na.rm = TRUE),
    mean_centered = mean(modality_centered, na.rm = TRUE),
    mean_male_only = mean(modality_centered_male_only, na.rm = TRUE),
    mean_delta_centered = mean(delta_centered_vs_raw, na.rm = TRUE),
    mean_delta_male = mean(delta_male_vs_centered, na.rm = TRUE)
  )


importance_by_vowel_male <- df_chunked_male %>%
  group_split(vowel) %>%
  map_df(~ {
    
    df_centered <- .x %>%
      group_by(modality) %>%
      mutate(
        across(
          all_of(feature_cols),
          ~ .x - mean(.x, na.rm = TRUE)
        )
      ) %>%
      ungroup()
    
    imp <- get_feature_importance(df_centered, feature_cols)
    
    imp %>%
      mutate(vowel = unique(.x$vowel))
  })


importance_male_all <- importance_by_vowel_male %>%
  group_by(Feature) %>%
  summarise(
    mean_gain = mean(Gain),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_gain))

top15_male <- importance_male_all %>%
  slice_max(mean_gain, n = 15) %>%
  arrange(mean_gain)


# ---------------------
# Step 3: freeze top-15 feature sets (do this ONCE)

top15_all <- importance_all %>%
  slice_max(mean_gain, n = 15) %>%
  arrange(mean_gain) 

# plot features from male model 
ggplot(
  top15_male,
  aes(
    x = mean_gain,
    y = reorder(Feature, mean_gain)
  )
) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Mean Gain",
    y = "Feature"
  ) +
  theme_minimal()



# analyze feature overlap 
normalize_feature_for_overlap <- function(feature) {
  
  parts <- str_split(feature, "_", simplify = TRUE)
  
  acoustic_var <- parts[, 1]
  stat         <- parts[, 2]
  chunk        <- parts[, 3]
  
  case_when(
    # mean: ignore chunk
    stat == "mean" ~ paste(acoustic_var, stat, sep = "_"),
    
    # cov: keep chunk
    stat == "cov"  ~ paste(acoustic_var, stat, chunk, sep = "_"),
    
    TRUE ~ feature
  )
}

top15_all_cmp <- top15_all %>%
  mutate(feature_cmp = normalize_feature_for_overlap(Feature))

top15_male_cmp <- top15_male %>%
  mutate(feature_cmp = normalize_feature_for_overlap(Feature))


overlap_features_cmp <- intersect(
  top15_all_cmp$feature_cmp,
  top15_male_cmp$feature_cmp
)

overlap_features_cmp
length(overlap_features_cmp)





