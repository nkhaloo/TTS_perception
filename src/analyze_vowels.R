library(tidyverse)
library(mgcv)
library(itsadug)
library(ggplot2)

# ---------------------
# Load ground truth csv 
ground_truth <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/formants/ground_truth_label.csv")


# ---------------------
# Load acoustic csv 
df <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/formants/output_all.csv") %>%
  mutate(speaker = substr(Filename, 1, 3))


# ---------------------
# join and build df 
df <- df %>%
  left_join(ground_truth, by = "speaker") %>%
  select(
    Filename, Label, seg_Start, seg_End,
    H1c, H2H4c, H42Kc, H2KH5Kc,
    CPP, Energy,
    HNR05, HNR15, HNR25, HNR35,
    sF0, sF1, sF2, sF3, sF4,
    epoch, soe,
    speaker, ground_truth_label, mean_gender_scale
  ) %>%
  filter(
    if_all(
      -c(soe, epoch),    
      ~ .x != 0            
    )
  ) %>%
  mutate(
    modality = case_when(
      str_ends(speaker, "hc") ~ "breathy_creaky",
      str_ends(speaker, "c")  ~ "creaky",
      str_ends(speaker, "h")  ~ "breathy",
      TRUE ~ "modal"
    )
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
    ground_truth_label,
    mean_gender_scale,
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

# ---------------------
# filter out outliers 
is_not_outlier_mad <- function(x, k = 3.5) {
  med <- median(x, na.rm = TRUE)
  mad_val <- mad(x, constant = 1, na.rm = TRUE)
  abs(x - med) < k * mad_val
}

acoustic_vars <- c(
  "H1c", "H2H4c", "H42Kc", "H2KH5Kc",
  "CPP", "Energy",
  "HNR05", "HNR15", "HNR25", "HNR35",
  "sF0", "sF1", "sF2", "sF3", "sF4"
)

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

df_clean <- df %>% filter(is_outlier == FALSE)


## ---------------------
# create normalized time
df_formants <- df_clean %>%
  select(
    Filename,
    speaker,
    ground_truth_label,
    mean_gender_scale,
    modality,
    sentence_type,
    seg_Start,
    seg_End,
    sF1,
    sF2,
    vowel,
    prec_sound,
    foll_sound,
    voice,
    stressed_syllable,
    word
  ) %>%
  mutate(
    duration = (seg_End - seg_Start) / 1000,   # convert ms → seconds
    log_dur  = log(duration)
  )
  
# BM2_Central.mat causing problems lets observe 
df %>%
  filter(Filename == "BM2_central.mat") %>%
  count(is_outlier)

# Bark transform
toBark <- function(f) 26.81 * f / (1960 + f) - 0.53
df_formants <- df_formants %>%
  mutate(
    F1b = toBark(sF1),
    F2b = toBark(sF2)
  )

# Count surviving frames per token
frames_per_token <- df_formants %>%
  group_by(Filename) %>%
  summarise(n_frames = n()) %>%
  arrange(n_frames)

# Show tokens with fewer than 5 frames (these must be dropped)
tokens_to_drop <- frames_per_token %>%
  filter(n_frames < 5)

tokens_to_drop

df_formants <- df_formants %>%
  filter(!(Filename %in% tokens_to_drop$Filename))



# ---- Now create normalized time safely ----
df_norm <- df_formants %>%
  group_by(Filename) %>%
  arrange(seg_Start, .by_group = TRUE) %>%      
  mutate(
    frame_index = row_number(),                
    n_frames = n(),
    time_bin = ntile(frame_index, 5)     # <<---- FIXED
  ) %>%
  ungroup() %>%
  
  # Now average Bark-scaled F1 and F2 within each of the 5 bins
  group_by(Filename, time_bin) %>%
  summarise(
    F1b = mean(F1b, na.rm = TRUE),
    F2b = mean(F2b, na.rm = TRUE),
    
    # Carry metadata forward
    speaker = first(speaker),
    ground_truth_label = first(ground_truth_label),
    mean_gender_scale = first(mean_gender_scale),
    modality = first(modality),
    sentence_type = first(sentence_type),
    vowel = first(vowel),
    prec_sound = first(prec_sound),
    foll_sound = first(foll_sound),
    voice = first(voice),
    stressed_syllable = first(stressed_syllable),
    word = first(word),
    duration = first(duration),
    log_dur  = first(log_dur)
  ) %>%
  ungroup() %>%
  
  # Make normalized time numeric (1,2,3,4,5)
  mutate(
    norm_time = as.numeric(time_bin)
  )

## ---------------------
# create formant column 
df_long <- df_norm %>%
  pivot_longer(
    cols = c(F1b, F2b),
    names_to = "formant",
    values_to = "value"
  ) %>%
  mutate(
    formant = recode(formant, F1b = "F1", F2b = "F2"),
    formant = factor(formant, levels = c("F1", "F2"))
  )

df_long <- df_long %>%
  mutate(
    sex = case_when(
      substr(Filename, 2, 2) == "F" ~ "female",
      substr(Filename, 2, 2) == "M" ~ "male",
      TRUE ~ NA_character_
    ),
    sex = factor(sex, levels = c("female", "male"))
  )

## ---------------------
# factor coding 
# 1. Ethnicity (3-level factor: Black, White, Ambiguous)
df_long <- df_long %>%
  mutate(
    ethnicity = factor(
      ground_truth_label,
      levels = c("Black", "White", "Ambiguous")   # Black = reference
    )
  )

# 2. Sex (categorical, derived from Filename; female = reference)
df_long <- df_long %>%
  mutate(
    sex = case_when(
      substr(Filename, 2, 2) == "F" ~ "female",
      substr(Filename, 2, 2) == "M" ~ "male",
      TRUE ~ NA_character_
    ),
    sex = factor(sex, levels = c("female", "male"))
  )

# 3. Formant (F1 reference level)
df_long <- df_long %>%
  mutate(
    formant = factor(formant, levels = c("F1", "F2"))
  )

# 4. Create 12-level interaction factor: ethnicity × sex × formant
df_long <- df_long %>%
  mutate(
    GroupFormant = interaction(ethnicity, sex, formant, sep = "_")
  )

# 5. Random-effect grouping variables → convert to factor
df_long <- df_long %>%
  mutate(
    speaker       = factor(speaker),
    word          = factor(word),
    prec_sound    = factor(prec_sound),
    foll_sound    = factor(foll_sound),
    sentence_type = factor(sentence_type)
  )





## ---------------------
# fit full model 
final_models <- list()

for (V in vowels) {
  
  cat("\n=====================================\n")
  cat("FITTING FINAL GAMM FOR VOWEL:", V, "\n")
  cat("=====================================\n\n")
  
  df_v <- df_long %>% filter(vowel == V)
  
  mod_full <- bam(
    value ~ ethnicity * sex * formant + 
      log_dur +
      s(norm_time, by = GroupFormant, k = 3) +
      s(speaker, bs = "re") +
      s(word, bs = "re") +
      s(prec_sound, bs = "re") +
      s(foll_sound, bs = "re"),
    
    data = df_v,
    method = "fREML",
    discrete = TRUE
  )
  
  final_models[[V]] <- mod_full
  
  cat("\n--- SUMMARY FOR VOWEL:", V, "---\n\n")
  
  print(summary(mod_full))
  
  cat("\n=====================================\n")
  cat("Finished model for vowel:", V)
  cat("\n=====================================\n\n")
}



## ---------------------
# model comparison 
compare_models <- function(df_long, vowels) {
  
  results <- data.frame(
    vowel = character(),
    better_model = character(),
    delta_AIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (V in vowels) {
    
    cat("\n=====================================\n")
    cat("Evaluating models for vowel:", V, "\n")
    cat("=====================================\n")
    
    df_v <- df_long %>% filter(vowel == V)
    
    # ----- SIMPLE MODEL -----
    mod_simple <- bam(
      value ~ ethnicity + sex + formant + 
        log_dur +
        s(norm_time, by = ethnicity, k = 4) +
        s(speaker, bs = "re") +
        s(prec_sound, bs = "re") +
        s(foll_sound, bs = "re"),
      
      data = df_v,
      method = "fREML",
      discrete = TRUE
    )
    
    # ----- INTERACTION MODEL -----
    mod_interact <- bam(
      value ~ ethnicity * sex + formant + 
        log_dur +
        s(norm_time, by = ethnicity, k = 4) +
        s(speaker, bs = "re") +
        s(prec_sound, bs = "re") +
        s(foll_sound, bs = "re"),
      
      data = df_v,
      method = "fREML",
      discrete = TRUE
    )
    
    # ----- Compare -----
    aic_simple <- AIC(mod_simple)
    aic_inter  <- AIC(mod_interact)
    
    if (aic_simple < aic_inter) {
      better <- "SIMPLE"
      delta  <- aic_inter - aic_simple
    } else {
      better <- "INTERACTION"
      delta  <- aic_simple - aic_inter
    }
    
    # Store result
    results <- rbind(
      results,
      data.frame(
        vowel = V,
        better_model = better,
        delta_AIC = round(delta, 3)
      )
    )
    
    cat("Better model for", V, ":", better, 
        "(ΔAIC =", round(delta, 3), ")\n")
  }
  
  return(results)
}

model_results <- compare_models(df_long, vowels)
model_results



## ---------------------
# fit simpler model 
simple_models <- list()

for (V in vowels) {
  
  cat("\n=====================================\n")
  cat("FITTING SIMPLE GAMM FOR VOWEL:", V, "\n")
  cat("=====================================\n\n")
  
  df_v <- df_long %>% filter(vowel == V)
  
  mod_simple <- bam(
    value ~ ethnicity + sex + formant + 
      log_dur +
      s(norm_time, by = ethnicity, k = 4) +
      s(speaker, bs = "re") +
      #s(word, bs = "re") +
      s(prec_sound, bs = "re") +
      s(foll_sound, bs = "re"),
    
    data = df_v,
    method = "fREML",
    discrete = TRUE
  )
  
  simple_models[[V]] <- mod_simple
  
  cat("\n--- SUMMARY FOR VOWEL:", V, "---\n\n")
  print(summary(mod_simple))
  
  cat("\n=====================================\n")
  cat("Finished SIMPLE model for vowel:", V)
  cat("\n=====================================\n\n")
}



## ---------------------
# autocorrelation loop 
vowels <- unique(df_long$vowel)

simple_models <- list()
acf_results  <- list()

for (V in vowels) {
  
  message("\n===============================")
  message("Fitting SIMPLE exploratory model for vowel: ", V)
  message("===============================\n")
  
  # -------------------------------
  # Subset vowel data
  # -------------------------------
  df_v <- df_long %>% filter(vowel == V)
  
  # Sorting frames (good practice)
  df_v <- df_v %>%
    arrange(Filename, norm_time) %>%
    mutate(
      AR_start = if_else(
        lag(Filename) != Filename,
        TRUE, FALSE,
        missing = TRUE
      )
    )
  
  # -------------------------------
  # Simple exploratory GAM
  # -------------------------------
  mod <- bam(
    value ~ ethnicity + sex + formant +
      log_dur +
      s(norm_time, by = ethnicity, k = 3, bs = "cr") +
      s(speaker, bs = "re"),
    
    data = df_v,
    method = "fREML",
    discrete = TRUE
  )
  
  simple_models[[V]] <- mod
  
  message("Model fitted for vowel: ", V, "\n")
  
  # -------------------------------
  # Autocorrelation summary
  # -------------------------------
  message("Autocorrelation (overall) for ", V, ":")
  
  acf_vals <- acf_resid(mod)   # overall ACF values
  print(acf_vals)
  
  acf_results[[V]] <- acf_vals
  
  message("\n(No token-level ACF plotted — suppressed due to 5 timepoints/token.)\n")
}



## ---------------------
#get model stats
model_fit_stats <- lapply(names(simple_models), function(v) {
  
  mod <- simple_models[[v]]
  sum_mod <- summary(mod)
  
  tibble(
    vowel = v,
    adj_R2 = sum_mod$r.sq,                       # adjusted R²
    dev_expl = sum_mod$dev.expl * 100            # percent deviance explained
  )
})

model_fit_stats <- bind_rows(model_fit_stats)

print(model_fit_stats) 




## ---------------------
# plot model adjusted results 
## ---------------------
make_predictions <- function(vowel_name, model, data){
  
  # Dense smooth grid
  base_grid <- expand.grid(
    ethnicity = c("Black", "White", "Ambiguous"),
    sex       = c("female", "male"),
    norm_time = seq(0, 1, length.out = 100)
  )
  
  # Replicate for F1 and F2
  pred_grid <- base_grid %>%
    crossing(formant = c("F1", "F2")) %>%
    mutate(
      GroupFormant = interaction(ethnicity, sex, formant, sep = "_"),
      
      # freeze duration
      log_dur = mean(data$log_dur, na.rm = TRUE),
      
      # use one example level of each random effect
      speaker     = data$speaker[1],
      word        = data$word[1],
      prec_sound  = data$prec_sound[1],
      foll_sound  = data$foll_sound[1]
    )
  
  # Predict
  pred_grid$pred <- predict(model, newdata = pred_grid, type = "response")
  
  # Wide format → columns F1, F2
  pred_wide <- pred_grid %>%
    select(ethnicity, sex, norm_time, formant, pred) %>%
    pivot_wider(names_from = formant, values_from = pred) %>%
    arrange(ethnicity, sex, norm_time) %>%
    mutate(vowel = vowel_name)
  
  return(pred_wide)
}

## ---------------------
## APPLY TO ALL VOWELS
prediction_df <- map2_dfr(
  vowels,
  final_models,
  ~ make_predictions(.x, .y, df_long %>% filter(vowel == .x))
)

## ---------------------
## PLOT FUNCTION
plot_vowels <- function(df) {
  ggplot(df,
         aes(
           x = F2, y = F1,
           color = ethnicity,
           linetype = sex,
           group = interaction(ethnicity, sex)
         )) +
    geom_path(linewidth = 1) +
    geom_point(
      data = df %>% filter(norm_time == max(norm_time)),
      size = 2
    ) +
    scale_color_manual(values = c(
      "Black" = "#1b9e77",
      "White" = "#d95f02",
      "Ambiguous" = "#7570b3"
    )) +
    scale_linetype_manual(values = c(
      "female" = "solid",
      "male"   = "dashed"
    )) +
    theme_bw(base_size = 14) +
    theme(panel.grid = element_blank()) +
    labs(
      x = "F2 (Bark)",
      y = "F1 (Bark)",
      color = "Ethnicity",
      linetype = "Sex"
    )
}

## ---------------------
## FOUR REQUESTED PLOTS

# 1. /ɑ/
plot_ah <- plot_vowels(prediction_df %>% filter(vowel == "ɑ"))
plot_ah

# 2. /i/
plot_i <- plot_vowels(prediction_df %>% filter(vowel == "i"))
plot_i

# 3. facet-group: æ, ɪ, e
plot_group1 <- plot_vowels(prediction_df %>% filter(vowel %in% c("æ", "ɪ", "e"))) +
  facet_wrap(~ vowel, scales = "free")
plot_group1

# 4. facet-group: æi, ei
plot_group2 <- plot_vowels(prediction_df %>% filter(vowel %in% c("æi", "ei"))) +
  facet_wrap(~ vowel, scales = "free")
plot_group2





