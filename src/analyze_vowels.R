library(tidyverse)
library(mgcv)
library(itsadug)

set.seed(123)

# Explicitly defined speakers
speakers <- c(paste0('BF_', 1:10), paste0('BM_', 1:7),
              paste0('WF_', 1:10), paste0('WM_', 1:7))

# Carefully selected vowel-word lists (no nasals, no initial clusters, CVC or CVCC only)
vowel_word_list <- list(
  'i'  = c('beat','heat','seat','feet','sheet','peat','keet','teeth','keep','deep'),
  'eɪ' = c('bait','hate','fate','date','late','gate','rate','kate','cape','tape'),
  'ɪ'  = c('bit','hit','sit','fit','ship','tip','lip','chip','kit','pit'),
  'ɛ'  = c('bet','set','pet','let','get','jet','vet','wet','yet','debt'),
  'æ'  = c('bat','hat','sat','fat','cat','rat','pat','chat','tat','vat'),
  'u'  = c('boot','hoot','suit','shoot','root','toot','loot','coot','hoop','coop'),
  'ɑ'  = c('cot','hot','pot','got','lot','dot','shot','tot','rot','yacht')
)

# Generate synthetic vowel dataset explicitly including vowel trajectories (normalized time)
data <- expand.grid(
  speaker = speakers,
  vowel = names(vowel_word_list),
  word_num = 1:10,
  normalized_time = seq(0, 100, length.out = 10)
) %>%
  mutate(
    word = map2_chr(vowel, word_num, ~ vowel_word_list[[.x]][.y]),
    race = ifelse(grepl('^B', speaker), 'Black', 'White'),
    sex = ifelse(grepl('F_', speaker), 'female', 'male')
  ) %>%
  rowwise() %>%
  mutate(
    # Explicit realistic base formants (F1, F2 in Bark)
    F1_base = c('i'=3.5,'eɪ'=4.5,'ɪ'=4.7,'ɛ'=5.2,'æ'=6.5,'u'=3.7,'ɑ'=6.0)[vowel] + ifelse(sex=='male', 0.3, 0),
    F2_base = c('i'=13.5,'eɪ'=12.5,'ɪ'=12.8,'ɛ'=12.0,'æ'=11.5,'u'=7.0,'ɑ'=8.5)[vowel] + ifelse(race=='Black', -0.2, 0.2),
    # Explicit vowel trajectory simulation
    F1_bark = rnorm(1, mean=F1_base + (normalized_time/100)*0.5, sd=0.2),
    F2_bark = rnorm(1, mean=F2_base - (normalized_time/100)*0.5, sd=0.4)
  ) %>%
  ungroup()

# Explicit factor coding
data <- data %>%
  mutate(across(c(speaker, race, sex, vowel, word), factor))

# Analysis loop explicitly modeling full interaction smooth (race × sex)
for(v in unique(data$vowel)){
  
  cat("\n\n==================================\n")
  cat("Now analyzing vowel:", v, "\n")
  cat("==================================\n\n")
  
  vowel_data <- filter(data, vowel == v)
  
  # GAMMs explicitly with race × sex interaction and random slopes/intercepts for speaker
  model_F1 <- bam(F1_bark ~ race * sex +
                    s(normalized_time, by=interaction(race, sex), k=4) +
                    s(normalized_time, speaker, bs="fs", m=1), # random slopes & intercepts
                  data=vowel_data, method="fREML")
  
  model_F2 <- bam(F2_bark ~ race * sex +
                    s(normalized_time, by=interaction(race, sex), k=4) +
                    s(normalized_time, speaker, bs="fs", m=1),
                  data=vowel_data, method="fREML")
  
  # Model summaries explicitly printed
  cat("\n--- Updated F1 Model Summary for Vowel", v, "---\n")
  print(summary(model_F1))
  
  cat("\n--- Updated F2 Model Summary for Vowel", v, "---\n")
  print(summary(model_F2))
  
  # Predictions explicitly excluding random effects
  new_data <- expand.grid(
    normalized_time = seq(0,100,length.out=20),
    race = factor(c("Black", "White")),
    sex = factor(c("female", "male"))
  ) %>%
    mutate(
      interaction_term = interaction(race, sex),
      F1_pred = predict(model_F1, newdata=., exclude=c("s(normalized_time,speaker)"), newdata.guaranteed=TRUE),
      F2_pred = predict(model_F2, newdata=., exclude=c("s(normalized_time,speaker)"), newdata.guaranteed=TRUE)
    )
  
  # Plot formant trajectories clearly showing interaction explicitly
  plot <- ggplot(new_data, aes(x=F2_pred, y=F1_pred, color=sex, linetype=race, group=interaction(race,sex))) +
    geom_path(arrow=arrow(angle=20,length=unit(0.1,"inches"),type="closed"), linewidth=1) +
    scale_x_reverse(name="F2 (Bark)") +
    scale_y_reverse(name="F1 (Bark)") +
    theme_classic() +
    ggtitle(paste("Formant trajectory (Race × Sex) for vowel /", v, "/", sep="")) +
    theme(legend.position="right")
  
  print(plot)
}
