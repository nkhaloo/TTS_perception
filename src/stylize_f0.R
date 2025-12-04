library(dplyr)
library(signal)
library(pracma)
library(purrr)
library(tidyr)
library(readr)

#----------------------------------------------------------
# 1. Octave jump correction (Praat-like)
#----------------------------------------------------------
kill_octave_jumps <- function(f0){
  f02 <- f0
  f0_before <- mean(f02, na.rm = TRUE)
  
  for (i in 2:length(f02)){
    if (is.na(f02[i]) || is.na(f02[i-1])) next
    ratio <- f02[i] / f02[i-1]
    if (!is.finite(ratio)) next
    
    if (ratio > 1.9) {
      f02[i] <- f02[i] / 2
    } else if (ratio < 0.51) {
      f02[i] <- f02[i] * 2
    }
  }
  
  f0_after <- mean(f02, na.rm = TRUE)
  
  list(
    f0 = f02,
    jumpkilleffect = f0_before / f0_after
  )
}

#----------------------------------------------------------
# 2. Smooth (Savitzky–Golay)
#----------------------------------------------------------
smooth_f0 <- function(f0, window_ms=10, fs=1000){
  win <- round(window_ms * fs / 1000)
  if (win %% 2 == 0) win <- win + 1
  sgolayfilt(f0, p=2, n=win)
}

#----------------------------------------------------------
# 3. Interpolate missing values
#----------------------------------------------------------
interpolate_f0 <- function(f0){
  x <- 1:length(f0)
  good <- which(!is.na(f0))
  approx(x[good], f0[good], x, rule = 2)$y
}

#----------------------------------------------------------
# 4. Stylize using Douglas–Peucker in semitones (Praat style)
#----------------------------------------------------------
stylize_f0 <- function(f0, fs=1000, st=2){
  
  finite_vals <- f0[is.finite(f0) & f0 > 0]
  if (length(finite_vals) < 3) return(f0)
  
  ref <- mean(finite_vals)
  
  to_st <- function(x){
    if (is.na(x) || x <= 0) return(NA)
    12 * log2(x / ref)
  }
  
  st_vals <- sapply(f0, to_st)
  t <- seq(0, length(f0)-1) / fs
  
  dp <- function(t, y, eps){
    if (length(y) < 3) {
      mask <- rep(FALSE, length(y))
      mask[c(1, length(y))] <- TRUE
      return(mask)
    }
    
    t1 <- t[1]; t2 <- t[length(t)]
    y1 <- y[1]; y2 <- y[length(y)]
    
    pred <- y1 + (y2 - y1)*(t - t1)/(t2 - t1 + 1e-9)
    dist <- abs(y - pred)
    
    if (length(dist) == 0 || all(is.na(dist))) {
      mask <- rep(FALSE, length(y))
      mask[c(1, length(y))] <- TRUE
      return(mask)
    }
    
    idx <- which.max(dist)
    if (length(idx) == 0 || is.na(dist[idx])) {
      mask <- rep(FALSE, length(y))
      mask[c(1, length(y))] <- TRUE
      return(mask)
    }
    
    if (dist[idx] < eps) {
      mask <- rep(FALSE, length(y))
      mask[c(1, length(y))] <- TRUE
      return(mask)
    }
    
    left  <- dp(t[1:idx], y[1:idx], eps)
    right <- dp(t[idx:length(t)], y[idx:length(y)], eps)
    
    c(left, right[-1])
  }
  
  mask <- dp(t, st_vals, eps = st)
  approx(t[mask], f0[mask], t, rule = 2)$y
}

#----------------------------------------------------------
# 5. Sample N values (Praat spaced)
#----------------------------------------------------------
sample_f0 <- function(f0, fs=1000, N=20){
  dur <- length(f0) / fs
  times <- seq(0, dur, length.out = N+2)[2:(N+1)]
  t <- seq(0, length(f0)-1) / fs
  approx(t, f0, times, rule = 2)$y
}

#----------------------------------------------------------
# 6. Master: returns 20 rows per interval
#----------------------------------------------------------
process_interval <- function(f0_interval, start, end,
                             kill_jumps=TRUE, smooth_bw=10,
                             stylize_st=2, N=20){
  
  f0 <- as.numeric(f0_interval)
  f0[f0 == 0] <- NA
  
  if (sum(is.finite(f0)) < 3) {
    return(
      tibble(
        steptime = rep(NA, N),
        stepnumber = 1:N,
        f0 = rep(NA, N),
        jumpkilleffect = NA
      )
    )
  }
  
  if (kill_jumps){
    kj <- kill_octave_jumps(f0)
    f0 <- kj$f0
    jumpkilleffect <- kj$jumpkilleffect
  } else {
    jumpkilleffect <- NA
  }
  
  f0 <- interpolate_f0(f0)
  f0 <- smooth_f0(f0, window_ms=smooth_bw)
  f0 <- stylize_f0(f0, st=stylize_st)
  
  f0_sampled <- sample_f0(f0, N=N)
  
  dur <- end - start
  steptime <- seq(start, end, length.out = N+2)[2:(N+1)]
  
  tibble(
    steptime = steptime,
    stepnumber = 1:N,
    f0 = f0_sampled,
    jumpkilleffect = jumpkilleffect
  )
}

#----------------------------------------------------------
# 7. Load raw Praat-generated F0 CSV
#----------------------------------------------------------
df <- read_csv("/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/f0_results/output_f0.csv")

df <- df %>% mutate(sF0 = ifelse(sF0 == 0, NA, sF0))

df <- df %>%
  mutate(interval_id = paste(Filename, seg_Start, seg_End, sep="_"))

#----------------------------------------------------------
# 8. Process each interval → Praat long format (20 rows per interval)
#----------------------------------------------------------
output <- df %>%
  group_by(Filename, interval_id, seg_Start, seg_End) %>%
  summarise(
    interval_df = list(process_interval(sF0, seg_Start[1], seg_End[1])),
    .groups="drop"
  ) %>%
  unnest(interval_df) %>%
  rename(
    filename = Filename,
    interval_label = interval_id,
    start = seg_Start,
    end = seg_End
  ) %>%
  select(
    filename,
    interval_label,
    start,
    end,
    steptime,
    stepnumber,
    f0,
    jumpkilleffect
  )

#----------------------------------------------------------
# 9. Filter out bad intervals (required for DTW clustering)
#----------------------------------------------------------
output_filtered %>%
  group_by(interval_label) %>%
  summarise(n_valid = sum(is.finite(f0))) %>%
  dplyr::filter(n_valid == 0)

#----------------------------------------------------------
# 10. Save CLEAN contour CSV for the clustering app
#----------------------------------------------------------
write_csv(output_filtered,
          "/Users/noahkhaloo/Desktop/TTS_perception/data/acoustic_data/f0_results/f0_stylized.csv"
)






