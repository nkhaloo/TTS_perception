# TTS_perception

This repository contains the code used in Khaloo, Holliday, and Creel (Accepted: Interspeech 2026). Perception and variability of African American English in TTS speech. 
This project tests whether people can converge on a perception of race in when presented with various Text-to-speech (TTS) voices. It also tests whether people assing anthropomorphic traits associated with race to said TTS voices. 

## SRC 
The `src` folder contains the following scripts:

`analyze_vowels.R`: Visualizes and analyzes the acoustic data for vowels. 

`random_sample_voices.R`: Radomly samples avaialble TTS voices hosted on Easy-easy AI. 

## Experiment
The `experiment` folder contains the audio for the attention check, as well as the .html file used to host the online experiment. 

## Results

### Data Summary
- **Total responses:** 12,201
- **Participants:** 144 (150 recruited; 6 dropped for incomplete responses)
- **Unique speakers/stimuli:** 32 (8 Black Male, 8 Black Female, 8 White Male, 8 White Female)
- **Collection date:** 2025-10-31

The full responses are stored in `data/survey_results/survey_results.csv`.

### Perceptual Race Labels
Race labels were derived via k-means clustering on the percentage of times each voice was rated as "Black" (%RB), yielding three clusters:
- **White**: ~0–20% rated Black
- **Black**: ~50–100% rated Black
- **Ambiguous**: ~20–50% rated Black

A general bias toward rating voices as White was observed. Notably, 5 of 9 Ambiguous voices were platform-labeled as Black Female, indicating listeners had more difficulty identifying Black Female voices — consistent with findings from human speech research.

Gender agreement between platform-assigned labels and participant ratings was high: **Male: 98.6%**, **Female: 93.6%**.

### Personality Results
Linear mixed-effects models were fit per trait with fixed effects for perceptually assigned race, participant ethnicity, and human-likeness.

- **Female-sounding voices**: Perceptually assigned race was **not** a significant predictor of any personality trait.
- **Male-sounding voices**: Voices rated as Black were rated significantly lower than voices rated as White on:
  - Pleasantness (β = −0.64, *p* < .05)
  - Professionalism (β = −1.09, *p* < .05)
  - Trustworthiness (β = −0.53, *p* < .05)
  - Competence (β = −0.67, *p* < .05)
  - No significant differences were found for friendliness or funniness.
- **Human-likeness** was a significant positive predictor of all personality traits across both perceived genders (*p* < .01).

### Acoustic Classification
A Gradient Boosted Decision Tree (XGBoost) classifier was trained on acoustic features to distinguish voices rated as Black vs. White (male-rated voices only).

- **Full model** (61 features): average cross-validated accuracy of **65%**
- **Reduced model** (top 15 features): average cross-validated accuracy of **66%**

Top acoustic features by gain (Figure 2 in paper): Residual H1\* onset, F1 midpoint, F1 onset, Formant Dispersion onset, H2kHz\*–H5kHz\* offset.

Voices rated as Black showed:
- **Lower** Residual H1\* (most robust effect), CPP, H2\*–H4\*, and H2kHz\*–H5kHz\*
- **Higher** F4

Vowel-specific differences in F1 (~100 Hz) were observed for /æ/, /ɛ/, and /i/, and in both F1 and F2 for /æɪ/, consistent with known MAE–AAE vowel differences in human speakers.

