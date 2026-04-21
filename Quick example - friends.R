# Quick example - friends

library(AIscreenR)
library(tidyverse)
library(future) 
library(tictoc)


friends_dat <- readRDS("Data/friends_dat.rds")

prompt <- "We are screening titles and abstracts of studies for a systematic review about FRIENDS-family interventions for children/adolescents.

INCLUDE (decision = 1) if ALL are true:
A) Intervention is FRIENDS-family OR clearly derived from it:
   - Explicitly named FRIENDS / FRIENDS for Life / Fun FRIENDS, OR
   - Explicitly described as an adaptation/translation/derivative of FRIENDS-family, OR
   - Described as a school-/group-based CBT anxiety prevention/resilience program that is 'based on' or 'informed by' FRIENDS/Fun FRIENDS (treat this as FRIENDS-family unless the abstract clearly indicates it is unrelated).
B) The study measures, evaluates, or reports on anxiety, internalizing symptoms, OR social-emotional/coping outcomes:
   - The abstract explicitly mentions anxiety/depression outcomes or anxiety reduction (e.g., 'decreased anxiety', 'anxiety symptoms improved', 'anxiety outcomes'), OR
   - The abstract indicates anxiety/depression/internalizing/emotional coping skills are measured, OR
   - The FRIENDS intervention is delivered with assessment of emotional/social/coping competencies, OR
   - The intervention is explicitly described as targeting anxiety/depression reduction (measurement specifics unclear from abstract).

EXCLUDE if ANY are true:
1) Not FRIENDS-family and not clearly derived from FRIENDS-family (mere generic CBT with no FRIENDS link).
2) Is a single group/arm study of a FRIENDS-family intervention with no mentioning of a comparison/control group (e.g., pre-post design with only a FRIENDS arm should be excluded).
3) Discussion/review/conceptual paper with no empirical study described.
4) Study explicitly focuses ONLY on non-symptom outcomes (e.g., social validity, acceptability, satisfaction, implementation fidelity, teacher/student attendance) WITHOUT mentioning measurement of anxiety or internalizing symptoms.
5) Outcomes are only non-symptom constructs (e.g., social skills/SEL, cooperation) with NO indication that anxiety/internalizing symptoms are being measured.
"

tic() # Tracking the time it takes to run the screening
plan(multisession, workers = 10)

result_obj <- 
  AIscreenR::tabscreen_gpt(
    data = friends_dat, # The dataset containing the studies to be screened
    prompt = prompt, # The prompt defined above
    studyid = eppi_id, # The column in the dataset that contains the study IDs
    title = title, # The column in the dataset that contains the study titles
    abstract = abstract, # The column in the dataset that contains the study abstracts
    model = "gpt-4o-mini", # The model to use for screening (This is the default)
    overinclusive = TRUE, # Indicate if the model should be include studies where it is uncertain (Default is TRUE)
) 

plan(sequential)

# Timing (not strictly needed just nice information to show you)
t <- toc()
elapsed <- t$toc - t$tic
cat(sprintf("%d minutes and %d seconds\n", as.integer(elapsed %/% 60), as.integer(elapsed %% 60)))

save(result_obj, file = "screen objects/friends_screening_full.RData")

performance <- 
  result_obj |> 
  AIscreenR::screen_analyzer(
    # Specify variable name for the human decisions 
    human_decision = human_code,
    # Specify whether only key results should be printed
    key_result = TRUE # Default
  )

performance 
