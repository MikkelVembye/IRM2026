# Quick example - friends

# install.packages("AIscreenR")

library(AIscreenR)
library(tidyverse)
library(future) 
library(readxl) 
library(tictoc)
library(quarto)

excl_path <- system.file("extdata", "friends_dat.rds", package = "AIscreenR")

friends_dat <- readRDS(excl_path) |> select(-include)

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
2) Is a single group/arm study of a FRIENDS-family intervention with no comparison/control group (e.g., pre-post design with only a FRIENDS arm).
3) Discussion/review/conceptual paper with no empirical study described.
4) Study explicitly focuses ONLY on non-symptom outcomes (e.g., social validity, acceptability, satisfaction, implementation fidelity, teacher/student attendance) WITHOUT mentioning measurement of anxiety or internalizing symptoms.
5) Outcomes are only non-symptom constructs (e.g., social skills/SEL, cooperation) with NO indication that anxiety/internalizing symptoms are being measured.
"

tic()
plan(multisession)

result_obj <- 
  AIscreenR::tabscreen_gpt(
    data = friends_dat, # The dataset containing the studies to be screened
    prompt = prompt, # The prompt defined above
    studyid = studyid, # The column in the dataset that contains the study IDs
    title = title, # The column in the dataset that contains the study titles
    abstract = abstract, # The column in the dataset that contains the study abstracts
    model = "gpt-4o-mini", # The model to use for screening
    reps = 1, # Number of repetitions (set to 1 for this comparison)
    decision_description = FALSE, # Whether to include the model's reasoning in the output (set to FALSE for this comparison)
    overinclusive = TRUE, # Whether to overinclude studies (set to FALSE for this comparison)
) 
plan(sequential)
toc()

answer_dat <- result_obj$answer_data

# Exact model used and run date for RAISE
model_info <- answer_dat$submodel |> unique()
model_info

performance <- 
  result_obj |> 
  AIscreenR::screen_analyzer(
    # Specify variable name for the human decisions 
    human_decision = human_code,
    # Specify whether only key results should be printed
    key_result = TRUE # Default
  )

performance 

# Get description of discrepancies

discrepancies_dat <- 
  answer_dat |>
  filter(decision_binary != human_code)

tic()
plan(multisession)

result_obj_with_descriptions <- 
  AIscreenR::tabscreen_gpt(
    data = discrepancies_dat, # The dataset containing the studies to be screened
    prompt = prompt, # The prompt defined above
    studyid = studyid, # The column in the dataset that contains the study IDs
    title = title, # The column in the dataset that contains the study titles
    abstract = abstract, # The column in the dataset that contains the study abstracts
    model = "gpt-4o-mini", # The model to use for screening
    reps = 1, # Number of repetitions (set to 1 for this comparison)
    decision_description = TRUE, # Whether to include the model's reasoning in the output (set to FALSE for this comparison)
    overinclusive = TRUE, # Whether to overinclude studies (set to FALSE for this comparison)
) 
plan(sequential)
toc()

# download quarto CLI at https://quarto.org/docs/get-started/ to make it work 

disagreements2 <- 
  result_obj_with_descriptions$answer_data |> 
  mutate(
    # More aggressive text cleaning
    detailed_description = str_replace_all(detailed_description, "[\\x00-\\x1F\\x7F-\\x9F]", ""),
    detailed_description = iconv(detailed_description, from = "UTF-8", to = "UTF-8", sub = ""),
    title = str_replace_all(title, "[\\x00-\\x1F\\x7F-\\x9F]", ""),
    title = iconv(title, from = "UTF-8", to = "UTF-8", sub = ""),
    abstract = str_replace_all(abstract, "[\\x00-\\x1F\\x7F-\\x9F]", ""),
    abstract = iconv(abstract, from = "UTF-8", to = "UTF-8", sub = "")
  )

report(
  data = disagreements2,
  studyid = studyid,
  title = title,
  abstract = abstract,
  gpt_answer = detailed_description,
  human_code = human_code,
  final_decision_gpt_num = decision_binary,
  file = "disagreement_report",
  format = "html",
  document_title = "Screening Disagreement Review",
  open = TRUE
)

