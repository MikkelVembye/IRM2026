# Full screening

## worst case scenario having no internet
## load("screen objects/friends_worstcase.RData")

library(AIscreenR)
library(tidyverse)
library(future) 
library(tictoc)

# Options
options(scipen = 100)
#options(pillar.sigfig = 4) # ensure tibble include 4 digits
#options(tibble.width = Inf)
options(dplyr.print_min = 100)
options(dplyr.summarise.inform = FALSE) # Avoids summarize info from tidyverse


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

## Timing (not strictly needed just nice information to show you)
t <- toc()
elapsed <- t$toc - t$tic
cat(sprintf("%d minutes and %d seconds\n", as.integer(elapsed %/% 60), as.integer(elapsed %% 60)))

## Save the data
save(result_obj, file = "screen objects/friends_screening_full.RData")

result_dat <- result_obj$answer_data

## Exact model used and run date for RAISE
model_info <- result_dat$submodel |> unique()
model_info

## Time 
run_date <- result_dat$run_date |> unique()
run_date

performance <- 
  result_obj |> 
  AIscreenR::screen_analyzer(
    # Specify variable name for the human decisions 
    human_decision = human_code,
    # Specify whether only key results should be printed
    key_result = TRUE # Default
  )

performance 

## Re-screen discrepancies to get descriptions 

discrepancies_dat <- 
  result_dat |>
  filter(decision_binary != human_code)

tic()
plan(multisession)

result_obj_with_descriptions <- 
  AIscreenR::tabscreen_gpt(
    data = discrepancies_dat, # The dataset containing the studies to be screened
    prompt = prompt, # The prompt defined above
    studyid = eppi_id, # The column in the dataset that contains the study IDs
    title = title, # The column in the dataset that contains the study titles
    abstract = abstract, # The column in the dataset that contains the study abstracts
    model = "gpt-5.1", # The model to use for screening
    reps = 1, # Number of repetitions (set to 1 for this comparison)
    decision_description = TRUE, # Whether to include the model's reasoning in the output (set to FALSE for this comparison)
    overinclusive = TRUE, # Whether to overinclude studies (set to FALSE for this comparison)
) 
plan(sequential)
toc()

agreement_2round <- 
  result_obj_with_descriptions$answer_data |> 
  filter(decision_binary == human_code) 

final_result_dat <- 
  result_dat |> 
  rows_update(
    select(agreement_2round, -detailed_description), 
    by = "eppi_id"
  )

#final_result_dat |> 
#  filter(eppi_id %in% agreement_2round$eppi_id) |> 
#  View()

# Making screening report for disagreements

final_disagreements <- 
  result_obj_with_descriptions$answer_data |> 
  filter(decision_binary != human_code) 

report(
  data = final_disagreements,
  studyid = eppi_id,
  title = title,
  abstract = abstract,
  gpt_answer = detailed_description,
  human_code = human_code,
  final_decision_gpt_num = decision_binary,
  file = "full_disagreement_report",
  format = "docx",
  document_title = "Screening Disagreement Review",
  open = TRUE,
  directory = paste0(getwd(), "/Screening reports")
)


# Preparing for EPPI - relevant for you? -----------------

included_ids <- 
  final_result_dat |> 
  filter(decision_binary == 1) |> 
  pull(eppi_id) |> 
  paste(collapse = ", ")

# Generate RIS file with included studies

raw_friends_dat <- readRDS("Data/raw_friends_dat.rds")

included_dat_for_ris <- 
  raw_friends_dat |> 
  filter(
    eppi_id %in% final_result_dat$eppi_id[final_result_dat$decision_binary == 1]
  ) |> 
  select(-human_code) # Removing human code to avoid confusion 


save_dataframe_to_ris(
  included_dat_for_ris,
  file = "Ris files/final_included_by_gpt_studies.ris"
)

#save(
#  result_obj, 
#  result_obj_with_descriptions,
#  file = "screen objects/friends_worstcase.RData"
#)