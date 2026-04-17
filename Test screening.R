# Test screening 

#install.packages("AIscreenR")
#install.packages("remotes")
remotes::install_github("MikkelVembye/AIscreenR", build_vignettes = TRUE)

library(AIscreenR)
library(tidyverse)
library(future) 
library(readxl) 
library(tictoc)
library(usethis) 

options(scipen = 100)

# Open vignette to AIscreenR
#vignette("Using-GPT-API-Models-For-Screening", package = "AIscreenR")

# Handling API key -----------------
## Create you API key here: https://developers.openai.com/api/docs/quickstart#generate-an-api-key 

#set_api_key()
#usethis::edit_r_environ()

# Handling ris file data -----------------

## First we make the full dataset containing the records for the screening
## Here we assumed that the first human screening decisions has been made and are stored in two separate RIS files 

tic()
ris_dat_excl <- 
  read_ris_to_dataframe("Ris files/friends_excl.ris") |> # Add the path to your RIS file here
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    human_code = 0, #Tracking the human decision
    across(c(author, title, abstract), ~ na_if(., "")) # Handling missing values  
  )
toc()

ris_dat_incl <- 
  read_ris_to_dataframe("Ris files/friends_incl.ris") |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |>
  mutate(
    human_code = 1, #Tracking the human decision
    across(c(author, title, abstract), ~ na_if(., ""))
  )

## This data will be used for full-scale screening
friends_dat <- 
  bind_rows(ris_dat_excl, ris_dat_incl) |> 
  filter_out(is.na(abstract)) # Removing records with missing abstracts, as they cannot distort the screening performance
  
#saveRDS(friends_dat, "friends_dat.rds")

friends_dat <- readRDS("friends_dat.rds")

# Creating the test dataset for screening -----------------

set.seed(24042026) # Set seed to ensure reproducibility of the random sampling

excl_sample <- 
  ris_dat_excl |> 
  filter_out(is.na(abstract)) |> 
  AIscreenR::sample_references(100)

incl_sample <- 
  ris_dat_incl |> 
  filter_out(is.na(abstract)) |> 
  AIscreenR::sample_references(25)

test_dat <- 
  bind_rows(excl_sample, incl_sample) |> 
  mutate(
    studyid = 1:n()
  ) |> 
  relocate(studyid, .after = eppi_id)

test_dat

# Making prompit for screening -----------------

prompt1 <- "We are screening studies for a systematic literature review. 
The topic of the systematic review is the effect of the FRIENDS preventive programme on reducing
anxiety symptoms in children and adolescents. The FRIENDS programme is a 10-session manualised 
cognitive behavioural therapy (CBT) programme which
can be used as both prevention and treatment of child and youth anxiety. 
The study should focus exclusively on this topic and we are exclusively searching for studies with a treatment and a comparison group.
For each study, I would like you to assess:  1) Is the study about the FRIENDS preventive programme? 
2) Is the study estimating an effect between a treatment and control/comparison group?
"

prompt2 <- "We are screening titles and abstracts of studies for a systematic review about FRIENDS-family interventions for children/adolescents.

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

# Rate limit ------------------------
rate_limits <- rate_limits_per_minute(model = model_prizes$model)
rate_limits

# Approximate prize ------------------------------

app_prize <- 
  approximate_price_gpt(
    data = test_dat, # The dataset containing the studies to be screened
    prompt = c(prompt1, prompt2), # The prompt defined above
    studyid = eppi_id, # The column in the dataset that contains the study IDs
    title = title, # The column in the dataset that contains the study titles
    abstract = abstract,
    model = c("gpt-4o-mini", "gpt-5.1"), # The model to use for screening
    reps = 1
)

app_prize$price_data
app_prize$price_dollar

# Run test screening ------------------------------

tic() # Tracking the time it takes to run the screening
plan(multisession)

test_result_obj <- 
  AIscreenR::tabscreen_gpt(
    data = test_dat[c(1:10, 101:110),], # The dataset containing the studies to be screened
    prompt = c(prompt1, prompt2), # The prompt defined above
    studyid = eppi_id, # The column in the dataset that contains the study IDs
    title = title, # The column in the dataset that contains the study titles
    abstract = abstract, # The column in the dataset that contains the study abstracts
    model = c("gpt-4o-mini", "gpt-5.1"), # The model to use for screening (This is the default)
    reps = c(10, 1),
    overinclusive = TRUE # Indicate if the model should be include studies where it is uncertain (Default is TRUE)
) 

plan(sequential)
toc()

#save(test_result_obj, file = "screen objects/test_screening_results_incl_10reps.RData")
load("screen objects/test_screening_results_incl_10reps.RData")


performance <- 
  test_result_obj |> 
  AIscreenR::screen_analyzer(
    # Specify variable name for the human decisions 
    human_decision = human_code,
    # Specify whether only key results should be printed
    key_result = TRUE # Default
  )

performance 
