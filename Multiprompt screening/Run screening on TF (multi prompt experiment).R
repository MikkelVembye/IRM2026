

################################################################################
################# RUNNING THE SCREENING FOR TESTING FREQUENCY ##################
################################################################################

#########################
# Loading packages 
#########################

#install.packages("remotes")
#remotes::install_github("MikkelVembye/AIscreenR", force = TRUE, build_vignettes = TRUE)

#browseVignettes("AIscreenR")
#vignette("Using-GPT-API-Models-For-Screening", package = "AIscreenR")


library(tidyverse) # Data manipulation, etc. 
library(purrr) # For loops
library(AIscreenR) # Used to run the screen via tabscreen_gpt()
library(future) # To run screening in parallel 
library(furrr) # To run screening in parallel 
library(dplyr) # Data manipulation, etc.


################################################################################
# Analyze screenings (the analysis presented in the Psych Methods paper)
################################################################################

tf_exp_dat_res <- readRDS("Multiprompt screening/tf_exp_dat_res.rds")

tf_res_func <- function(min_n_prompts){
   
   tf_res_wide <- 
      tf_exp_dat_res |> 
      pivot_wider(
        names_from = promptid,
        values_from = prompt:n_mis_answers 
      ) |> 
      select(author:abstract, final_decision_gpt_num_1:final_decision_gpt_num_6) |> 
      rowwise() |> 
      mutate(
        n_prompt_incl = sum(c_across(final_decision_gpt_num_1:final_decision_gpt_num_6), na.rm = TRUE),
        gpt_include = if_else(n_prompt_incl >= min_n_prompts, 1, 0) 
      ) |> 
      ungroup()
    
    tf_test_perform <- 
      tf_res_wide |> 
      summarise(
        min_n_prompt_included = min_n_prompts,
        TP = sum(gpt_include == 1 & human_code == 1, na.rm = TRUE),
        TN = sum(gpt_include == 0 & human_code == 0, na.rm = TRUE),
        FN = sum(gpt_include == 0 & human_code == 1, na.rm = TRUE),
        FP = sum(gpt_include == 1 & human_code == 0, na.rm = TRUE),
        recall = TP / (TP + FN),
        spec = TN / (TN + FP),
        raw_agree = (TP + TN)/(TP + TN + FN + FP),
        bacc = (recall + spec) / 2
      ) |> 
      ungroup()
    
    tf_test_perform
}

# Results presented in Table 3 using multi-prompt screening
map(3:6, .f = tf_res_func) |> list_rbind()

