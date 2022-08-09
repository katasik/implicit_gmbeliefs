#Processing labjs 

#Load required libraries 
library(Hmisc)
library(RSQLite)
library(jsonlite)
library(tidyverse)
library(janitor)
library(pacman)
library(knitr)
library(kableExtra)
library(mousetrap)
library(readr)
library(stringr)



###we opened the labjs data with these steps but the raw data is too big and it was not possible to upload on github
# 
# # load labjs data
# iqmsmtpep<- read.csv("data/iqmsmtpep.csv")
# 
# #renaming variables
# names(iqmsmtpep)[names(iqmsmtpep)=="id"] <- "running_id"
# names(iqmsmtpep)[names(iqmsmtpep)=="match_id"] <- "matchid"
# names(iqmsmtpep)[names(iqmsmtpep)=="result"] <- "labjs-data"
# names(iqmsmtpep)[names(iqmsmtpep)=="created"] <- "date"
# 
# 
# iqmsmtpep_raw <- iqmsmtpep %>% type_convert()
# 
# #removing tests by our IT guy, because the script doesn't run otherwise
# iqmsmtpep_raw<-iqmsmtpep_raw[-c(1, 2), ]
# 
# 
# # columns in this file contains the
# # JSON-encoded data from lab.js
# labjs_column <- 'labjs-data'
# 
# # Unpack the JSON data and discard the compressed version
# iqmsmtpep_raw %>%
#     # Provide a fallback for missing data
#     mutate(
#         !!labjs_column := recode(.[[labjs_column]], .missing='[{}]')
#     ) %>%
#     # Expand JSON-encoded data per participant
#     group_by_all() %>%
#     do(
#         fromJSON(.[[labjs_column]], flatten=T)
#     ) %>%
#     ungroup() %>%
#     # Remove column containing raw JSON
#     dplyr::select(-matches(labjs_column)) -> iqmsmtpep_opened
# 
# 
# 
# # match labjs and qualtrics ids
# data_pep <- subset(iqmsmtpep_opened, matchid %in% explicit_raw$matchid_pep)



#saveRDS(data_pep, file = "iqms_mtpep_raw.rds")

iqms_mtpep_raw <- readRDS(file = "data/iqms_mtpep_raw.rds") %>% 
  #rearranging the datatable and getting rid of metadata by labjs
  select(running_id:word6, word7:word12, duration1:duration6, duration7:duration11, 
         probe:trial_type, response:mouseouts)



######From this point on, as pre-registered, I use Jamie Cummin's related code from Cummins & De Houwer, 2021


#"Initial cleaning of MT-PEP data

#Involves removing superfluous rows from the dataframe, some basic restructuring and mutating of other columns


mt_pep_cleaned_df <- iqms_mtpep_raw %>%
  
  # filter out irrelevant components of trial
  filter(!str_detect(sender, "word"),
         !str_detect(sender, "intro"),
         !str_detect(sender, "blank"),
         !str_detect(sender, "start"),
         !str_detect(sender, "cross"),
         !str_detect(sender, "trial"),
         !str_detect(sender, "intro"),
         !str_detect(sender, "block"),
         !str_detect(sender, "end"),
         !str_detect(sender, "break")) %>% 
  
  # separate sender column into block, trial, and component columns
  # will return NA for questionnaire-based trials, this is no issue
  separate(sender_id, into = c("block", "trial_num", "component"), sep = "_") %>% 
  
  # mutate block variable
  mutate(# reformat trial number variable
    trial_num = as.numeric(trial_num) + 1) %>% 
  
  # remove practice trials
  filter(block == 6) %>% 
  
  # create new column to indicate slow responding
  mutate(too_slow = lead(ended_on), 
         too_slow = case_when(too_slow == "timeout" ~ 1,
                              TRUE ~ 0),
         too_slow = as.numeric(too_slow)) %>% 
  
  # remove too slow component rows
  filter(!str_detect(sender, "too_slow")) %>% 
  
  # remove extraneous probe/catch components
  filter(sender == "probe" & !(is.na(duration)) |
           sender == "catch" & !(is.na(duration))) %>% 
  
  # select only relevant variables
  dplyr::select(matchid, trial_num, sender, duration, probe, reversed, 
                correct_response, spelling, response, correct, too_slow, 
                timestamps, xpos, ypos) %>% 
  
  # rename some variables for clarity
  dplyr::rename(required_response = correct_response) %>%
  dplyr::rename(rt = duration,
                trial_type = sender) %>%
  
  # recode correct_response column,
  # then mutate correct response so all catch responses are true
  mutate(correct = case_when(correct == TRUE ~ 1, 
                             TRUE ~ 0)) %>% 
  
  # remove participants who did not complete all trials
  group_by(matchid) %>% 
  filter(n() >= 72) %>% 
  ungroup() %>% 
  
  # remove c( from beginning and ) from end of mousetracking columns
  mutate(xpos       = str_remove_all(xpos, "^[a-z]{1}[(]"),
         ypos       = str_remove_all(ypos, "^[a-z]{1}[(]"),
         timestamps = str_remove_all(timestamps, "^[a-z]{1}[(]"),
         xpos       = str_sub(xpos, 1, str_length(xpos) - 1),
         ypos       = str_sub(ypos, 1, str_length(ypos) - 1),
         timestamps = str_sub(timestamps, 1, str_length(timestamps) - 1)) %>%
#this part shows a warning message because xpos, ypos and timestamps are lists, but it still achieves what we want
 
  
  # create belief column to identify whether belief to be expressed was changeable or unchangeable
  # 1 meaning that your intelligence in unchangeable
  mutate(belief = case_when(probe == "WAHR" & reversed == 0 ~ 0,
                            probe == "WAHR" & reversed == 1 ~ 1,
                            probe == "FALSCH" & reversed == 0 ~ 1,
                            probe == "FALSCH" & reversed == 1 ~ 0,
                            probe == "??WAHR ODER FALSCH??" & reversed == 0 ~ 0,
                            probe == "??WAHR ODER FALSCH??" & reversed == 1 ~ 1
  )) %>%
  
  distinct(matchid, trial_num, .keep_all = TRUE)

#write_csv(mt_pep_cleaned_df, "mt_pep_cleaned_df.csv")



## Recentering and normalisation

#Here we recenter the time of each trial such that each first timestamp begins at 0ms.

#We additionally normalise the x and y positions of the mouse at each timestamp from a range of 0 to 1. In this way, measurements which were taken in different resolutions are directly comparable.

#Here we also remove trajectories where x or y coordinates began recording outside of pre-defined acceptance bounds. That is, we should expect that x coordinates begin in the middle of the screen (0.5), which y coordinates should begin at the bottom of the screen (which is actually registered as a larger number than if it were at the top of the screen, 1).

## Recenter timestamps


# define length of vector that should be created - should be long enough that each measure for each trial has a separate column
vec_len <- 750

recenter_timestamps_df <- mt_pep_cleaned_df %>%
  
  # separate each xpos value into an individual column
  separate(timestamps, into = c(as.character(1:vec_len)), 
           sep = ",", remove = TRUE) %>% 
  filter(is.na(`749`)) %>% 
  dplyr::select(-3:-9, -xpos, -ypos, -too_slow) %>% 
  dplyr::select(matchid, trial_num, `1`:as.character(vec_len)) %>% 
  
  # turn into long form, ensure data are numeric
  gather(measurement, mouse_time, `1`:as.character(vec_len)) %>% 
  mutate(measurement= as.numeric(measurement),
         mouse_time = as.numeric(mouse_time)) %>% 
  #this shows an error message because of the NAs but it doesn't influence our table
  
  # calculate starting time for each participant
  mutate(start_time = ifelse(measurement == 1, mouse_time, NA)) %>% 
  arrange(matchid, trial_num, measurement) %>% 
  
  # recenter the timestamps
  fill(start_time) %>% 
  mutate(recentered_mouse_time = mouse_time - start_time) %>% 
  
  # select relevant columns
  dplyr::select(matchid, trial_num, measurement, recentered_mouse_time) %>%
  
  distinct(.keep_all = TRUE) %>% 
  
  
  # turn back into wide form
  spread(measurement, recentered_mouse_time, drop = FALSE) %>% 
  
  # remove trials where no measurement was taken at point 1
  filter(!is.na(`1`)) %>% 
  mutate_all(funs(ifelse(is.na(.), "", .))) %>% 
  
  # bring it all back together
  unite(recentered_timestamps, `1`:as.character(vec_len), sep = ",") %>% 
  # remove excess commas 
  mutate(recentered_timestamps = str_remove(recentered_timestamps, "\\,,[^.]*$")) 


## trajectories


# function for normalising coordinates
coordinate_normalisation <- function(df,
                                     # the dimension of the mouse movement of interest
                                     mouse_dimension = "xpos",
                                     
                                     # the number of columns required
                                     vector_length = 101, 
                                     
                                     # expected value: 0.5 for x, 1 for y
                                     expected_start_value = .5,
                                     
                                     # the acceptable degree of deviation that the expected start value can have to be a valid trial
                                     acceptance_bound = .1,
                                     
                                     # name for the new normalised column
                                     normalised_column_name = "xpos") {
  
  df_1 <- df %>%
    
    # separate each xpos value into an individual column
    separate(mouse_dimension, into = c(as.character(1:vector_length)), 
             sep = ",", remove = TRUE) %>%
    
    filter(is.na(`749`))
  
  str_length <- df_1 %>%
    dplyr::select(-1:-14) %>%
    ncol(.) %>%
    as.character()
  
  
  df_2 <- df_1 %>% 
    dplyr::select(matchid, trial_num, `1`:str_length) %>%
    
    # turn into long form, ensure data are numeric
    gather(measurement, mouse_pos, `1`:str_length) %>%
    mutate(mouse_pos = as.numeric(mouse_pos)) %>% 
    
    # calculate min and max x for each participant
    group_by(matchid) %>%
    mutate(max_mouse = max(mouse_pos, na.rm = TRUE),
           min_mouse = min(mouse_pos, na.rm = TRUE)) %>%
    ungroup() %>%
    
    # normalise x values
    mutate(normalised_mouse_pos = ((mouse_pos - min_mouse) / (max_mouse - min_mouse))) %>%
    dplyr::select(matchid, trial_num, measurement, normalised_mouse_pos) %>%
    
    
    # ensure columns are ordered sequenatially, then turn back into wide form
    mutate(measurement = as.numeric(measurement)) %>%
    
    distinct(.keep_all = TRUE) %>%
    spread(measurement, normalised_mouse_pos, drop = FALSE) %>%
    
    # remove trials where no measurement was taken at point 1
    filter(!is.na(`1`)) %>%
    mutate_all(funs(ifelse(is.na(.), "", .))) %>%
    
    # determine whether trial was valid or not based on some criterion
    mutate(valid_trial = case_when(abs(`1` - expected_start_value) > acceptance_bound ~ 0,
                                   TRUE ~ 1)) %>%
    unite(col = !!normalised_column_name, `1`:str_length, sep = ",") 
  
  return(df_2)
  
}


# normalise x values
normalise_x_values_df <- mt_pep_cleaned_df %>%
  coordinate_normalisation(mouse_dimension = "xpos",
                           vector_length = vec_len, 
                           expected_start_value = .5,
                           acceptance_bound = .1,
                           normalised_column_name = "xpos_normalised") %>%
  # remove excess commas
  mutate(xpos_normalised = str_remove(xpos_normalised, "\\,,[^.]*$")) %>%
  # rename valid trial for clarity
  dplyr::rename(valid_x = valid_trial)


# normalise y values
normalise_y_values_df <- mt_pep_cleaned_df %>%
  coordinate_normalisation(mouse_dimension = "ypos",
                           vector_length = vec_len, 
                           expected_start_value = 1,
                           acceptance_bound = .1,
                           normalised_column_name = "ypos_normalised") %>%
  # remove excess commas 
  mutate(ypos_normalised = str_remove(ypos_normalised, "\\,,[^.]*$")) %>%
  # rename valid trial for clarity
  dplyr::rename(valid_y = valid_trial) 



# create new df with normalised and recented values
normalised_mouse_vals_df <- normalise_x_values_df %>%
  left_join(normalise_y_values_df, by = c("matchid", "trial_num")) %>%
  left_join(recenter_timestamps_df, by = c("matchid", "trial_num")) %>%
  na.omit()
# na.omit could be used if some rows contain x data and not y data

#rm(normalise_x_values_df, normalise_y_values_df, recenter_timestamps_df)
#write.csv(normalised_mouse_vals_df, "normalised_mouse_vals_df.csv")




# Process mousetracking data using Mousetrap

#Creates a mousetrap object with trajectories scaled equivalently, time-normalised, and aligned.



mt_pep_df <- mt_pep_cleaned_df %>%
  left_join(normalised_mouse_vals_df) %>%
  
  # remove invalid trials
  filter(valid_x == 1 & valid_y == 1) %>% 
  
  # remove old coordinates and unnecessary columns, attach new normalised ones
  dplyr::select(-xpos, -ypos, -valid_x, -valid_y, -timestamps) %>%
  dplyr::rename(xpos = xpos_normalised,
                ypos = ypos_normalised,
                timestamps = recentered_timestamps)



tweak_and_normalise <- function(x) {
  x %>%
    mt_import_mousetrap() %>%
    mt_resample(save_as = "trajectories") %>%
    mt_align_start_end() %>%
    mt_remap_symmetric(remap_xpos = "right") %>%
    mt_derivatives() %>%
    mt_time_normalize(verbose = TRUE) %>%
    mt_measures()
}



normalised_mt <- tweak_and_normalise(mt_pep_df)



# create to-be-analysed dataframes



# trial-level

spelling_correct <- normalised_mt$measures %>%
  
  # creating score for correct rate above 80%
  bind_cols(normalised_mt$data) %>%
  group_by(matchid) %>%
  dplyr::summarise(spelling_correct_response = sum(correct),
                   spelling_total_n = 72,
                   spelling_score = spelling_correct_response / spelling_total_n) %>% 
  ungroup() %>% 
  filter(spelling_score> 0.8)
#out of 213, 184 achieves the spelling correct pre-registered criteria

trial_level_pep_df <- normalised_mt$measures %>%
  
  # to get information about individual trials back
  bind_cols(normalised_mt$data) %>% 
  #removing participants who scored less than 80% correct
  left_join(spelling_correct) %>% 
  filter(!is.na(spelling_score)) %>% 
  # filter only relevant trials
  filter(correct == 1) %>% 
  #select only important columns
  dplyr::select(matchid, trial_type, reversed, spelling, belief, AUC, RT, trial_num) %>%
  #rename auc and rt
  dplyr::rename(auc = AUC,
                rt = RT) %>% 
  #group by participant id
  group_by(matchid) %>% 
  #exclude outliers for rt
  schoRsch::outlier(dv = "rt", todo = "elim", upper.z = 3, lower.z = -3) %>%
  dplyr::select(-outlier, -zscores) %>%
  #exclude outliers for auc
  schoRsch::outlier(dv = "auc", todo = "elim", upper.z = 3, lower.z = -3) %>%
  ungroup() %>%
  dplyr::select(-outlier, -zscores) %>%
  ungroup()

# calculate pep effect for each participant 

mt_pep_df <- trial_level_pep_df %>%
  #include only catch trials
  dplyr::filter(trial_type != "catch") %>%
  #select important columns
  dplyr::select(matchid, belief, auc) %>% 
  #group by belief column and matchid
  group_by(belief, matchid) %>% 
  #create average area under the curves by belief column and matchids
  summarise(auc = mean(auc)) %>% 
  #put it back to wide format
  spread(belief, auc) %>% 
  #calculate pep_effect, higher scores represent higher growth mindset beliefs
  mutate(pep_effect = `1` - `0`) %>% 
  #select only important colums
  dplyr::select(matchid, pep_effect) %>% 
  #put matchids to character to be able to match with other data
  mutate(matchid = as.character(matchid))

write.csv(mt_pep_df, "data/mt_pep_df.csv")


