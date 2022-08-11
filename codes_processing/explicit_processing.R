# Processing explicit
library(tidyverse)

#Read data
explicit_raw<- read.csv("data/MTPEP_project_October 25, 2021_06.35.csv")
#exclude qualtrics extra 2 rows at the beginning of the dataset
explicit_raw<-explicit_raw[-c(1, 2), ]

#convert data types
explicit_raw <- explicit_raw %>%  type_convert() %>%  
    #filter out those who just opened up the page but did not finish = -12
    filter(Progress>98,
           #pre-registered exclusion criteria:non-natives = -2
           mothertongue == "Muttersprache",
           #pre-registered exclusion criteria:don't use my data = -5
           Q433 == "Ja, meine Daten verwenden."
    ) 

#setting the name of labjs column so we can merge the implicit data with the explicit ones
names(explicit_raw)[names(explicit_raw)=="labjs.data"] <- "matchid_pep"

#labjs automatically removes 0s from beginning of ids, so we also remove them from explicit
explicit_raw<- explicit_raw %>% 
    mutate(matchid_pep= str_remove(matchid_pep, "^0+"))

#writing raw explicit data for behavioral processing
#write.csv(explicit_raw, "data/explicit_raw.csv")
  
##cleaning explicit data
explicit_coded <- 
    explicit_raw %>% 
    mutate_at(c(28:76, 199:204, 268:274), 
              ~recode(.,`Stimme gar nicht zu` = 1,
                      `Stimme überwiegend nicht zu` = 2,
                      `Stimme eher nicht zu` = 3,
                      `Stimme etwas zu` = 4,
                      `Stimme überwiegend zu` = 5,
                      `Stimme völlig zu` = 6)) %>% 
    mutate_at(vars(task_enjoyment_1, task_enjoyment_2), 
              ~recode(.,`Überhaupt nicht gut` = 1,
                      `Überwiegend nicht gut` = 2,
                      `Eher nicht gut` = 3,
                      `Eher gut` = 4,
                      `Überwiegend gut` = 5,
                      `Sehr gut` = 6)) %>% 
    mutate_at(vars(success_imp_1), 
              ~recode(.,`Überhaupt nicht wichtig` = 1,
                      `Überwiegend unwichtig` = 2,
                      `Eher unwichtig` = 3,
                      `Eher wichtig` = 4,
                      `Überwiegend wichtig` = 5,
                      `Sehr wichtig` = 6)) %>%
    mutate_at(vars(Q603_1), 
              ~recode(.,`Überhaupt nicht gut` = 1,
                      `Überwiegend nicht gut` = 2,
                      `Eher nicht gut` = 3,
                      `Eher gut` = 4,
                      `Überwiegend gut` = 5,
                      `Sehr gut` = 6)) %>%
    mutate_at(vars(iq_related_1, iq_related_2),
              ~recode(.,`Gar nicht` = 1,
                      `2` = 2,
                      `3` = 3,
                      `4` = 4,
                      `5` =5,
                      `Sehr` = 6)) %>%
    mutate_at(vars(akad_leistung_1),
              ~recode(.,`eindeutig ein Misserfolg` = 1,
                      `eher ein Misserfolg` = 2,
                      `eher ein Erfolg` = 3,
                      `eindeutig ein Erfolg` = 4)) %>%
    mutate_at(vars(eltern_abschluss),
              ~recode(.,`Ja, eine*r von beiden.` = 1,
                      `Nein, keine*r der beiden.` = 0,
                      `Ja, beide.` = 1)) %>%
    #reverse coding variables
    mutate_at(vars(iq_mindset_1, iq_mindset_3, iq_mindset_5, effort_pep1_2, effort_pep1_3, effort_pep1_6, effort_pep2_1,
                   effort_pep2_3, effort_pep2_5, achm2_1, achm2_3, achm2_4, achm2_5, tsab_1_3, tsab_1_4,
                   tsab_2_3, tsab_2_4), 
              ~recode(.,`1` = 6,
                      `2` = 5,
                      `3` = 4,
                      `4` = 3,
                      `5` = 2,
                      `6` = 1
              )) %>% 
  #cleaning self-reported grade data
    mutate_at(vars(note), 
              ~recode(.,`?` = "NA",
                      `73` = "NA",
                      `keine Angabe` = "NA",
                      `2,3 circa` = "2,3",
              )) %>% 
  #translating gender variable
  mutate_at(vars(gender), 
            ~recode(.,`männlich` = "male",
                    `weiblich` = "female",
                    `divers` = "divers",
            ))

#creating cummulated variables
explicit_scores<-
    explicit_coded %>% 
    mutate(iqms_avg = rowMeans(x = dplyr::select(.data = ., starts_with(match = "iq_mindset_"))),
           effort_enjoyment = rowMeans(x = dplyr::select(.data = ., starts_with(match = "effort_pep"))),
           anxiety = rowMeans(x = dplyr::select(.data = ., starts_with(match = "anxiety"))),
           self_efficacy = rowMeans(x = dplyr::select(.data = ., starts_with(match = "MSLQ")), na.rm = TRUE),
           self_perc_talent = rowMeans(x = dplyr::select(.data = ., starts_with(match = "begabung"))),
           achievement_mot = rowMeans(x = dplyr::select(.data = ., starts_with(match = "achm"))),
           self_ability_bf = rowMeans(x = dplyr::select(.data = ., starts_with(match = "tsab_1_"))),
           other_ability_bf = rowMeans(x = dplyr::select(.data = ., starts_with(match = "tsab_2_"))),
           together_ability = rowMeans(x = dplyr::select(.data = ., starts_with(match = "tsab"))),
           control_effort = rowMeans(x = select(.data = .,control_effort_2, control_effort_3)),
           control_ability = rowMeans(x = select(.data = .,control_ability_2, control_ability_3)),
           effort_approach  = rowMeans(x = select(.data = .,`ACHIEVEMENT.GOALS_4`, `ACHIEVEMENT.GOALS_5`)),
           effort_avoidance  = rowMeans(x = select(.data = .,`ACHIEVEMENT.GOALS_6`, `ACHIEVEMENT.GOALS_7`)),
           grade = gsub(",", ".", note, fixed=TRUE),
           #in Austrian education, GPA is reversed, so reverse it back so higher numbers mean higher achievement
           gpa_reversed = 6-as.numeric(grade)) %>% 
  #renamnig some variables for the final dataset
    rename(task_approach = `ACHIEVEMENT.GOALS_1`,
           task_avoidance = `ACHIEVEMENT.GOALS_2`,
           self_approach = `ACHIEVEMENT.GOALS_3`,
           matchid = matchid_pep,
           study = iq_related.2,
           study_text = iq_related_12_TEXT,
           iq_importance = iq_related_2,
           iq_security = iq_related_1,
           setback_experience = Q603_1,
           academic_succ_experience = akad_leistung_1,
           first_gen = eltern_abschluss,
           first_gen_text = eltern_abschluss_4_TEXT,
           success_performance = SC0,
           setback_performance = SC1,
           age = birthyear)
         

#creating final, cleaned dataset for explicit data
explicit_final<- explicit_scores %>% 
  select(matchid, iqms_avg:effort_avoidance, task_approach, task_avoidance, self_approach, iq_security:study_text, 
         setback_performance, setback_experience, success_imp_1:task_enjoyment_2, gender, age, 
         semester_1, semester_2, gpa_reversed, academic_succ_experience, first_gen)

write.csv(explicit_final, "data/explicit_final.csv")
  
  