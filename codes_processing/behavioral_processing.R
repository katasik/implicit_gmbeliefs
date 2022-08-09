##Processing behavioral 
#Load library
library(tidyverse)

#reading in raw data
explicit_raw<- read.csv("data/explicit_raw.csv")

#rename important columns for behavioral data
learning_table<- explicit_raw %>% 
  rename(decision_1 = Q525,
         decision_2 = Q4,
         decision_3 = Q8, 
         decision_4 = Q12,
         decision_5 = Q16,
         decision_6 = Q20,
         decision_7 = Q24,
         decision_8 = Q28,
         decision_9 = Q32,
         decision_10 = Q36,
         decision_11 = Q40,
         decision_12 = Q44,
         solution_time_1 = `Q1_Page.Submit`,
         solution_time_2 = `Q5_Page.Submit`,
         solution_time_3 = `Q9_Page.Submit`,
         solution_time_4 = `Q13_Page.Submit`,
         solution_time_5 = `Q17_Page.Submit`,
         solution_time_6 = `Q21_Page.Submit`,
         solution_time_7 = `Q25_Page.Submit`,
         solution_time_8 = `Q29_Page.Submit`,
         solution_time_9 = `Q33_Page.Submit`,
         solution_time_10 = `Q37_Page.Submit`,
         solution_time_11 = `Q41_Page.Submit`,
         solution_time_12 = `Q45_Page.Submit`) %>% 
  select(matchid_pep,
         decision_1, decision_2, decision_3, decision_4, decision_5, decision_6, decision_7, decision_8,
         decision_9, decision_10, decision_11, decision_12, solution_time_1, solution_time_2, solution_time_3,
         solution_time_4, solution_time_5, solution_time_6, solution_time_7, solution_time_8, solution_time_9,
         solution_time_10, solution_time_11, solution_time_12) %>% 
  #because of a programming error, even if people decided not to look at any solutions at the first decision, they were led to the first
  #solution automatically. This way, there is automatically some time calculated for everyone even if they chose
  #not to view any solutions at the beginning. Furthermore, among these people, after viewing the first solution, some of them chose to view more more solutions. 
  #As we don't want to build our results on a technical mistake, so we need to recode their responses as if 
  #they did not look at any solutions if they initially chose not to view any solutions. This problem occurred only at the first 5 solutions, 
  #so we only need to clean this data for the first 5 trials
  
  #thus, in the case when they decided not to view any solutions at the first decision, solution time should be 0, otherwise it should stay the same
  mutate(solution_time_1 = case_when(str_detect(decision_1, "Ich möchte mit dem Haupttest fortfahren.") ~ 0,
                                     TRUE ~ (solution_time_1)),
         solution_time_2 = case_when(str_detect(decision_1, "Ich möchte mit dem Haupttest fortfahren.") ~ 0,
                                     TRUE ~ (solution_time_2)),
         solution_time_3 = case_when(str_detect(decision_1, "Ich möchte mit dem Haupttest fortfahren.") ~ 0,
                                     TRUE ~ (solution_time_3)),
         solution_time_4 = case_when(str_detect(decision_1, "^Ich möchte mit dem Haupttest fortfahren.") ~ 0,
                                     TRUE ~ (solution_time_4)),
         solution_time_5 = case_when(str_detect(decision_1, "^Ich möchte mit dem Haupttest fortfahren.") ~ 0,
                                     TRUE ~ (solution_time_5))) %>% 
 #dummy coding decision variables
  mutate_at(vars(decision_1, decision_2, decision_3, decision_4, decision_5, decision_6, decision_7,
                 decision_8, decision_9, decision_10, decision_11, decision_12), 
            ~recode(.,`Ich möchte mit dem Haupttest fortfahren.` = 0,
                    `Ich möchte die Lösungen ansehen.` = 1,
                    `Mit dem Haupttest fortfahren` = 0,
                    `Lösungen ansehen` = 1)) %>% 
  #the long comment applies also for th number of items viewed
  #thus, if they decided not to view any solutions at the first decision, decisions should be 0, otherwise it should stay the same
  mutate(decision_1 = if_else(decision_1 == 0, 0, decision_1),
         decision_2 = if_else(decision_1 == 0, 0, decision_2),
         decision_3 = if_else(decision_1 == 0, 0, decision_3),
         decision_4 = if_else(decision_1 == 0, 0, decision_4),
         decision_5 = if_else(decision_1 == 0, 0, decision_5))


#cleaning and calculating learning behavior score

learning<-learning_table %>% 
  rowwise() %>% 
  #calculating learning score based on Porter et al (2020), thus creating a variable by adding up the time spent on each solution
  mutate(learning_time = (sum(solution_time_1, solution_time_2, solution_time_3, solution_time_4, solution_time_5,
                              solution_time_6, solution_time_7, solution_time_8, solution_time_9, solution_time_10,
                              solution_time_11, solution_time_12, na.rm =  TRUE))) %>% 
  #adding up the number of solutions reviewed
  mutate(learning_num = sum(decision_1, decision_2, decision_3, decision_4, decision_5, decision_6, decision_7,
                            decision_8, decision_9, decision_10, decision_11, decision_12, na.rm =TRUE)) %>% 
  select(matchid_pep, learning_time, learning_num) %>% 
  mutate(matchid_pep = as.character(matchid_pep)) %>% 
  rename(matchid = matchid_pep)

#creating binary column for analyses
learning$binary_learning<- if_else(learning$learning_time == 0, 0, 1)

write.csv(learning, "data/learning_table.csv")
