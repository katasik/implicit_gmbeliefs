#load library
library(tidyverse)

#read datatables

learning_table<- read.csv("data/learning_table.csv") %>% 
  select(-1)
explicit_final<- read.csv("data/explicit_final.csv") %>% 
  select(-1)
mt_pep_df<- read.csv("data/mt_pep_df.csv") %>% 
  select(-1)

#merge behavioral, explicit and implicit data

learning_explicit<- left_join(learning_table, explicit_final, by="matchid")

#create final df by mergind also the implicit data
final_df<- left_join(mt_pep_df, learning_explicit, by="matchid")

write.csv(final_df, "final_data/final_df.csv")
