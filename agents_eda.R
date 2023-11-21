library(tidyverse)

### load in agents data
agents<-read_csv("./data/AGENTS_DATA_COLLECTION.csv")
### agency means tribe totals are contained in the single agency row, we can treat tribe numbers 
### zeroes for the purpose of agency / state totals
agents<-agents %>% 
  filter(YEAR >= 1880) %>% 
  mutate(N_STUDENTS = ifelse(N_STUDENTS == "Agency" | N_STUDENTS == "Reserve"| 
                               N_STUDENTS == "as above", NA, N_STUDENTS))


sum_students <- agents %>% 
  mutate(n_students = eval(parse(text = N_STUDENTS)))
