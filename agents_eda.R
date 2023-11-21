library(tidyverse)

### load in agents data
agents<-read_csv("./data/AGENTS_DATA_COLLECTION.csv")
### agency means tribe totals are contained in the single agency row, we can treat tribe numbers 
### zeroes for the purpose of agency / state totals
agents<-agents %>% 
  filter(YEAR >= 1895) %>% 
  mutate(AVG_ATTENDANCE = ifelse(AVG_ATTENDANCE == "Agency" | AVG_ATTENDANCE == "Reserve"| 
                               AVG_ATTENDANCE == "as above", NA, AVG_ATTENDANCE))


sum_students <- agents %>% 
  mutate(AVG_ATTENDANCE = eval(parse(text = AVG_ATTENDANCE)))


agents_1890<-agents %>% 
  filter(YEAR>=1890) %>% 
  select(`STATE/SUPERINTENDENCY`,
         TRIBE,
         AGENCY,
         YEAR,
         `DAY Y/N/B`,
         AVG_ATTENDANCE) 


students_ts<-agents_1890 %>% 
  group_by(`STATE/SUPERINTENDENCY`, YEAR) %>% 
  summarize(students = sum(AVG_ATTENDANCE))

### use the flag on day school to sort into attendance categories
### create AVG_ATTENDANCE, AVG_ATTENDANCE - DAY !="Y"