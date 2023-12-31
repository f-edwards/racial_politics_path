library(tidyverse)

### cleaning needed:
# state label typos
# NA states
# eval all addition statements
# replace NA with 0 when appropriate
# code through 1930

### load in agents data
agents<-read_csv("./data/AGENTS_DATA_COLLECTION1890_1920.csv") 



agents<-agents %>% 
  mutate(across(BOARDING_N_ACC:N_MISSIONARIES, as.numeric)) %>% 
  select(`STATE/SUPERINTENDENCY`, TRIBE, YEAR,
         BOARDING_N_ACC:N_MISSIONARIES) %>% 
  rename(state = `STATE/SUPERINTENDENCY`) %>% 
  mutate(BOARDING_N_ACC = replace_na(BOARDING_N_ACC, 0),
         DAY_N_ACC = replace_na(DAY_N_ACC, 0),
         STUDENTS_ENROLL = replace_na(STUDENTS_ENROLL, 0),
         AVG_ATTENDANCE = replace_na(AVG_ATTENDANCE, 0),
         N_MISSIONARIES = replace_na(N_MISSIONARIES, 0)) %>% 
  mutate(state = case_when(
    state == "Colordao" ~ "Colorado",
    state == "Indian Territory" ~ "Oklahoma",
    state == "MInnesota" ~ "Minnesota",
    state == "Nebreska" ~ "Nebraska",
    state == "Oklahoma Territory" ~ "Oklahoma",
    state == "Uin" ~ "Utah",
    state == "Virgina" ~ "Virginia",
    state == "Virginiga" ~ "Virginia",
    state == "indiana" ~ "Indiana",
    T ~ state
  )) %>% #recode typos
  filter(!(is.na(state))) %>% 
  group_by(state, YEAR) %>% 
  summarize(across(BOARDING_N_ACC:N_MISSIONARIES, sum)) %>% 
  rename_all(tolower)
### agency means tribe totals are contained in the single agency row, we can treat tribe numbers 

### make plot for full period
year<-1890:1920
state<-unique(state.name)

xwalk<-expand_grid(year, state)
agents<-xwalk %>% 
  left_join(agents) %>% 
  mutate(across(boarding_n_acc:n_missionaries, ~replace_na(.x, 0)))

agents %>% 
  ggplot(aes(x = year, y = boarding_n_acc)) + 
  geom_line() + 
  facet_wrap(~state)
