library(tidyverse)

### cleaning needed:
# state label typos
# NA states
# eval all addition statements
# replace NA with 0 when appropriate
# code through 1930
# check zeroes / fluctuations on time series with ts plot below
# 1910 looks like a gap
# some weird single years in AZ, MT, MN

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
xwalk<-xwalk %>% 
  left_join(data.frame(state = state.name, STUSPS = state.abb))
agents<-xwalk %>% 
  left_join(agents) %>% 
  mutate(across(boarding_n_acc:n_missionaries, ~replace_na(.x, 0))) %>% 
  filter(year != 1910)

#### VALIDATION PLOTS
# ## ts validation plot
# agents %>%
#   ggplot(aes(x = year, y = boarding_n_acc)) +
#   geom_line() +
#   facet_wrap(~STUSPS)
# 
# ## 1920 validation map
# agents %>% 
#   filter(year == 1920) %>% 
#   left_join(st) %>% 
#   ggplot(aes(fill = boarding_n_acc, geometry = geometry)) + 
#   geom_sf() + 
#   theme_void()
# 
# ### scatterplot with bs data: looks pretty good. some obs where boarding_schools = 0, n_acc !=0
# ### should check them
# source("read_bs.R")
# agents %>% 
#   left_join(bs_out) %>% 
#   ggplot(aes(x = boarding_schools,
#              y = students_enroll)) + 
#   geom_point()

write_csv(agents, "./data/temp_agents.csv")

