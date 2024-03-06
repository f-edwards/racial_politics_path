library(tidyverse)

bs<-read_csv("./data/Boarding_Schools.csv")


### create row for each boarding-school-year
### using start end-pair

### make period of interest
year_prog <- data.frame(year = 1900:1930)

dat_out<-list()

for(i in 1:nrow(bs)){
  
  temp<-bs[i,]
  
  temp<-temp %>% 
    mutate(`End Date` = ifelse(is.na(`End Date`),
                               2022,
                               `End Date`)) %>% 
    select(Name, State, `Start Date`, `End Date`) %>% 
    distinct()
  
  ### top code still operating schools

  
  years<-seq(temp$`Start Date`, temp$`End Date`)
  
  out<-data.frame(year = years,
                  bs_name = temp$Name,
                  state = temp$State)
  
  dat_out[[i]]<-out
  }

bs_ts<-bind_rows(dat_out)

write_csv(bs_ts, "doi_school_index.csv")

bs_state_ts<-bs_ts %>% 
  group_by(state, year) %>% 
  summarize(n = n()) 

### make full 50 state ts for full period
state<-state.abb
year<-seq(min(bs_state_ts$year), max(bs_state_ts$year))
xwalk<-expand_grid(state, year) 

bs_out<-xwalk %>% 
  left_join(bs_state_ts) %>% 
  mutate(boarding_schools = ifelse(is.na(n), 0, n)) %>% 
  select(-n) %>% 
  rename(STUSPS = state)

write_csv(bs_out, "./data/temp_bs_ts.csv")
