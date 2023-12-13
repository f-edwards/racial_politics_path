### edited 12/6/23; FE
#### read royce data, morill data, homestead data
#### make state time series for native land disposession up to 1920
#### make state time series for settler distribution up to 1920
# - land grab u shapes for morill parcels https://github.com/HCN-Digital-Projects/landgrabu-data
# - royce map shapes from forest service 
# 'Indian Land Cessions in US' https://data-usfs.hub.arcgis.com/datasets/usfs::tribal-lands-ceded-to-the-united-states-feature-layer/about
# - homestead data from http://homestead.unl.edu/projects/homesteading-the-plains/data/blm_homesteads.xlsx

library(tidyverse)
library(tidycensus)
library(tigris)
library(units)
library(sf)

options(tigris_use_cache = T)

##############################################################################
# pull state shape file --------------------------------------------------
##############################################################################

st<-states(cb = T) %>%
  st_transform(8528) %>% 
  filter(!STUSPS%in%
           c("HI", "AK", 
             "PR", "GU", "AS","VI",
             "MP"))


st_acres<-data.frame(state = st$STUSPS,
                     total_acres_2021 = as.numeric(set_units(st_area(st), "acre")))

##############################################################################
# read and format land dispossession data /shapes ----------------------------
##############################################################################

cessions<-read_sf("./data/us_forest_cessions_royce/S_USA.TRIBALCEDEDLANDS.shp")

cessions_dat<-read_csv("./data/us_forest_cessions_royce/royce.csv") %>% 
  mutate(time = as_date(TribalCededLandsTableCessDate1),
         year = year(time)) %>% 
  rename(CESSNUM = TribalCededLandsNewCESSNUM) %>% 
  select(CESSNUM, year)

cessions<-cessions %>% 
  left_join(cessions_dat)

cessions<-cessions %>% 
  st_transform(8528)

cessions_diff<-st_difference(st, st_union(cessions))

### cessions_diff is land not ceded prior to 1893. 
### needs manual coding for pre 1784 claims
st_1796<-c("ME", "NH", "VT", "NY", "MA",
           "CT", "RI", "DE", "NJ", "PA",
           "MD", "VA", "NC", "SC", "GA",
           "WV", "KY", "TN")

cessions_diff<-cessions_diff %>% 
  mutate(st_1796 = STUSPS%in%st_1796)

### layer state boundaries
cessions<-st_intersection(cessions, st)
### add in settler claimed territories as of 1796
cessions2 <- cessions %>%  
  bind_rows(cessions_diff %>% filter(st_1796==T) %>% 
              mutate(year = 1795))

ts<-cessions2 %>% 
  select(year, STUSPS) %>% 
  mutate(ceded_acres = set_units(st_area(cessions2), "acre")) %>% 
  group_by(year, STUSPS) %>% 
  summarize(ceded_acres = sum(ceded_acres)) 

### make the full time series
ts_full<-expand_grid(year = unique(ts$year),
                     STUSPS = unique(ts$STUSPS)) %>% 
  left_join(ts)%>% 
  arrange(STUSPS, year) %>% 
  ungroup() %>% 
  mutate(ceded_acres = ifelse(is.na(ceded_acres), 0, ceded_acres)) %>% 
  group_by(STUSPS) %>% 
  mutate(cumulative_ceded = cumsum(ceded_acres)) %>% 
  left_join(st_acres %>% 
              rename(STUSPS = state))

### top code cessions to total acres

ts_full<-ts_full %>% 
  filter(!is.na(year)) %>% 
  mutate(prop_ceded = cumulative_ceded / total_acres_2021,
         prop_ceded = ifelse(prop_ceded>1, 1, prop_ceded))

#### THIS CAPTURES DISPOSESSION GEOGRAPHY AND PROCESS. OUTPUT

ts_full %>% 
  select(year, STUSPS,
         ceded_acres, cumulative_ceded,
         total_acres_2021, prop_ceded) %>% 
  write_csv("./data/temp_disposession_ts.csv")

##############################################################
### REDISTRIBUTION.
##############################################################

### MORRILL ACT DATA
morill<-read_csv("./data/landgrabu-data/Morrill_Act_of_1862_Indigenous_Land_Parcels_Database/CSVs/Parcels.csv")
### create state by year patented acreage time series
### measuring settler distribution, so use patent, st_accept, or uni_assign
morill <- morill %>% 
  mutate(Yr_ST_Accept = as.numeric(Yr_ST_Accept),
         Yr_Uni_Assign = as.numeric(Yr_Uni_Assign),
         Yr_Patent = as.numeric(Yr_Patent))
### rowwise maxima of year measures
### aiming to capture salience of transactions
morill<-morill %>% 
  rowwise() %>% 
  mutate(year = max(c(Yr_ST_Accept, 
                      Yr_Uni_Assign, 
                      Yr_Patent),
                    na.rm = T)) 

morill<-morill %>% 
  group_by(Loc_State, year) %>% 
  summarize(Acres = sum(Acres)) %>% 
  arrange(year, Loc_State)

### homestead act data
### modified raw data by cleaning columns / headers for read_csv() otherwise unchanged
### http://homestead.unl.edu/projects/homesteading-the-plains/data/blm_homesteads.xlsx

hs<-read_csv("./data/blm_homesteads_clean.csv") %>%
  pivot_longer(claims1868:acres1961,
               names_to = "name",
               values_to = "value") %>%
  mutate(year = str_sub(
    name, start = -4, end = -1),
    type = str_sub(
      name, start = 1, end = -5)) %>%
  select(-name) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(value = ifelse(is.na(value),
                        0,
                        value)) %>% 
  filter(type == "acres") %>% 
  select(-type)

### split Dakota Territory values to 0.5 ND, 0.5 SD to preserve time series
hs_dakota <- hs %>%
  filter(state == "Dakota Territory") %>%
  mutate(value = value / 2) %>%
  select(-state)

hs <- hs %>%
  bind_rows(hs_dakota %>%
              mutate(state = "North Dakota")) %>%
  bind_rows(hs_dakota %>%
              mutate( state = "South Dakota")) %>%
  filter(state != "Dakota Territory") %>%
  group_by(state, year) %>%
  summarize(value = sum(value))

## harmonize names
state_xwalk <- data.frame(state = state.name,
                        state.abb = state.abb)

hs<-hs %>% 
  left_join(state_xwalk) %>% 
  rename(homestead_acres = value) %>% 
  ungroup()

morill<-morill %>% 
  rename(state.abb = Loc_State) %>% 
  left_join(state_xwalk) %>% 
  rename(morill_acres = Acres) %>% 
  ungroup()

### complete missing years
### as foundation for join

land_distrib<-ts_full %>% 
  select(STUSPS, total_acres_2021) %>% 
  distinct() %>% 
  full_join(expand_grid(
    year = 1862:1930,
    STUSPS = unique(ts_full$STUSPS))) %>% 
  left_join(morill %>% 
              select(state.abb, year, morill_acres) %>% 
              rename(STUSPS = state.abb)) %>% 
  left_join(hs %>% 
              select(state.abb, year, homestead_acres) %>% 
                       rename(STUSPS = state.abb)) %>% 
  replace_na(list(morill_acres = 0, homestead_acres = 0)) %>% 
  arrange(STUSPS, year) %>%
  group_by(STUSPS) %>%
  mutate(morill_acres_cum = cumsum(morill_acres),
         homestead_acres_cum = cumsum(homestead_acres)) %>%
  ungroup()

### output
land_distrib %>% 
  write_csv("./data/temp_distribution_ts.csv")
