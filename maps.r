library(tidyverse)
library(tidycensus)
library(tigris)
library(units)
library(sf)

options(tigris_use_cache = T)

# 1. geodata merge in shape_merge.r. 
# - land grab u shapes for morill parcels https://github.com/HCN-Digital-Projects/landgrabu-data
# - royce map shapes from forest service 'Indian Land Cessions in US' https://data-usfs.hub.arcgis.com/datasets/usfs::tribal-lands-ceded-to-the-united-states-feature-layer/about
# - contemporary (or 1920) state shape files
# - Pacific railroad act shapes, need to figure this out (WAITING ON BLM FOR MORE INFO)
# 
# active.digitsposession = (morill act + homestead act + railroad grants) / total state area
# 
# given that railroad acreage is unknown, then 
# 
# \[Disp_{s} \propto \frac{MorillAcres_s + HomesteadAcres_s}{Acreage_s}\]
# 
# If
# 
# \[cor(MorillAcres_s + HomesteadAcres_s, RailroadAcres_s)\] is constant across states
# 
# Remaining native lands as of 1892 can be computed with Royce data as 
# 
# \[RemainingLands_s = RoyceAcres_s - Acreage_s\]
# 
# Should be a pretty high negative correlation between Disp_s and RemainingLands_s

### First obtain Acreage_s

st<-states(cb = T) %>%
  st_transform(8528) %>% 
  filter(!STUSPS%in%
           c("HI", "AK", 
             "PR", "GU", "AS","VI",
             "MP"))

st_acres<-data.frame(state = st$STUSPS,
                     total_acres_2021 = as.numeric(set_units(st_area(st), "acre")))

### royce map data
## available here: https://data-usfs.hub.arcgis.com/datasets/usfs::tribal-lands-ceded-to-the-united-states-feature-layer/about
## and archived here: https://data.nativeland.info/dataset/indian-land-cessions-in-the-united-states-1784-1894/resource/e2b874d5-3c39-4287-bf89-d4ef14b87c83
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

cessions_diff<-cessions_diff %>% 
  mutate(year = NA)

### layer state boundaries
cessions<-st_intersection(cessions, st)
### add in settler claimed territories as of 1796
cessions2 <- cessions %>%  
  bind_rows(cessions_diff %>%  
              mutate(year = NA))

m1<-ggplot(cessions2,
       aes(fill = year)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "Year",
       title = "Post-1784 Indigenous disposession",
       subtitle = "Cessions to US federal government through 1894") 

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
# 
# ## no cession states
# drops<-ts_full %>% 
#   filter(year == 1893) %>% 
#   filter(cumulative_ceded == 0) 


### add in full state area

ts_full<-ts_full %>% 
  filter(!STUSPS%in%drops$STUSPS) %>% 
  full_join(pop %>% 
              rename(STUSPS = state) %>% 
              select(STUSPS, region, division) %>% 
              distinct())

p1<-ggplot(ts_full %>% 
         filter(region == "Northeast"),
       aes(x = year, y = ceded_acres / 1e6)) + 
  geom_line() + 
  geom_line(aes(y = cumulative_ceded / 1e6),
            lty = 2) + 
  facet_wrap(division~STUSPS) + 
  labs(x = "Year",
       y = "Acres (millions)",
       title = "Northeast") + 
  lims(y = c(0, 150)) 

p2<-ggplot(ts_full %>% 
         filter(region == "South"),
       aes(x = year, y = ceded_acres / 1e6)) + 
  geom_line() + 
  geom_line(aes(y = cumulative_ceded / 1e6),
            lty = 2) + 
  facet_wrap(division~STUSPS)+ 
  labs(x = "Year",
       y = "Acres (millions)",
       title = "South") + 
  lims(y = c(0, 150))

p3<-ggplot(ts_full %>% 
         filter(region == "North Central"),
       aes(x = year, y = ceded_acres / 1e6)) + 
  geom_line() + 
  geom_line(aes(y = cumulative_ceded / 1e6),
            lty = 2) + 
  facet_wrap(division~STUSPS)+ 
  labs(x = "Year",
       y = "Acres (millions)",
       title = "North Central") + 
  lims(y = c(0, 150))

p4<-ggplot(ts_full %>% 
         filter(region == "West"),
       aes(x = year, y = ceded_acres / 1e6)) + 
  geom_line() + 
  geom_line(aes(y = cumulative_ceded / 1e6),
            lty = 2) + 
  facet_wrap(division~STUSPS)+ 
  labs(x = "Year",
       y = "Acres (millions)",
       title = "West") + 
  lims(y = c(0, 150))

### top code cessions to total acres

ts_full<-ts_full %>% 
  filter(!is.na(year)) %>% 
  mutate(prop_ceded = cumulative_ceded / total_acres_2021,
         prop_ceded = ifelse(prop_ceded>1, 1, prop_ceded)) 

ggplot(ts_full,
       aes(x = year, y = prop_ceded)) +
  geom_line() +
  geom_vline(xintercept = 1862, lty = 2) + 
  facet_wrap(~STUSPS)

#### THIS CAPTURES DISPOSESSION GEOGRAPHY AND PROCESS. OUTPUT

ts_full %>% 
  select(year, STUSPS,
         ceded_acres, cumulative_ceded,
         total_acres_2021, prop_ceded) %>% 
  rename(state = STUSPS) %>% 
  write_csv("./data/disposession_ts.csv")

##############################################################
### REDISTRIBUTION.
##############################################################

### MORRILL ACT DATA
morill<-read_csv("./data/landgrabu-data/Morrill_Act_of_1862_Indigenous_Land_Parcels_Database/CSVs/Parcels.csv")
### create state by year patented acreage time series
### prefer to use Cession year, (Yr_US_Acquire)
### but for unceded, will use Yr_ST_Accept as alt
### take the minimum of all year variables for first year recorded dispossessed
morill <- morill %>% 
  mutate(Yr_ST_Accept = as.numeric(Yr_ST_Accept),
         Yr_Uni_Assing = as.numeric(Yr_Uni_Assign),
         Yr_Patent = as.numeric(Yr_Patent))
### rowwise minima of year measures
morill<-morill %>% 
  rowwise() %>% 
  mutate(year = Yr_Patent) 

morill<-morill %>% 
  group_by(Loc_State, year) %>% 
  summarize(Acres = sum(Acres))

full_ts<-expand_grid(Loc_State = unique(morill$Loc_State),
                     year = unique(morill$year))

morill<-morill %>% 
  right_join(full_ts) %>% 
  mutate(Acres = ifelse(is.na(Acres), 0, Acres)) %>% 
  arrange(Loc_State, year) %>% 
  group_by(Loc_State) %>% 
  mutate(cumulative_acres = cumsum(Acres)) %>% 
  filter(year<=1930)

morill_plot<-ggplot(morill,
       aes(x = year, y = Acres / 1e6,
           group = Loc_State)) + 
  geom_line() + 
  geom_line(aes(y = cumulative_acres / 1e6),
            lty = 2) + 
  facet_wrap(~Loc_State) + 
  labs(y = "Acres (millions)",
       x = "Year",
       subtitle = "Annual totals solid\nCumulative totals dashed")

### homestead act data
#### load in homestead data and boarding school data
### state measure can be cumulative sum of homesteads, that gets at polity
### homesteading data
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
                        value))

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
  group_by(state, year, type) %>%
  summarize(value = sum(value))

## harmonize names
state_xwalk <- data.frame(state = state.name,
                        state.abb = state.abb)

hs<-hs %>% 
  left_join(state_xwalk) %>% 
  rename(homestead_acres = value)%>% 
  filter(type == "acres") %>% 
  select(-type)

## 
morill<-morill %>% 
  rename(state.abb = Loc_State) %>% 
  left_join(state_xwalk) %>% 
  rename(morill_acres = Acres,
         year = Yr_Patent) 

land_distrib<-morill %>% 
  full_join(hs)

### 
land_distrib <- land_distrib %>%
  filter(year <= 1920) %>% 
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(morill_acres_cum = cumsum(morill_acres),
         homestead_acres_cum = cumsum(homestead_acres)) %>%
  ungroup()
