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
aianh<-native_areas(cb = T) %>%
  st_transform(8528)  

### lower 48
ggplot(st%>% 
         filter(!STUSPS%in%
                  c("HI", "AK", 
                    "PR", "GU", "AS","VI",
                    "MP"))) + 
  geom_sf() + 
  theme_void()

### aianh
ggplot(aianh) + 
  geom_sf() + 
  theme_void()

### overlay
ggplot(st) + 
  geom_sf() + 
  geom_sf(data = aianh,
          fill = "dodgerblue") + 
  theme_void()

aianh_acres<-data.frame(name = aianh[[6]],
                        aianh_acres_2021 = as.numeric(set_units(st_area(aianh), "acre")))

st_acres<-data.frame(state = st$STUSPS,
                     total_acres_2021 = as.numeric(set_units(st_area(st), "acre")))

### subtract out aianh areas from states to compute (state - aianh) / state
t<-st_intersection(st, aianh)
#ggplot(t) + geom_sf()
# area intersection
state_overlap_aian<-data.frame(state = t$STUSPS,
                            area = st_area(t))

state_overlap_aian<-state_overlap_aian %>% 
  group_by(state) %>% 
  summarize(acres_aianh = as.numeric(set_units(sum(area), "acre"))) %>% 
  left_join(st_acres) %>% 
  mutate(prop_aianh = acres_aianh / total_acres_2021)

## (st_area(st) - st_area(t)) / st_area(st)
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

ggplot(cessions,
       aes(fill = year)) + 
  geom_sf() + 
  theme_void()

### WHAT IS THE STATUS OF THE ORIGINAL STATES? 1784 BOUNDARIES?

### can pull unceded lands by year now!!!!!
### this isn't quite right...
### need to overlay full US map, then diff out ceded areas
### the geography i want is NATIVE LANDS UNCEDED BY DATE.
### ROYCE CONTAINS ONLY CESSIONS, SO UNCEDED IS NOT IN THERE

cessions_diff<-st_difference(st, st_union(cessions))

ggplot(st_union(cessions)) + 
  geom_sf() + 
  theme_void()

ggplot(cessions,
       aes(fill = year)) + 
  geom_sf() + 
  theme_void()

ggplot(cessions_diff) + 
  geom_sf() + 
  theme_void()

### cessions_diff is land not ceded prior to 1893. 
### needs manual coding for pre 1784 claims
st_1796<-c("ME", "NH", "VT", "NY", "MA",
           "CT", "RI", "DE", "NJ", "PA",
           "MD", "VA", "NC", "SC", "GA",
           "WV", "KY", "TN")

cessions_diff<-cessions_diff %>% 
  mutate(st_1796 = STUSPS%in%st_1796)

ggplot(cessions_diff,
       aes(fill = st_1796)) + 
  geom_sf() + 
  theme_void()

### unceded light blue, reservation dark blue
ggplot(st) + 
  geom_sf() + 
  geom_sf(data = cessions_diff, fill = "dodgerblue") +
  geom_sf(data = aianh, fill = "blue") + 
  theme_void()

### layer state boundaries
cessions<-st_intersection(cessions, st)
### add in settler claimed terriroties as of 1796
cessions2 <- cessions %>%  
  bind_rows(cessions_diff %>% filter(st_1796==T) %>% 
              mutate(year = 1784))

ggplot(cessions2,
       aes(fill = st_1796)) + 
  geom_sf()

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

### add in full state area

ggplot(ts_full,
       aes(x = year, y = ceded_acres)) + 
  geom_line() + 
  geom_line(aes(y = cumulative_ceded),
            lty = 2) + 
  facet_wrap(~STUSPS)

### top code cessions to total acres

ts_full<-ts_full %>% 
  filter(!is.na(year)) %>% 
  mutate(prop_ceded = cumulative_ceded / total_acres_2021,
         prop_ceded = ifelse(prop_ceded>1, 1, prop_ceded))

ggplot(ts_full,
       aes(x = year, y = prop_ceded)) + 
  geom_line() +
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
  mutate(Yr_US_Acquire = as.numeric(Yr_US_Acquire),
         Yr_ST_Accept = as.numeric(Yr_ST_Accept),
         Yr_Uni_Assing = as.numeric(Yr_Uni_Assign),
         Yr_Patent = as.numeric(Yr_Patent))
### rowwise minima of year measures
morill<-morill %>% 
  rowwise() %>% 
  mutate(year = min(c(Yr_US_Acquire, Yr_ST_Accept, 
                      Yr_Uni_Assign, Yr_Patent),
                    na.rm = T)) %>% 
  select(year)



morill<-morill %>% 
  group_by(Loc_State, Ys_ST_Accept) %>% 
  summarize(Acres = sum(Acres))

full_ts<-expand_grid(Loc_State = unique(morill$Loc_State),
                     Yr_Patent = unique(morill$Yr_Patent))

morill<-morill %>% 
  right_join(full_ts) %>% 
  mutate(Acres = ifelse(is.na(Acres), 0, Acres)) %>% 
  arrange(Loc_State, Yr_Patent)

ggplot(morill %>% 
         filter(Yr_Patent<1921),
       aes(x = Yr_Patent, y = Acres,
           group = Loc_State)) + 
  geom_line() + 
  facet_wrap(~Loc_State)

#### how do I think patenting impacts local politics - through active efforts to make ownership claims
#### subsequent efforts to develop / exploit land

###

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
