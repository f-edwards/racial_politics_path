## ----setup, include = F, message = F----------------------------------
library(tidyverse)
library(ipumsr)
library(srvyr)
library(maps)

## ----read IPUMS tables--------------------------------------------------
# ### occupations 

dat_occ<-read_ipums_micro("./data/usa_00071.xml")
dat_occ<-dat_occ %>%
  rename_all(tolower) %>%
  mutate(occ1950 =
           case_when(
             occ1950 == 79 ~ "socwork",
             occ1950 == 773 ~ "police",
             occ1950 == 771 ~ "police",
             occ1950 == 782 ~ "police",
             occ1950 == 93 ~ "teachers"
           )) %>%
  group_by(year, statefip, occ1950) %>%
  summarize(emp = sum(perwt)) %>%
  pivot_wider(names_from = occ1950, values_from = emp,
              values_fill = 0)
# pop (total, age, race, nativitiy, urban, farm, landown)

# srvyr mod ---------------------------------------------------------------
dat_immig<-read_ipums_micro("./data/usa_00073.xml")

dat_immig<-dat_immig %>% 
  filter(YEAR>=1900) %>% 
  as_survey_design(
    ids = CLUSTER,
    weights = PERWT,
    strata = STRATA,
    nest = T
  )

temp<-dat_immig %>% 
  filter(YEAR == 1920) %>% 
  group_by(STATEFIP) %>% 
  summarize(pop = survey_total())

###

dat_immig<-dat_immig %>% 
  rename_all(tolower) %>%
  group_by(year, statefip) %>%
  summarize(tot_pop = sum(perwt),
            children = sum((age<18) * perwt),
            white_immig =
              sum((nativity == 5) * (race == 1) * (hispan == 0) * perwt),
            blk =
              sum((race == 2) * perwt),
            blk_sharecrop = 
              sum((race==2) * (farm==2) * (ownershp==2) * perwt),
            blk_farms = sum((race==2) * (farm==2) * perwt),
            urban_pop = sum(hhwt * (urban ==2)),
            urban_immig_pop = 
              sum((urban==2) * 
                    (nativity == 5) * (race == 1) * (hispan == 0) * perwt),
            tot_hh = sum(hhwt))
### group quarters, 100%
gq<-read_csv("./data/usa_00069.csv") %>%
  rename_all(tolower) %>%
  filter(age<18) %>%
  group_by(year, statefip, gqtype) %>%
  summarize(gq_pop = sum(perwt, na.rm = T)) %>%
  mutate(gqtype = case_when(
    gqtype == 2 ~ "child_inst_correctional",
    gqtype == 3 ~ "child_inst_mental",
    gqtype == 4 ~ "child_inst_poor_disabled")) %>%
  filter(gqtype!="other") %>%
  pivot_wider(names_from = gqtype, values_from = gq_pop,
              values_fill = 0)
### join
dat <- data.frame(state = state.abb,
               region = state.region,
               division = state.division) %>%
  left_join(state.fips %>%
              select(fips, abb) %>%
              distinct() %>%
              rename(state = abb, statefip = fips)) %>%
  left_join(dat_immig) %>%
  left_join(dat_occ) %>%
  ungroup() %>%
  left_join(gq)

dat %>% 
  filter(!(is.na(statefip))) %>% 
  write_csv("./data/temp_population_ts.csv")
