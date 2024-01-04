## Diagnostics for model fit
# focus on zero inflated processes
# and parsimony / convergence trade offs
# using default brms priors for ease

mdat<-pop %>% 
  left_join(dist) %>% 
  left_join(bs)


###### teachers - easiest outcome given power
### baseline model as reported in eq (1) in ms
t_m1<-brm(
  teachers ~ 
    scale(I(blk_sharecrop / tot_pop)) +
    scale(I(urban_immig_pop / tot_pop)) +
    scale(morill_acres_cum + homestead_acres_cum) +
    log(children) + 
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  data = mdat
) 

### add regime zero inflation / threshold adjustments
t_m2<-brm(
  teachers ~ 
    scale(I(blk_sharecrop / tot_pop)) +
    I(blk / tot_pop > 0.3) +  
    scale(I(urban_immig_pop / tot_pop)) +
    I(urban_pop>2e5) + 
    scale(morill_acres_cum + homestead_acres_cum) +
    I((morill_acres_cum + homestead_acres_cum)>0) + 
    log(children) + 
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  data = mdat
) 

loo_compare(loo(t_m1), loo(t_m2))

#### cops

p_m1<-brm(
  police ~ 
    scale(I(blk_sharecrop / tot_pop)) +
    scale(I(urban_immig_pop / tot_pop)) +
    scale(morill_acres_cum + homestead_acres_cum) +
    log(children) + 
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  data = mdat
) 

### add regime zero inflation / threshold adjustments
p_m2<-brm(
  police ~ 
    scale(I(blk_sharecrop / tot_pop)) +
    I(blk / tot_pop > 0.3) +  
    scale(I(urban_immig_pop / tot_pop)) +
    I(urban_pop>2e5) + 
    scale(morill_acres_cum + homestead_acres_cum) +
    I((morill_acres_cum + homestead_acres_cum)>0) + 
    log(children) + 
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  data = mdat
) 

loo_compare(loo(p_m1), loo(p_m2))

#### loo shows no meaningful difference bw m1, m2 for both teachers and cops

###### GQ MODELS

mdat <- mdat %>% 
  mutate(inst_gq = inst_mental + 
           inst_poor_disabled)

prison_m1<-brm(
  floor(inst_correctional) ~ 
    scale(I(blk_sharecrop / tot_pop)) +
    scale(I(urban_immig_pop / tot_pop)) +
    scale(morill_acres_cum + homestead_acres_cum) +
    log(children) + 
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  data = mdat)

prison_m2<-brm(
  floor(inst_correctional) ~ 
    scale(I(blk_sharecrop / tot_pop)) +
    I(blk / tot_pop > 0.3) +  
    scale(I(urban_immig_pop / tot_pop)) +
    I(urban_pop>2e5) + 
    scale(morill_acres_cum + homestead_acres_cum) +
    I((morill_acres_cum + homestead_acres_cum)>0) + 
    log(children) + 
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  data = mdat)

loo_compare(loo(prison_m1), loo(prison_m2))

### no meaningful difference in models according to loo
### NJ 1900 shows NA
### looks like failed joins. investigate!
### ballparking numbers for prisoners against https://bjs.ojp.gov/content/pub/pdf/p2581.pdf
### not too bad! 1925 n is 91,689; 1930 is 129,453. I report 215682 for 1930, 79073 for 1920 
# year gqtype  gq_pop
# 1  1880      2  49272 
# 2  1900      2  71491 
# 3  1910      2 135323.
# 4  1920      2  79073 
# 5  1930      2 215682 
### 1910 is discontinuous - that's the sample year, not full census. check docs again
# From IPUMS
# User Note: The Census Bureau did not collect information on the type of group quarters in the 1910 census. 
# This is reflected in the 1910 complete count file, where all group-quarters units have been assigned a GQ code of 4 (other group quarters). 
# Values for GQ, GQTYPE, and GQFUNDS in the 1910 1% and 1.4% samples have been imputed.

gq<-read_csv("./data/usa_00069.csv") %>%
  rename_all(tolower) %>%
  filter(year!=1910) %>% 
  group_by(year, gqtype) %>%
  summarize(gq_pop = sum(perwt)) 

ggplot(gq,
       aes(x = year, 
           y = gq_pop,
           color = factor(gqtype))) + 
  geom_line()


inst_gq_m_bayes<-brm(
  floor(inst_gq) ~ 
    scale(I(blk_sharecrop / tot_pop)) +
    scale(I(urban_immig_pop / tot_pop)) +
    scale(morill_acres_cum + homestead_acres_cum) +
    log(children) + 
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  priors = priors_m1,
  data = mdat)







# boarding schools models use 1890 - 1920 time period
# models need to be specified differntly because of census mismatch
# may need to increase samples to up ESS
# but for a simple descriptive, this looks fine IMO

bs_mdat<-dist %>% 
  filter(year >= 1890, year<=1920, year!=1910) %>% 
  mutate()

#### maybe run this zero inflated

bs_m_bayes<-brm(boarding_schools ~ 
                  scale(I(morill_acres_cum + homestead_acres_cum > 0)) +
                  scale(I(morill_acres_cum + homestead_acres_cum)) +
                  (1|STUSPS) + 
                  (1|year),
                family = negbinomial(),
                data = bs_mdat)

### zero infl model for capacity

bscap_m_bayes_bin<-brm(boarding_n_acc > 0 ~ 
                         I((morill_acres_cum + homestead_acres_cum)>0) +
                         scale(I(morill_acres_cum + homestead_acres_cum)) +
                         (1|STUSPS) + 
                         (1|year),
                       family = bernoulli(link = "logit"),
                       data = bs_mdat)

bscap_m_bayes<-brm(boarding_n_acc ~ 
                     I((morill_acres_cum + homestead_acres_cum)>0) +
                     scale(I(morill_acres_cum + homestead_acres_cum)) +
                     (1|STUSPS) + 
                     (1|year),
                   family = negbinomial(),
                   data = bs_mdat %>% 
                     filter(boarding_n_acc>0))

#### model vis
# bivar - looks like it's just a binary relationship, the linear component isn't adding much here
ggplot(bs_mdat %>% 
         filter(boarding_n_acc >0),
       aes(x = morill_acres_cum + homestead_acres_cum,
           y = boarding_n_acc,
           color = STUSPS)) + 
  geom_point()

ggplot(bs_mdat %>% 
         filter(boarding_n_acc >0),
       aes(x = morill_acres_cum + homestead_acres_cum,
           y = boarding_schools,
           color = STUSPS)) + 
  geom_point()

bsenroll_m_bayes<-brm(floor(students_enroll) ~ 
                        I(morill_acres_cum + homestead_acres_cum > 0) +
                        scale(I(morill_acres_cum + homestead_acres_cum)) +
                        (1|STUSPS) + 
                        (1|year),
                      family = negbinomial(),
                      data = bs_mdat)

