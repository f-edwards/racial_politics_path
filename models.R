## sourced by ms.Rmd

mdat<-pop %>% 
  left_join(dist) %>% 
  left_join(bs) %>% 
  mutate(blk_sharecrop_pc = blk_sharecrop / tot_pop,
         urban_immig_pc = urban_immig_pop / tot_pop,
         homestead_plus_morill = homestead_acres_cum + morill_acres_cum)

#### set weakly informative priors for model parameters
### for m1

priors_m1<-c(
  prior(normal(0, 10), class = b),
  prior(normal(0, 10), class = Intercept),
  prior(student_t(3, 0, 2.5), class = sd),
  prior(gamma(0.01, 0.01), class = shape))

### fit HMC model with brms
teachers_m1<-brm(
  teachers ~ 
    scale(blk_sharecrop_pc) +
    scale(urban_immig_pc) +
    scale(homestead_plus_morill) +
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat
) 

cops_m1<-brm(
  police ~ 
    scale(blk_sharecrop_pc) +
    scale(urban_immig_pc) +
    scale(homestead_plus_morill) +
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  data = mdat
)

socwork_m1<-brm(
  socwork ~ 
    scale(blk_sharecrop_pc) +
    scale(urban_immig_pc) +
    scale(homestead_plus_morill) +
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  data = mdat)

### institutionalization model

mdat <- mdat %>% 
  mutate(inst_gq = inst_mental + 
           inst_poor_disabled)

prison_m1<-brm(
  floor(inst_correctional) ~ 
    scale(blk_sharecrop_pc) +
    scale(urban_immig_pc) +
    scale(homestead_plus_morill) +
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat %>% 
    filter(year!=1910))

inst_m1<-brm(
  floor(inst_gq) ~ 
    scale(blk_sharecrop_pc) +
    scale(urban_immig_pc) +
    scale(homestead_plus_morill) +
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat %>% 
    filter(year!=1910))

# boarding schools models use 1890 - 1920 time period
# models need to be specified differntly because of census mismatch
# may need to increase samples to up ESS
# but for a simple descriptive, this looks fine IMO

priors_mz<-priors_m1<-c(
  prior(normal(0, 10), class = b),
  prior(normal(0, 10), class = Intercept))

bs_m1_z<-brm(boarding_schools>0 ~ 
               scale(blk_sharecrop_pc) +
               scale(urban_immig_pc) +
               scale(homestead_plus_morill) ,
             family = bernoulli(link = "logit"),
             prior = priors_mz,
             data = mdat %>% 
               filter(year == 1900))

bs_m1<-brm(boarding_schools ~ 
             scale(blk_sharecrop_pc) +
             scale(urban_immig_pc) +
             scale(homestead_plus_morill) +
             factor(year) +
             (1|STUSPS),
           family = negbinomial(),
           prior = priors_m1,
           data = mdat %>% 
             filter(boarding_schools>0))
