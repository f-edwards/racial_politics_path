## sourced by ms.Rmd


### CONSIDER SPLITTING MENTAL INST AND POVERTY INST????

mdat<-pop %>% 
  left_join(dist) %>% 
  left_join(bs) %>% 
  mutate(blk_sharecrop_pc = blk_sharecrop / tot_pop,
         urban_immig_pc = urban_immig_pop / tot_pop,
         homestead_plus_morill = homestead_acres_cum + morill_acres_cum)

#### set weakly informative priors for model parameters
### for m1
### try a growth model
### try a growth model

mdat<-mdat %>% 
  mutate(year_c = (year - 1900)/10)

priors_m1<-c(
  prior(normal(0, 10), class = b),
  prior(normal(0, 10), class = Intercept),
  prior(student_t(3, 0, 2.5), class = sd),
  prior(gamma(0.01, 0.01), class = shape))

### fit HMC model with brms
teachers_m1<-brm(
  teachers ~ 
    (scale(blk_sharecrop_pc) +
       scale(urban_immig_pc) +
       scale(homestead_plus_morill)) *
    year_c + 
    (year_c|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat) 

cops_m1<-brm(
  police ~
    (scale(blk_sharecrop_pc) +
       scale(urban_immig_pc) +
       scale(homestead_plus_morill)) *
    year_c + 
    (year_c|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat) 

socwork_m1<-brm(
  socwork ~
    (scale(blk_sharecrop_pc) +
       scale(urban_immig_pc) +
       scale(homestead_plus_morill)) *
    year_c + 
    (year_c|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat) 

### institutionalization model

mdat <- mdat %>%
  mutate(inst_gq = inst_mental +
           inst_poor_disabled)

prison_m1<-brm(
  floor(inst_correctional) ~
    (scale(blk_sharecrop_pc) +
    scale(urban_immig_pc) +
    scale(homestead_plus_morill)) *
    year_c + 
    (year_c|STUSPS) +
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat)

mental_m1<-brm(
  floor(inst_mental) ~
    (scale(blk_sharecrop_pc) +
       scale(urban_immig_pc) +
       scale(homestead_plus_morill)) *
    year_c + 
    (year_c|STUSPS) +
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat)

poor_m1<-brm(
  floor(inst_poor_disabled) ~
    (scale(blk_sharecrop_pc) +
       scale(urban_immig_pc) +
       scale(homestead_plus_morill)) *
    year_c + 
    (year_c|STUSPS) +
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat)

# boarding school binary

priors_mz<-priors_m1<-c(
  prior(normal(0, 10), class = b),
  prior(normal(0, 10), class = Intercept))

bs_m1_z<-brm(boarding_schools>0 ~
               scale(blk_sharecrop_pc) +
               scale(urban_immig_pc) +
               scale(homestead_plus_morill) +
               factor(year_c) +
               (1|STUSPS) ,
             family = bernoulli(link = "logit"),
             prior = priors_mz,
             data = mdat,
             iter = 1e5)

## write model results to file
saveRDS(teachers_m1, file = "./models/teachers_m1.rds")
saveRDS(cops_m1, file = "./models/cops_m1.rds")
saveRDS(socwork_m1, file = "./models/socwork_m1.rds")
saveRDS(prison_m1, file = "./models/prison_m1.rds")
saveRDS(mental_m1, file = "./models/mental_m1.rds")
saveRDS(poor_m1, file = "./models/poor_m1.rds")
saveRDS(bs_m1_z, file = "./models/bs_m1_z.rds")


