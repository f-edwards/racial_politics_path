## sourced by ms.Rmd

mdat<-pop %>% 
  left_join(dist) %>% 
  left_join(bs) %>% 
  mutate(blk_sharecrop_pc = blk_sharecrop / tot_pop,
         urban_immig_pc = urban_immig_pop / tot_pop,
         homestead_plus_morill = homestead_acres_cum + morill_acres_cum)

#### set weakly informative priors for model parameters
### for m1
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
    scale(blk_sharecrop_pc) +
    scale(urban_immig_pc) +
    scale(homestead_plus_morill) +
    year_c + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat
) 
### try a growth model
mdat<-mdat %>% 
  mutate(year_c = (year - 1900)/10)

teachers_m2<-brm(
  teachers ~ 
    (scale(blk_sharecrop_pc) +
    scale(urban_immig_pc) +
    scale(homestead_plus_morill)) *
    factor(year_c) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat) 
# context interaction (slopes by predictor type)
teachers_m3<-brm(
  teachers ~ 
    (scale(blk_sharecrop_pc) +
       scale(urban_immig_pc) +
       scale(homestead_plus_morill)) *
    year_c + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat) 

## compare m1 / m3, clear performance gain for m3
# loo_compare(loo(teachers_m1), loo(teachers_m3))
### adding year RE doesn't help.

loo_compare(loo(teachers_m3), loo(teachers_m4))


#### START HERE!!!!
### TIME VARYING SLOPES ON CONTEXT
### DOES INTERACTION ON SHARECROPPING PERSIST?
### simulate

year_c = 0:7
STUSPS <- "XX"
tot_pop = 1e6

pred_dat<-data.frame(
  scen = c("South", "Northeast", "West"),
  blk_sharecrop_pc = quantile(mdat$blk_sharecrop_pc, c(0.9, 0, 0)),
  urban_immig_pc = quantile(mdat$urban_immig_pc, c(0, 0.9, 0)),
  homestead_plus_morill = quantile(mdat$homestead_plus_morill, c(0, 0, 0.9))) %>% 
  expand_grid(year_c, tot_pop)

## define scenarios

predsm3<-teachers_m3 %>% 
  add_epred_draws(re_formula = NA,
                  newdata = pred_dat)

ggplot(predsm3,
       aes(x = year_c, y = .epred)) + 
  stat_lineribbon() +
  facet_wrap(~scen) + 
  scale_fill_brewer(palette = "Greys")


#### fit simple growth model
teachers_m0<-brm(
  teachers ~ 
    year_c + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat
) 

preds_m3<-posterior_epred(teachers_m3)
plot_dat<- mdat %>% 
  mutate(yhat = apply(preds_m3, 2, mean))

ggplot(plot_dat, aes(x = year, 
                     y = yhat / tot_pop,
                     group = STUSPS,
                     color = region)) + 
  geom_line() 
### 

mdat<-mdat %>% 
  mutate(year_c = year - 1900)


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

### write model results to file
saveRDS(teachers_m1, file = "./models/teachers_m1.rds")
saveRDS(cops_m1, file = "./models/cops_m1.rds")
saveRDS(socwork_m1, file = "./models/socwork_m1.rds")
saveRDS(prison_m1, file = "./models/prison_m1.rds")
saveRDS(inst_m1, file = "./models/inst_m1.rds")
saveRDS(bs_m1_z, file = "./models/bs_m1_z.rds")
saveRDS(bs_m1, file = "./models/bs_m1.rds")


### theory: intercept and slope may differ based on focal values
## 

