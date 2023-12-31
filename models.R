## sourced by ms.Rmd

mdat<-pop %>% 
  left_join(dist) %>% 
  left_join(bs)

#### set weakly informative priors for model parameters
### b ~ N(0, 20)
### use brms defaults for variance params (PEG THEM DOWN)

priors_m1<-c(
  prior(normal(0, 10), class = b),
  prior(normal(0, 10), class = Intercept),
  prior(student_t(3, 0, 2.5), class = sd),
  prior(gamma(0.01, 0.01), class = shape))



### fit HMC model with brms
teachers_m1_bayes<-brm(
  teachers ~ 
    scale(I(blk_sharecrop / tot_pop)) +
    scale(I(urban_immig_pop / tot_pop)) +
    scale(morill_acres_cum + homestead_acres_cum) +
    log(children) + 
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  prior = priors_m1,
  data = mdat
) 


### prior visual
# teachers_m1_bayes %>% 
#   prior_summary() %>% 
#   parse_dist(prior) %>% 
#   ggplot(aes(y = class, dist = .dist, args = .args)) + 
#   stat_dist_halfeye()

cops_m1_bayes<-brm(
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

socwork_m1_bayes<-brm(
  socwork ~ 
    scale(I(blk_sharecrop / tot_pop)) +
    scale(I(urban_immig_pop / tot_pop)) +
    scale(morill_acres_cum + homestead_acres_cum) +
    log(children) + 
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  data = mdat)

### institutionalization model

mdat <- mdat %>% 
  mutate(inst_gq = inst_mental + 
           inst_poor_disabled)

prison_gq_m_bayes<-brm(
  floor(inst_correctional) ~ 
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
