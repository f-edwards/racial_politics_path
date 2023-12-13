library(lme4)
library(brms)

#DISPOSESSION MEASURES - YEAR OF LAST TREATY SIGNED, ACRES PATENTED UNDER MORILL AND HOMESTEAD
#model 1: teachers ~ jim crow, urbanization, disposession (child control?)

mdat<-pop %>% 
  left_join(disp %>% 
              group_by(STUSPS) %>% 
              filter(ceded_acres!=0) %>% 
              summarize(last_treaty = max(year, na.rm = T))) %>% 
  left_join(dist)

#### set weakly informative priors for model parameters
### b ~ N(0, 20)
### use brms defaults for variance params (PEG THEM DOWN)

priors_m1<-c(
  prior(normal(0, 20), class = b),
  prior(normal(0, 20), class = Intercept),
  prior(student_t(3, 0, 2.5), class = sd),
  prior(gamma(0.01, 0.01), class = shape))


# #### lme4 frequentist to check fit
# teachers_m1_nb<-glmer.nb(
#   teachers ~ 
#     scale(I(blk_sharecrop / tot_pop)) +
#     scale(I(urban_immig_pop / tot_pop)) +
#     scale(morill_acres_cum + homestead_acres_cum) +
#     log(children) + 
#     factor(year) + 
#     (1|STUSPS) + 
#     offset(log(tot_pop)),
#   data = mdat
# )
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

# cops_m1_nb<-glmer.nb(
#   police ~ 
#     scale(I(blk_sharecrop / tot_pop)) +
#     scale(I(urban_immig_pop / tot_pop)) +
#     scale(morill_acres_cum + homestead_acres_cum) +
#     log(children) + 
#     factor(year) + 
#     (1|STUSPS) + 
#     offset(log(tot_pop)),
#   data = mdat
# )

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


# socwork_m1_nb<-glmer.nb(
#   socwork ~ 
#     scale(I(blk_sharecrop / tot_pop)) +
#     scale(I(urban_immig_pop / tot_pop)) +
#     scale(morill_acres_cum + homestead_acres_cum) +
#     log(children) + 
#     factor(year) + 
#     (1|STUSPS) + 
#     offset(log(tot_pop)),
#   data = mdat)

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

prison_gq<-brm(
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

inst_gq<-brm(
  floor(inst_gq) ~ 
    scale(I(blk_sharecrop / tot_pop)) +
    scale(I(urban_immig_pop / tot_pop)) +
    scale(morill_acres_cum + homestead_acres_cum) +
    log(children) + 
    factor(year) + 
    (1|STUSPS) + 
    offset(log(tot_pop)),
  family = negbinomial(),
  data = mdat)
