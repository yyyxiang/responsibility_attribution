library('tidyverse')
library('lmerTest')
source('helper.R')

dat <- read.csv('./../Data/data.csv', header = T, stringsAsFactors = T)
model_predictions <- generate_model_predictions(dat)
dat <- merge(dat, model_predictions) %>% 
  dplyr::rename(resp_data = resp)
dat$contestant <- as.factor(dat$contestant)
dat$subject <- as.factor(dat$subject)
dat <- arrange(dat, exp_index, subject, scenario, contestant)
dat <- dat[, c('exp_index','subject','condition','scenario','box_weight','contestant',
               'strength','effort','resp_data','s1','s2','e1','e2',
               'resp_E','resp_S','resp_F','resp_BA','resp_FA','resp_NFA','resp_EBA')]

## descriptive stats
# same strength
for (idx in c('1a', '1b', '2a', '2b')) {
  same_strength <- lmer(resp_data ~ condition*contestant + (condition*contestant|subject),
                   data = dat %>% filter(exp_index == idx & s1 == s2))
  print(summary(same_strength))
}

# same effort
for (idx in c('1a', '1b', '2a', '2b')) {
  same_effort <- lmer(resp_data ~ condition*contestant + (condition*contestant|subject),
                   data = dat %>% filter(exp_index == idx & e1 == e2))
  print(summary(same_effort))
}

# same force
# Exp 1a
same_force <- lmer(resp_data ~ condition*contestant + (condition*contestant|subject),
                 data = dat %>% filter(exp_index == '1a' & s1 != s2 & e1 != e2))
print(summary(same_force))

# Exp 1b (only has one condition)
same_force <- lmer(resp_data ~ contestant + (contestant|subject),
                 data = dat %>% filter(exp_index == '1b' & s1 != s2 & e1 != e2))
print(summary(same_force))

# Exp 2a
same_force <- lmer(resp_data ~ condition*contestant + (condition*contestant|subject),
                   data = dat %>% filter(exp_index == '2a' & s1 != s2 & e1 != e2))
print(summary(same_force))

# Exp 2b (only has one condition)
same_force <- lmer(resp_data ~ contestant + (contestant|subject),
                   data = dat %>% filter(exp_index == '2b' & s1 != s2 & e1 != e2))
print(summary(same_force))

## show that effort explains away the effect of force
for (idx in unique(dat$exp_index)) {
  fit_lmm1 <- lmer(resp_data ~ resp_E + resp_F +
                     (resp_E|subject) + (resp_F|subject),
                   data = dat %>% filter(exp_index == idx))
  print(summary(fit_lmm1))
}

## show that all three factors contribute (effort, counterfactual of focal agent, and counterfactual of non-focal agent)
for (idx in unique(dat$exp_index)) {
  fit_lmm2 <- lmer(resp_data ~ resp_E + resp_FA + resp_NFA + 
                     (resp_E|subject) + (resp_FA|subject) + (resp_NFA|subject),
                   data = dat %>% filter(exp_index == idx))
  print(summary(fit_lmm2))
}


## EBA model weighting justification
for (idx in c('1a', '1b', '2a', '2b', '3')) {
  EBA_model_breakdown <- lmer(resp_data ~ resp_BA + resp_E + (resp_BA|subject) + (resp_E|subject),
                                 data = dat %>% filter(exp_index == idx))
  print(summary(EBA_model_breakdown))
}

