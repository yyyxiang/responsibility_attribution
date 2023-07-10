library('tidyverse')
library('patchwork')
library('Rmisc')
library('lmerTest')
source('helper.R')

# figS1
dat <- read.csv('data.csv', header = T, stringsAsFactors = T)
equal_weighting <- ensemble_models(dat, 0.5) %>% dplyr::rename(resp_EBA_equal = resp_EBA)
unequal_weighting1 <- ensemble_models(dat, 0.3) %>% dplyr::rename(resp_EBA_unequal1 = resp_EBA) %>% select(resp_EBA_unequal1)
unequal_weighting2 <- ensemble_models(dat, 0.4) %>% dplyr::rename(resp_EBA_unequal2 = resp_EBA) %>% select(resp_EBA_unequal2)
unequal_weighting3 <- ensemble_models(dat, 0.6) %>% dplyr::rename(resp_EBA_unequal3 = resp_EBA) %>% select(resp_EBA_unequal3)
unequal_weighting4 <- ensemble_models(dat, 0.7) %>% dplyr::rename(resp_EBA_unequal4 = resp_EBA) %>% select(resp_EBA_unequal4)
df <- cbind(equal_weighting, unequal_weighting1, unequal_weighting2, unequal_weighting3, unequal_weighting4)

pdf('figS1.pdf', onefile = T, width = 15, height = 15)

dat_summary <- dat %>%
  group_by(exp_index, condition, scenario, contestant) %>%
  dplyr::summarize(avg_resp = mean(resp),
                   uci_resp = CI(resp)[1],
                   lci_resp = CI(resp)[3])
dat_merge <- merge(dat_summary, df)
dat_merge_long <- dat_merge %>% pivot_longer(cols = c(resp_EBA_equal,resp_EBA_unequal1,resp_EBA_unequal2,resp_EBA_unequal3,resp_EBA_unequal4), names_to = 'weight', values_to = 'predicted_resp') %>% 
  mutate(weight = case_when(weight == 'resp_EBA_equal' ~ 0.5,
                            weight == 'resp_EBA_unequal1' ~ 0.3,
                            weight == 'resp_EBA_unequal2' ~ 0.4,
                            weight == 'resp_EBA_unequal3' ~ 0.6,
                            weight == 'resp_EBA_unequal4' ~ 0.7))
dat_merge_long$model <- factor(dat_merge_long$weight, levels = c(0.5, 0.3, 0.4, 0.6, 0.7), ordered = T)

text_labels <- data.frame(exp_index = sort(rep(unique(dat_merge_long$exp_index),5)), model = rep(unique(dat_merge_long$model),5))
for (exp in unique(dat_merge_long$exp_index)) {
  for (m in unique(dat_merge_long$model)) {
    cor_both <- cor(dat_merge_long$avg_resp[dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_both <- paste0('r = ', format(round(cor_both, 2), nsmall = 2))
    cor_fail <- cor(dat_merge_long$avg_resp[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_fail <- paste0('r_fail = ', format(round(cor_fail, 2), nsmall = 2))
    cor_lift <- cor(dat_merge_long$avg_resp[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_lift <- paste0('r_lift = ', format(round(cor_lift, 2), nsmall = 2))
    text_labels$text[text_labels$exp_index == exp & text_labels$model == m] <- paste0(cor_both, sep = '\n', cor_fail, sep = '\n', cor_lift)
    text_labels$x[text_labels$exp_index == exp & text_labels$model == m] = 8
    text_labels$y[text_labels$exp_index == exp & text_labels$model == m] = 1.5
  }
}

figS1 <- cor_plt(dat_merge_long, 'predicted_resp', 'avg_resp', 'lci_resp', 'uci_resp', text_labels) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0,2.5,5,7.5,10)) +
  scale_y_continuous(limits = c(0,10), breaks = c(0,2.5,5,7.5,10)) +
  coord_fixed()
figS1 + labs(x = 'Model', y = 'Data')

dev.off()

## line plots
dat <- read.csv('data.csv', header = T, stringsAsFactors = T)
model_predictions <- generate_model_predictions(dat)
dat$contestant <- as.factor(dat$contestant)
model_predictions$contestant <- as.factor(model_predictions$contestant)
model_predictions <- model_predictions %>% mutate(type = case_when(s1 == s2 ~ 'Same strength',
                                                                   e1 == e2 ~ 'Same effort',
                                                                   T ~ 'Same force'))

# figS2
pdf('figS2.pdf', onefile = T, width = 12, height = 8)
line_plots_actual(dat, model_predictions, '1a')
dev.off()

# figS3
pdf('figS3.pdf', onefile = T, width = 12, height = 8)
line_plots_actual(dat, model_predictions, '1b')
dev.off()

# figS4
pdf('figS4.pdf', onefile = T, width = 12, height = 8)
line_plots_counterfactual(dat, model_predictions, '1a')
dev.off()

# figS5
pdf('figS5.pdf', onefile = T, width = 12, height = 8)
line_plots_counterfactual(dat, model_predictions, '1b')
dev.off()

# figS9
pdf('figS9.pdf', onefile = T, width = 12, height = 8)
line_plots_actual(dat, model_predictions, '2a')
dev.off()

# figS10
pdf('figS10.pdf', onefile = T, width = 12, height = 8)
line_plots_actual(dat, model_predictions, '2b')
dev.off()

# figS11
pdf('figS11.pdf', onefile = T, width = 12, height = 8)
line_plots_counterfactual(dat, model_predictions, '2a')
dev.off()

# figS12
pdf('figS12.pdf', onefile = T, width = 12, height = 8)
line_plots_counterfactual(dat, model_predictions, '2b')
dev.off()

## scatter plots
# figS6
pdf('figS6.pdf', onefile = T, width = 12, height = 15)

dat <- read.csv('data.csv', header = T, stringsAsFactors = T)
model_predictions <- generate_model_predictions(dat)

dat_summary <- dat %>%
  group_by(exp_index, condition, scenario, contestant) %>%
  dplyr::summarize(avg_resp = mean(resp),
                   uci_resp = CI(resp)[1],
                   lci_resp = CI(resp)[3])
dat_merge <- merge(dat_summary, model_predictions)
dat_merge_long <- gather(dat_merge, key = 'model', value = 'predicted_resp', c('resp_F', 'resp_S', 'resp_E'))
dat_merge_long$model <- substr(dat_merge_long$model, 6, nchar(dat_merge_long$model))
dat_merge_long$model <- factor(dat_merge_long$model, levels = c('F', 'S', 'E', ordered = T))

text_labels <- data.frame(exp_index = sort(rep(unique(dat_merge_long$exp_index),3)), model = rep(unique(dat_merge_long$model),5))
for (exp in unique(dat_merge_long$exp_index)) {
  for (m in unique(dat_merge_long$model)) {
    cor_both <- cor(dat_merge_long$avg_resp[dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_both <- paste0('r = ', format(round(cor_both, 2), nsmall = 2))
    cor_fail <- cor(dat_merge_long$avg_resp[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_fail <- paste0('r_fail = ', format(round(cor_fail, 2), nsmall = 2))
    cor_lift <- cor(dat_merge_long$avg_resp[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_lift <- paste0('r_lift = ', format(round(cor_lift, 2), nsmall = 2))
    text_labels$text[text_labels$exp_index == exp & text_labels$model == m] <- paste0(cor_both, sep = '\n', cor_fail, sep = '\n', cor_lift)
    text_labels$x[text_labels$exp_index == exp & text_labels$model == m] = 8
    text_labels$y[text_labels$exp_index == exp & text_labels$model == m] = 1.5
  }
}

figS6 <- cor_plt(dat_merge_long, 'predicted_resp', 'avg_resp', 'lci_resp', 'uci_resp', text_labels) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0,2.5,5,7.5,10)) +
  scale_y_continuous(limits = c(0,10), breaks = c(0,2.5,5,7.5,10)) +
  coord_fixed()
figS6 + labs(x = 'Model', y = 'Data')

dev.off()

# figS7
pdf('figS7.pdf', onefile = T, width = 12, height = 15)

dat <- read.csv('data.csv', header = T, stringsAsFactors = T)
model_predictions <- generate_model_predictions(dat)

dat_summary <- dat %>%
  group_by(exp_index, condition, scenario, contestant) %>%
  dplyr::summarize(avg_resp = mean(resp),
                   uci_resp = CI(resp)[1],
                   lci_resp = CI(resp)[3])
dat_merge <- merge(dat_summary, model_predictions)
dat_merge_long <- gather(dat_merge, key = 'model', value = 'predicted_resp', c('resp_BA', 'resp_FA', 'resp_NFA'))
dat_merge_long$model <- substr(dat_merge_long$model, 6, nchar(dat_merge_long$model))
dat_merge_long$model <- factor(dat_merge_long$model, levels = c('FA', 'NFA', 'BA', ordered = T))

text_labels <- data.frame(exp_index = sort(rep(unique(dat_merge_long$exp_index),3)), model = rep(unique(dat_merge_long$model),5))
for (exp in unique(dat_merge_long$exp_index)) {
  for (m in unique(dat_merge_long$model)) {
    cor_both <- cor(dat_merge_long$avg_resp[dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_both <- paste0('r = ', format(round(cor_both, 2), nsmall = 2))
    cor_fail <- cor(dat_merge_long$avg_resp[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_fail <- paste0('r_fail = ', format(round(cor_fail, 2), nsmall = 2))
    cor_lift <- cor(dat_merge_long$avg_resp[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_lift <- paste0('r_lift = ', format(round(cor_lift, 2), nsmall = 2))
    text_labels$text[text_labels$exp_index == exp & text_labels$model == m] <- paste0(cor_both, sep = '\n', cor_fail, sep = '\n', cor_lift)
    text_labels$x[text_labels$exp_index == exp & text_labels$model == m] = 8
    text_labels$y[text_labels$exp_index == exp & text_labels$model == m] = 1.5
  }
}

figS7 <- cor_plt(dat_merge_long, 'predicted_resp', 'avg_resp', 'lci_resp', 'uci_resp', text_labels) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0,2.5,5,7.5,10)) +
  scale_y_continuous(limits = c(0,10), breaks = c(0,2.5,5,7.5,10)) +
  coord_fixed()
figS7 + labs(x = 'Model', y = 'Data')

dev.off()

