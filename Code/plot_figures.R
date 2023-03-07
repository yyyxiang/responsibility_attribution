library('tidyverse')
library('patchwork')
library('Rmisc')
library('lmerTest')
source('helper.R')

dat <- read.csv('data.csv', header = T, stringsAsFactors = T)
model_predictions <- generate_model_predictions(dat)
dat$contestant <- as.factor(dat$contestant)
model_predictions$contestant <- as.factor(model_predictions$contestant)
model_predictions <- model_predictions %>% mutate(type = case_when(s1 == s2 ~ 'Same strength',
                                                                   e1 == e2 ~ 'Same effort',
                                                                   T ~ 'Same force'))

# fig2
pdf('fig2.pdf', onefile = T, width = 12, height = 8)
line_plots(dat, model_predictions, '1a')
dev.off()

# fig3
pdf('fig3.pdf', onefile = T, width = 12, height = 8)
line_plots(dat, model_predictions, '1b')
dev.off()

# fig6
pdf('fig6.pdf', onefile = T, width = 12, height = 8)
line_plots(dat, model_predictions, '2a')
dev.off()

# fig7
pdf('fig7.pdf', onefile = T, width = 12, height = 8)
line_plots(dat, model_predictions, '2b')
dev.off()

# fig4a BIC (across participants)
dat <- read.csv('data.csv', header = T, stringsAsFactors = T)
model_predictions <- generate_model_predictions(dat)
dat <- merge(dat, model_predictions) %>% 
  dplyr::rename(resp_data = resp)
dat$subject <- as.factor(dat$subject)
dat <- arrange(dat, exp_index, subject, scenario, contestant)

bic_table <- NULL
for (exp in unique(dat$exp_index)) {
  F_model <- lmer(resp_data ~ resp_F + (resp_F|subject),
                  data = dat %>% filter(exp_index == exp), REML = F)
  S_model <- lmer(resp_data ~ resp_S + (resp_S|subject),
                  data = dat %>% filter(exp_index == exp), REML = F)
  E_model <- lmer(resp_data ~ resp_E + (resp_E|subject),
                  data = dat %>% filter(exp_index == exp), REML = F)
  FA_model <- lmer(resp_data ~ resp_FA + (resp_FA|subject),
                   data = dat %>% filter(exp_index == exp), REML = F)
  NFA_model <- lmer(resp_data ~ resp_NFA + (resp_NFA|subject),
                    data = dat %>% filter(exp_index == exp), REML = F)
  BA_model <- lmer(resp_data ~ resp_BA + (resp_BA|subject),
                   data = dat %>% filter(exp_index == exp), REML = F)
  EBA_model <- lmer(resp_data ~ resp_EBA + (resp_EBA|subject),
                    data = dat %>% filter(exp_index == exp), REML = F)
  df <- BIC(F_model, S_model, E_model, FA_model, NFA_model, BA_model, EBA_model) %>% select(BIC)
  df$model <- rownames(df)
  rownames(df) <- NULL
  df$exp_index <- exp
  bic_table <- rbind(bic_table, df)
}
bic_table$model <- factor(bic_table$model, labels = c('F','S','E','FA','NFA','BA','EBA'), 
                          levels = c('F_model','S_model','E_model','FA_model',
                                     'NFA_model','BA_model',
                                     'EBA_model'), ordered = T)

fig4a <- bic_table %>% ggplot(aes(model, BIC, color = model)) +
  geom_point(size = 3) +
  facet_wrap(~ exp_index, nrow = 5, scales = 'free_y', 
             labeller = as_labeller(c(`1a`='Exp 1a', `1b`='Exp 1b', `2a`='Exp 2a', `2b`='Exp 2b', `3`='Exp 3'))) +
  scale_color_manual(name = NULL, labels = NULL,
                     values = c('gray', 'gray', '#71cbe1', 'gray', 'gray', '#fe9653', '#9cdc6f'),
                     limits = c('F','S','E','FA','NFA','BA','EBA')) +
  theme_bw()

# fig4b BIC (single participant)
full_df <- NULL
full_p <- NULL
full_ci <- NULL
for (exp in unique(dat$exp_index)) {
  subset <- dat %>% filter(exp_index == exp)
  df <- data.frame(exp_index = exp, subject = unique(subset$subject), bic_e = NaN, bic_s = NaN, bic_f = NaN,
                   bic_ba = NaN, bic_fa = NaN, bic_nfa = NaN, bic_eba = NaN)
  for (s in df$subject) {
    F_model <- lm(resp_data ~ resp_F, data = subset %>% filter(subject == s))
    S_model <- lm(resp_data ~ resp_S, data = subset %>% filter(subject == s))
    E_model <- lm(resp_data ~ resp_E, data = subset %>% filter(subject == s))
    FA_model <- lm(resp_data ~ resp_FA, data = subset %>% filter(subject == s))
    NFA_model <- lm(resp_data ~ resp_NFA, data = subset %>% filter(subject == s))
    BA_model <- lm(resp_data ~ resp_BA, data = subset %>% filter(subject == s))
    EBA_model <- lm(resp_data ~ resp_EBA, data = subset %>% filter(subject == s))
    bic_df <- BIC(E_model, S_model, F_model, BA_model, FA_model, NFA_model, EBA_model)
    df$lowest[df$subject == s] <- row.names(bic_df[which(bic_df$BIC == min(bic_df$BIC)),])
    df[df$subject == s, 3:(ncol(df)-1)] <- bic_df[,2]
    counts <- data.frame(rbind(table(df$lowest)))
    num_subject <- length(df$subject)
    p <- counts/num_subject
    ci <- sqrt(p*(1-p)/num_subject)*1.96
    p$exp_index <- exp
    ci$exp_index <- exp
  }
  full_df <- dplyr::bind_rows(full_df, df)
  full_p <- dplyr::bind_rows(full_p, p)
  full_ci <- dplyr::bind_rows(full_ci, ci)
}

full_p <- full_p %>% pivot_longer(cols = !exp_index, names_to = 'model', values_to = 'p')
full_ci <- full_ci %>% pivot_longer(cols = !exp_index, names_to = 'model', values_to = 'ci')
full_p <- merge(full_p, full_ci)
full_p$model <- factor(full_p$model, labels = c('F','S','E','FA','NFA','BA','EBA'), 
                       levels = c('F_model','S_model','E_model','FA_model',
                                  'NFA_model','BA_model',
                                  'EBA_model'), ordered = T)

fig4b <- full_p %>% ggplot(aes(model, p, fill = model)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin = p-ci, ymax = p+ci), width = .1,
                position = position_dodge(.9)) +
  coord_cartesian(ylim = c(NA,1)) +
  facet_wrap(~ exp_index, nrow = 5,
             labeller = as_labeller(c(`1a`='Exp 1a', `1b`='Exp 1b', `2a`='Exp 2a', `2b`='Exp 2b', `3`='Exp 3'))) +
  scale_fill_manual(name = NULL, labels = NULL,
                    values = c('gray', 'gray', '#71cbe1', 'gray', 'gray', '#fe9653', '#9cdc6f'),
                    limits = c('F','S','E','FA','NFA','BA','EBA')) +
  theme_bw() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1))

pdf('fig4.pdf', onefile = T, width = 15, height = 6)

fig4a + labs(x = 'Model', title = 'A') + theme(legend.position = "none") +
  fig4b + labs(x = 'Model', y = 'Proportion', title = 'B') + theme(legend.position = "none")

dev.off()

# fig5
pdf('fig5.pdf', onefile = T, width = 12, height = 15)

dat <- read.csv('data.csv', header = T, stringsAsFactors = T)
model_predictions <- generate_model_predictions(dat)

dat_summary <- dat %>%
  group_by(exp_index, condition, scenario, contestant) %>%
  dplyr::summarize(avg_resp = mean(resp),
                   uci_resp = CI(resp)[1],
                   lci_resp = CI(resp)[3])
dat_merge <- merge(dat_summary, model_predictions)
dat_merge_long <- dat_merge %>% pivot_longer(cols = c('resp_E', 'resp_BA', 'resp_EBA'), 
                                             names_to = 'model',
                                             values_to = 'predicted_resp')
dat_merge_long$model <- substr(dat_merge_long$model, 6, nchar(dat_merge_long$model))
dat_merge_long$model <- factor(dat_merge_long$model, levels = c('E', 'BA', 'EBA', ordered = T))

text_labels <- data.frame(exp_index = sort(rep(unique(dat_merge_long$exp_index),3)), model = rep(unique(dat_merge_long$model),5))
for (exp in unique(dat_merge_long$exp_index)) {
  for (m in unique(dat_merge_long$model)) {
    cor_both <- cor(dat_merge_long$avg_resp[dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_both <- paste0('r = ', format(round(cor_both, 2), nsmall = 2))
    cor_fail <- cor(dat_merge_long$avg_resp[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_fail <- paste0('r_fail = ', format(round(cor_fail, 2), nsmall = 2))
    cor_lift <- cor(dat_merge_long$avg_resp[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$predicted_resp[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_lift <- paste0('r_lift = ', format(round(cor_lift, 2), nsmall = 2))
    # use cor.test to calculate p-values
    text_labels$text[text_labels$exp_index == exp & text_labels$model == m] <- paste0(cor_both, sep = '\n', cor_fail, sep = '\n', cor_lift)
    text_labels$x[text_labels$exp_index == exp & text_labels$model == m] = 8
    text_labels$y[text_labels$exp_index == exp & text_labels$model == m] = 1.5
  }
}

fig5 <- cor_plt(dat_merge_long, 'predicted_resp', 'avg_resp', 'lci_resp', 'uci_resp', text_labels) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0,2.5,5,7.5,10)) +
  scale_y_continuous(limits = c(0,10), breaks = c(0,2.5,5,7.5,10)) +
  coord_fixed()
fig5 + labs(x = 'Model', y = 'Data')

dev.off()

