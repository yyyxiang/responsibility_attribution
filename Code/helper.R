generate_model_predictions <- function(data) {
  
  data$force <- data$strength*data$effort/100
  data$total_force <- data$s1*data$e1/100 + data$s2*data$e2/100  
  df <- data %>% filter(subject == 1) %>% mutate(subject = NULL)
  
  df_fail <- df %>% filter(condition == 'fail')
  df_fail$probability <- apply(array(1:dim(df_fail)[1]), 1, function(i){
    mean(apply(array(seq(from = 1, to = 100 - df_fail$effort[i], by = 1)), 1, function(x){
      x/100*df_fail$strength[i] + df_fail$total_force[i] >= df_fail$box_weight[i]
    }))
  })
  
  df_lift <- df %>% filter(condition == 'lift')
  df_lift$probability <- apply(array(1:dim(df_lift)[1]), 1, function(i){
    mean(apply(array(seq(from = 1, to = df_lift$effort[i], by = 1)), 1, function(x){
      df_lift$total_force[i] - x/100*df_lift$strength[i] < df_lift$box_weight[i]
    }))
  })
  
  df <- rbind(df_fail, df_lift)
  
  df_flipped <- df %>% 
    mutate(contestant = 3-contestant) %>% 
    mutate(resp_NFA = (1-probability)*10) %>% 
    select(exp_index, scenario, contestant, resp_NFA)
  df <- merge(df, df_flipped)
  
  model_predictions <- df %>% 
    mutate(resp_F = case_when(condition == 'fail' ~ 10-force,
                              condition == 'lift' ~ force),
           resp_S = strength,
           resp_E = case_when(condition == 'fail' ~ (100-effort)/10,
                              condition == 'lift' ~ effort/10),
           resp_FA = probability*10,
           resp_BA = (resp_FA + resp_NFA)/2,
           resp_EBA = (resp_E + resp_BA)/2)
  model_predictions <- arrange(model_predictions, exp_index, scenario)
  model_predictions <- model_predictions[, c('exp_index','condition','scenario','box_weight','contestant',
                                             'strength','effort','s1','s2','e1','e2',
                                             'resp_E','resp_S','resp_F',
                                             'resp_BA','resp_FA','resp_NFA',
                                             'resp_EBA')]
  
  return(model_predictions)
}

plt_data <- function(dat, x, y) {
  
  return(
    ggplot(dat, aes_string(x, y, group = 'scenario', color = 'condition')) +
      stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar', width = .1) +
      stat_summary(fun = 'mean', geom = 'point') +
      stat_summary(fun = 'mean', geom = 'line') +
      coord_cartesian(ylim = c(0, 10)) +
      theme_bw()
  )
}

plt_model_predictions <- function(df, x, y) {
  
  return(
    ggplot(df, aes_string(x, y, group = 'scenario', color = 'condition')) +
      geom_line() +
      geom_point() +
      scale_color_discrete(name = 'Condition', breaks = c('fail', 'lift'), labels = c('Fail', 'Lift')) +
      coord_cartesian(ylim = c(0, 10)) +
      theme_bw()
  )
}

line_plots <- function(dat, model_predictions, idx) {
  
  subplot_a1 <- plt_data(dat %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp')
  subplot_a2 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp_E')
  subplot_a3 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp_BA')
  subplot_a4 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp_EBA') +
    facet_wrap(~ type, strip.position ='right')
  
  subplot_b1 <- plt_data(dat %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp')
  subplot_b2 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp_E')
  subplot_b3 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp_BA')
  subplot_b4 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp_EBA') +
    facet_wrap(~ type, strip.position ='right')
  
  subplot_c1 <- plt_data(dat %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp')
  subplot_c2 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp_E')
  subplot_c3 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp_BA')
  subplot_c4 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp_EBA') +
    facet_wrap(~ type, strip.position ='right')
  
  return(
    subplot_a1 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = 'Responsibility', title = 'Data') + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
      subplot_a2 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = NULL, title = 'Effort model') + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
      subplot_a3 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = NULL, title = 'BA counterfactual model') + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
      subplot_a4 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = NULL, title = 'Ensemble model') +
      theme(plot.title = element_text(hjust = 0.5)) +
      
      subplot_b1 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = 'Responsibility') + 
      theme(legend.position = "none") +
      subplot_b2 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = NULL) + 
      theme(legend.position = "none") +
      subplot_b3 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = NULL) + 
      theme(legend.position = "none") +
      subplot_b4 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = NULL) +
      theme(legend.position = "none") +
      
      subplot_c1 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = 'Responsibility') + 
      theme(legend.position = "none") +
      subplot_c2 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = NULL) + 
      theme(legend.position = "none") +
      subplot_c3 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = NULL) + 
      theme(legend.position = "none") +
      subplot_c4 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = NULL) +
      theme(legend.position = "none") +
      
      plot_layout(ncol = 4)
  )
}

cor_plt <- function(data, x, y, ymin, ymax, text_data) {
  
  return(
    ggplot(data, aes_string(x, y, color = 'condition')) +
      geom_errorbar(aes_string(ymin = ymin, ymax = ymax, width = 0)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, lty = 3) +
      scale_color_discrete(name = 'Condition', breaks = c('fail', 'lift'), labels = c('Fail', 'Lift')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(data = text_data,
                mapping = aes(label = text, x = x , y = y),
                inherit.aes = F,
                fontface='italic') +
      facet_grid(exp_index ~ model, 
                 labeller = as_labeller(c(`1a`='Exp 1a', `1b`='Exp 1b', `2a`='Exp 2a', `2b`='Exp 2b', `3`='Exp 3',
                                          `F`='Force', `S`='Strength', `E` = 'Effort', `EBA` = 'Ensemble',
                                          `FA` = 'FA counterfactual', `NFA` = 'NFA counterfactual', `BA` = 'BA counterfactual',
                                          `0.5` = '0.5', `0.3` = '0.3', `0.4` = '0.4', `0.6` = '0.6', `0.7` = '0.7')))
  )
}

## Supplement: Line plots of production-style models
line_plots_production <- function(dat, model_predictions, idx) {
  
  subplot_a1 <- plt_data(dat %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp')
  subplot_a2 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp_F')
  subplot_a3 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp_S')
  subplot_a4 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp_E') +
    facet_wrap(~ type, strip.position ='right')
  
  subplot_b1 <- plt_data(dat %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp')
  subplot_b2 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp_F')
  subplot_b3 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp_S')
  subplot_b4 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp_E') +
    facet_wrap(~ type, strip.position ='right')
  
  subplot_c1 <- plt_data(dat %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp')
  subplot_c2 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp_F')
  subplot_c3 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp_S')
  subplot_c4 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp_E') +
    facet_wrap(~ type, strip.position ='right')
  
  return(
    subplot_a1 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = 'Responsibility', title = 'Data') + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
      subplot_a2 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = NULL, title = 'Force model') + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
      subplot_a3 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = NULL, title = 'Strength model') + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
      subplot_a4 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = NULL, title = 'Effort model') +
      theme(plot.title = element_text(hjust = 0.5)) +
      
      subplot_b1 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = 'Responsibility') + 
      theme(legend.position = "none") +
      subplot_b2 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = NULL) + 
      theme(legend.position = "none") +
      subplot_b3 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = NULL) + 
      theme(legend.position = "none") +
      subplot_b4 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = NULL) +
      theme(legend.position = "none") +
      
      subplot_c1 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = 'Responsibility') + 
      theme(legend.position = "none") +
      subplot_c2 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = NULL) + 
      theme(legend.position = "none") +
      subplot_c3 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = NULL) + 
      theme(legend.position = "none") +
      subplot_c4 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = NULL) +
      theme(legend.position = "none") +
      
      plot_layout(ncol = 4)
  )
}

## Supplement: Line plots of counterfactual-style models
line_plots_counterfactual <- function(dat, model_predictions, idx) {
  
  subplot_a1 <- plt_data(dat %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp')
  subplot_a2 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp_FA')
  subplot_a3 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp_NFA')
  subplot_a4 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & s1==s2), 'contestant', 'resp_BA') +
    facet_wrap(~ type, strip.position ='right')
  
  subplot_b1 <- plt_data(dat %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp')
  subplot_b2 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp_FA')
  subplot_b3 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp_NFA')
  subplot_b4 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & e1==e2), 'contestant', 'resp_BA') +
    facet_wrap(~ type, strip.position ='right')
  
  subplot_c1 <- plt_data(dat %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp')
  subplot_c2 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp_FA')
  subplot_c3 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp_NFA')
  subplot_c4 <- plt_model_predictions(model_predictions %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', 'resp_BA') +
    facet_wrap(~ type, strip.position ='right')
  
  return(
    subplot_a1 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = 'Responsibility', title = 'Data') + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
      subplot_a2 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = NULL, title = 'FA counterfactual model') + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
      subplot_a3 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = NULL, title = 'NFA counterfactual model') + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
      subplot_a4 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) + 
      labs(x = NULL, y = NULL, title = 'BA counterfactual model') +
      theme(plot.title = element_text(hjust = 0.5)) +
      
      subplot_b1 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = 'Responsibility') + 
      theme(legend.position = "none") +
      subplot_b2 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = NULL) + 
      theme(legend.position = "none") +
      subplot_b3 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = NULL) + 
      theme(legend.position = "none") +
      subplot_b4 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) + 
      labs(x = NULL, y = NULL) +
      theme(legend.position = "none") +
      
      subplot_c1 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = 'Responsibility') + 
      theme(legend.position = "none") +
      subplot_c2 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = NULL) + 
      theme(legend.position = "none") +
      subplot_c3 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = NULL) + 
      theme(legend.position = "none") +
      subplot_c4 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) + 
      labs(x = 'Contestant', y = NULL) +
      theme(legend.position = "none") +
      
      plot_layout(ncol = 4)
  )
}

## Supplement: Ensemble models with different weightings
ensemble_models <- function(data, w) {
  
  data$force <- data$strength*data$effort/100
  data$total_force <- data$s1*data$e1/100 + data$s2*data$e2/100  
  df <- data %>% filter(subject == 1) %>% mutate(subject = NULL)
  
  df_fail <- df %>% filter(condition == 'fail')
  df_fail$probability <- apply(array(1:dim(df_fail)[1]), 1, function(i){
    mean(apply(array(seq(from = 1, to = 100 - df_fail$effort[i], by = 1)), 1, function(x){
      x/100*df_fail$strength[i] + df_fail$total_force[i] >= df_fail$box_weight[i]
    }))
  })
  
  df_lift <- df %>% filter(condition == 'lift')
  df_lift$probability <- apply(array(1:dim(df_lift)[1]), 1, function(i){
    mean(apply(array(seq(from = 1, to = df_lift$effort[i], by = 1)), 1, function(x){
      df_lift$total_force[i] - x/100*df_lift$strength[i] < df_lift$box_weight[i]
    }))
  })
  
  df <- rbind(df_fail, df_lift)
  
  df_flipped <- df %>% 
    mutate(contestant = 3-contestant) %>% 
    mutate(resp_NFA = (1-probability)*10) %>% 
    select(exp_index, scenario, contestant, resp_NFA)
  df <- merge(df, df_flipped)
  
  model_predictions <- df %>% 
    mutate(resp_E = case_when(condition == 'fail' ~ (100-effort)/10,
                              condition == 'lift' ~ effort/10),
           resp_FA = probability*10,
           resp_BA = (resp_FA + resp_NFA)/2,
           resp_EBA = w*resp_E + (1-w)*resp_BA)
  model_predictions <- arrange(model_predictions, exp_index, scenario)
  model_predictions <- model_predictions[, c('exp_index','condition','scenario','box_weight','contestant',
                                             'strength','effort','s1','s2','e1','e2',
                                             'resp_EBA')]
  
  return(model_predictions)
}
