# Stats 506, Fall 2019
# Problem Set 2, Question 2
#
# This script contains functions return to data from mouse-tracking 
# experiments of the type encountered in problem set one.
#
# Author: Sijun Zhang (randyz@umich.edu) (umid:889934761)
# Date: Oct 10, 2019
#80: ---------------------------------------------------------------------------

# libraries including ----------------------------------------------------------
source('./ps2_q2_funcs.R', encoding = 'UTF-8')
library(tidyverse)
library(mousetrap)
library(lme4)


# read in the  data: -----------------------------------------------------------

factor_to_numeric_list = function(x) {
  tmp = str_replace(x, '(\\[)', '') %>%
    str_replace('(\\])', '') %>%
    str_split(', ')
  return(sapply(tmp, as.numeric))
}


data_prep = function(x) {
  return(
    x %>%
      transmute(subject = subject_nr, trial = count_trial, 
                x = factor_to_numeric_list(xpos_get_response), 
                y = factor_to_numeric_list(ypos_get_response),
                tm = factor_to_numeric_list(timestamps_get_response),
                correct)
  )
}
    
sakh <- data_prep(mousetrap::KH2017_raw)  

# apply measures ----------------------------------------------------------------

normalize_dim3 = function(x,y,t) {
  a = unname(unlist(x))
  b = unname(unlist(y))
  c = unname(unlist(t))
  tmp = unname(cbind(a,b,c))
  return(list(normalize(tmp)) )
}

measure_curv_list = function(x) {
  tmp = (matrix(unlist(x), ncol = 3))
  return(list(measure_curvature(tmp)) )
}

unlist_curv = function(x,f) {
  tmp = unname(unlist(x))
  if(f == 'tot_dist') return(tmp[1])
  if(f == 'max_abs_dev') return(tmp[2])
  if(f == 'avg_abs_dev') return(tmp[3])
  if(f == 'AUC') return(tmp[4])
}

norm_sakh <- sakh %>%
  filter(correct == 1) %>%
  group_by(subject, trial) %>%
  summarize(norm_xyt = normalize_dim3(x,y,tm)) 

meas <- norm_sakh %>%
  group_by(subject,trial) %>%
  summarize(curv = measure_curv_list(norm_xyt) ) %>% 
  group_by(subject,trial) %>%
  summarize( tot_dist = unlist_curv(curv, 'tot_dist'),
         max_abs_dev = unlist_curv(curv, 'max_abs_dev'),
         avg_abs_dev = unlist_curv(curv, 'avg_abs_dev'),
         AUC = unlist_curv(curv, 'AUC')
         )
  
con_expl <- mousetrap::KH2017_raw %>%
  transmute(subject = subject_nr, trial = count_trial,
            Condition = Condition, Exemplar = Exemplar,correct) %>%
  filter(correct == 1) %>%
  select(-correct)

meas <- meas %>%
  left_join(con_expl, by=c('subject', 'trial'))
    
# fit linear mixed model ----------------------------------------------------

meas_fitting <- meas %>%
  filter(AUC > 0) %>%
  group_by(subject,trial) %>%
  summarize(tot_dist_log = log(tot_dist),
            max_abs_dev_log = log(max_abs_dev),
            avg_abs_dev_log = log(avg_abs_dev),
            AUC_log = log(AUC),
            Condition = factor(Condition, levels = c('Typical','Atypical'),
                               labels = c(0,1) ),
            Exemplar = factor(Exemplar, levels = c('Aal','Alligator', 'Chamaeleon',
                                                   'Falke', 'Fledermaus',
                                                   'Goldfisch','Hai','Hund',
                                                   'Kaninchen','Katze',
                                                   'Klapperschlange','Lachs',
                                                   'Loewe','Pferd','Pinguin',
                                                   'Schmetterling','Seeloewe',
                                                   'Spatz','Wal'),
                              labels = c(1:19)))

m1 <- lmer(tot_dist_log~Condition+(1|Exemplar)+(1|subject), data = meas_fitting )
m2 <- lmer(max_abs_dev_log~Condition+(1|Exemplar)+(1|subject), data = meas_fitting )
m3 <- lmer(avg_abs_dev_log~Condition+(1|Exemplar)+(1|subject), data = meas_fitting )
m4 <- lmer(AUC_log~Condition+(1|Exemplar)+(1|subject), data = meas_fitting )

re1 <- cbind( matrix(lme4::fixef(m1)[2]), matrix(confint(m1)['Condition1',],nrow=1), mean(fitted.values(m1)) ) 
re2 <- cbind( matrix(lme4::fixef(m2)[2]), matrix(confint(m2)['Condition1',],nrow=1), mean(fitted.values(m2)) ) 
re3 <- cbind( matrix(lme4::fixef(m3)[2]), matrix(confint(m3)['Condition1',],nrow=1), mean(fitted.values(m3)) ) 
re4 <- cbind( matrix(lme4::fixef(m4)[2]), matrix(confint(m4)['Condition1',],nrow=1), mean(fitted.values(m4)) ) 

re <- data.frame(rbind(re1,re2,re3,re4), row.names = c('tot_dist_log', 'max_abs_dev_log',
                                                       'avg_abs_dev_log','AUC_log'))
re <- cbind(re, exp(re[,1:4]) )
re <- cbind(re, re[5] / re[8])
names(re) <- c('Coef of Condition log', '2.5% lwr log', '97.5% upr log', 'mean log',
               'Coef of Condition', '2.5% lwr', '97.5% upr', 'mean', 'Coef / mean')




