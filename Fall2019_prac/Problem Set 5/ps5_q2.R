# Stats 506, Fall 2019
# Problem Set 5, Question 2
#
# This script contains functions for interpreting the 2015 RECS survey data 
#
# Author: Sijun Zhang (randyz@umich.edu) (umid:889934761)
# Date: Dec 1, 2019
#80: ---------------------------------------------------------------------------
#b.
library(data.table)
library(ggplot2)
gse = fread("GSE138311_series_matrix.txt", skip = 68)

gse_b = gse[grep("^ch", ID_REF),
        ][, -("GSM4105199"), with = FALSE]
gse_b = melt(gse_b, id.vars = c("ID_REF"))

head(gse_b)

#c. label the diseased sample as sample_group 1 and the other as 0
label_disease = function(x) {
  if (x %in% c("GSM4105187", "GSM4105188", "GSM4105189",
               "GSM4105190", "GSM4105191", "GSM4105192",
               "GSM4105193")) { re = 1}
  else {re = 0}
  return(re)
}

label_disease_all = function(x) {
  return(sapply(x,label_disease))
}

gse_c = gse_b[, `:=`(sample_group = label_disease_all(variable))]
head(gse_c)

#d. 
calc_t = function(mu,n,s) {
  sp = sqrt( ((n[1]-1)*(s[1])^2 + (n[2]-1)*(s[2])^2 ) / (sum(n)-2) )
  se = sp * sqrt(1/n[1] + 1/n[2])
  t = (mu[1] - mu[2]) / se
  return(t)
}

gse_d = gse_c[, .(mu = mean(value), n = .N, s = sd(value)), by=.(ID_REF, sample_group) 
        ][, .(t_score = calc_t(mu,n,s)), by=.(ID_REF) ]
head(gse_d)

#e.
gse_e = gse_d[,`:=`(probe_group = substr(ID_REF,  1, 5))]

#f.
gse_f = gse_e[, `:=`(critical_abs_value = qt(0.975, df=10))
        ][, `:=`(index = (abs(t_score) > critical_abs_value)) 
        ][, .(prop = sum(index) / .N), by=.(probe_group)]

ggplot(gse_f, aes(x = probe_group, y = prop)) +
  geom_point()

# plot(as.factor(as.matrix(gse_f[,1])), sapply(gse_f[,2], as.numeric), 
#      ylab = "proportion of signifcant probes", xlab = "probe_group")

#g.

# declare the sample_id for reference
crohn_ind = append(rep(1,7), rep(0,5))
sample_id = c("GSM4105187", "GSM4105188", "GSM4105189",
              "GSM4105190", "GSM4105191", "GSM4105192","GSM4105193",
              "GSM4105194", "GSM4105195", "GSM4105196", "GSM4105197", "GSM4105198")

permutation_test = function(df, type = "two-tailed", 
                            permuate = TRUE, alpha = 0.05){
  if (!permuate){
    # same process in d.
    df_est = df[, .(mu = mean(value), n = .N, s = sd(value)), by=.(ID_REF, sample_group) 
                ][, .(t_score = calc_t(mu,n,s)), by=.(ID_REF) 
                  ][,`:=`(probe_group = substr(ID_REF,  1, 5))]
  }
  if (permuate){
    # permutate the sample_group using merge
    sample_group = sample(crohn_ind, size = 12, replace = FALSE)
    sample_group_table = data.table(cbind(sample_id ,sample_group))
    df_est = df[, .(ID_REF, sample_id = variable,value)]
    # give the df permuated sample_group
    df_est = merge(df_est,sample_group_table, by = 'sample_id', all.x = TRUE)
    df_est = df_est[, .(mu = mean(value), n = .N, s = sd(value)), by=.(ID_REF, sample_group)
                    ][, .(t_score = calc_t(mu,n,s)), by=.(ID_REF)
                      ][,`:=`(probe_group = substr(ID_REF,  1, 5))]
  }
  if(type == "two-tailed"){
    df_est = df_est[, `:=`(index = (abs(t_score) > qt(1-alpha/2, df=10)))
                    ][, .(T_abs = mean(index*abs(t_score))), by=.(probe_group)]
  } else if (type == "greater"){
    df_est = df_est[, `:=`(index = ((t_score) > qt(1-alpha, df=10)))
                    ][, .(T_up = mean(index*(t_score))), by=.(probe_group)]
  } else {
    df_est = df_est[, `:=`(index = ((t_score) < qt(alpha, df=10)))
                    ][, .(T_down = mean(index*(t_score))), by=.(probe_group)]
  }
  return(df_est)
}

#h.
set.seed(5)
T_abs_original = permutation_test(gse_c, type = "two-tailed", permuate = FALSE)
p_value_index = T_abs_original[, .(probe_group, ind = 0)]
start_time=Sys.time()
for (i in 1:1000) {
  T_abs_permuated = permutation_test(gse_c, type = "two-tailed", permuate = TRUE)
  p_value_index_i = merge(T_abs_permuated, T_abs_original, 
                          all.x = TRUE, by='probe_group')
  # calc whether the observed Tabs score for each group is larger than the expectation
  p_value_index_i = p_value_index_i[,.(probe_group, ind_i = T_abs.x >= T_abs.y)]
  # cumulating the larger or not result
  p_value_index = merge(p_value_index, p_value_index_i, all.x = TRUE, by='probe_group')[
    ,.(probe_group,ind = ind+ind_i)]
}
end_time=Sys.time()
for_loop_time = end_time-start_time
p_value_index = p_value_index[, .(probe_group, p_value = (ind+1)/1001)]
cat("The time taken to compute 1,000 permutation in for-loop is shown below \n")
for_loop_time

knitr::kable(p_value_index)
ggplot(p_value_index, aes(x = probe_group, y = p_value)) +
  geom_point(shape=16)


#i.
library(parallel)
set.seed(6925)
p_value_index = T_abs_original[, .(probe_group, ind = 0)]
T_up_original = permutation_test(gse_c, type = "greater", permuate = FALSE)
permutating_T_up = function(i, x) {
  T_up_permuated = permutation_test(x, type = "greater", permuate = TRUE)
  return(T_up_permuated)
}
# parallel computing the T_up
start_time=Sys.time()
T_up_permuated_all = mclapply(1:1000, permutating_T_up, x=gse_c)
end_time=Sys.time()
# calc the p_value
for (i in 1:1000) {
  T_up_permuated = T_up_permuated_all[[i]]
  p_value_index_i = merge(T_up_permuated, T_up_original, 
                          all.x = TRUE, by='probe_group')
  # calc whether the observed Tup score for each group is larger than the expectation
  p_value_index_i = p_value_index_i[,.(probe_group, ind_i = T_up.x >= T_up.y)]
  # cumulating the larger or not result
  p_value_index = merge(p_value_index, p_value_index_i, all.x = TRUE, by='probe_group')[
    ,.(probe_group,ind = ind+ind_i)]
}
mclapply_time = end_time-start_time
p_value_index = p_value_index[, .(probe_group, p_value = (ind+1)/1001)]
cat("The time taken to compute 1,000 permutation in mclapply_time is shown below \n")
mclapply_time

knitr::kable(p_value_index)
ggplot(p_value_index, aes(x = probe_group, y = p_value)) +
  geom_point(shape=16)

#j. split the task into 2 sub-task using future
library(future)
plan(multisession)
set.seed(6925)
p_value_index = T_abs_original[, .(probe_group, ind = 0)]
T_down_original = permutation_test(gse_c, type = "lesser", permuate = FALSE)
permutating_T_down = function(x) {
  T_down_permuated = permutation_test(x, type = "lesser", permuate = TRUE)
  return(T_down_permuated)
}
# parallel computing the T_down
start_time=Sys.time()
T_up_permuated_all_1 %<-% {
  c = list()
  for (i in 1:500) {c[[i]] = permutating_T_down(gse_c)}
  c
}
T_up_permuated_all_2 %<-% {
  c = list()
  for (i in 1:500) {c[[i]] = permutating_T_down(gse_c)}
  c
}
T_down_permuated_all = c(T_up_permuated_all_1, T_up_permuated_all_2)
end_time=Sys.time()
future_time = end_time - start_time
# calc the p_value
for (i in 1:1000) {
  T_down_permuated = T_down_permuated_all[[i]]
  p_value_index_i = merge(T_down_permuated, T_down_original, 
                          all.x = TRUE, by='probe_group')
  # calc whether the observed Tup score for each group is larger than the expectation
  # according to the resampling method, as the value of T_down all smaller than or equal to 0
  # and the p_est compare the absolute value of statistics, we change the >= to <= here.
  p_value_index_i = p_value_index_i[,.(probe_group, ind_i = T_down.x <= T_down.y)]
  # cumulating the larger or not result
  p_value_index = merge(p_value_index, p_value_index_i, all.x = TRUE, by='probe_group')[
    ,.(probe_group,ind = ind+ind_i)]
}
p_value_index = p_value_index[, .(probe_group, p_value = (ind+1)/1001)]
cat("The time taken to compute 1,000 permutation in mclapply_time is shown below \n")
future_time

knitr::kable(p_value_index)
ggplot(p_value_index, aes(x = probe_group, y = p_value)) +
  geom_point(shape=16)


