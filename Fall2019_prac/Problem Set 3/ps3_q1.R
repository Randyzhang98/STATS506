# Problem Set 3
# Stats 506, Fall 2019
#
# Updated: November 3, 2019
# Author: Sijun Zhang 89934761

# Part a -- Jacknife Method
jacknife_ci = function(x, y, alpha = 0.05) {
  nx = length(x)
  ny = length(y)
  x_s = sum(x)
  y_s = sum(y)
  theta_i_c_x = ((x_s - x) / (nx - 1)) / mean(y)
  theta_i_c_y = mean(x) / ((y_s - y) / (ny - 1))
  theta_i_c = append(theta_i_c_x, theta_i_c_y)
  sigma = sqrt((nx + ny -1) / (nx + ny) * sum( (theta_i_c - mean(theta_i_c))^2) )
  return( list(c(mean(x)/mean(y) + qnorm(alpha/2)*sigma, 
           mean(x)/mean(y) + qnorm(1-alpha/2)*sigma)) )
}

x = matrix(1:4, nrow = 2, byrow = TRUE)
y = matrix(1:6, nrow = 2, byrow = TRUE)
jacknife_ci(x[1,],y[1,])

# Part b -- Bootstrap
boot_ci = function(x, y, nboot = 1e4, alpha = 0.05) {
  # method could take "percentile method", "basic bootstrap" and
  # "normal approximation"
  nx = length(x)
  ny = length(y)

  x_m = sample(x, nboot * nx, replace = TRUE)
  y_m = sample(y, nboot * ny, replace = TRUE)
  dim(x_m) = c(nboot, nx)
  dim(y_m) = c(nboot, ny)
  x_m_mean = rowMeans(x_m)
  y_m_mean = rowMeans(y_m)
  theta_i_star = x_m_mean / y_m_mean
  sigma = sqrt(var(theta_i_star))
  
  # cat("The percentile method confience interval is: (")
  # cat(quantile(theta_i_star, c(alpha/2, 1-alpha/2)))
  # cat(') \n')
  # cat("The basic bootstrap confience interval is: (")
  # cat(2*mean(x)/mean(y) - quantile(theta_i_star, c(1-alpha/2, alpha/2)) )
  # cat(') \n')
  # cat("The normal approximation with bootstrap standard error: (")
  # cat( mean(x)/mean(y)-qnorm(1-alpha/2)*sigma, mean(x)/mean(y)+qnorm(1-alpha/2)*sigma)
  # cat(') \n')
  
  # percentile method and basic bootstrap
  re = append(quantile(theta_i_star, c(alpha/2, 1-alpha/2)),
              2*mean(x)/mean(y) - quantile(theta_i_star, c(1-alpha/2, alpha/2)))
  # normal approximation
  re = append(re, c(mean(x)/mean(y)-qnorm(1-alpha/2)*sigma) )
  re = append(re, c(mean(x)/mean(y)+qnorm(1-alpha/2)*sigma) )
  return(list(unname(re)))
}
boot_ci(x[1,],y[1,])

# Part c -- ToothGrowth Stimulation
library(tidyverse)
data("ToothGrowth")
TG = tibble::as_tibble(ToothGrowth)
CI_TG = TG %>% 
  # select(len, supp) %>%
  group_by(dose) %>%
  pivot_wider(values_from = len, names_from = supp) %>%
  summarize(jacknife_ci = jacknife_ci(unlist(OJ), unlist(VC)),
            bootstrap_ci = boot_ci(unlist(OJ), unlist(VC)),
            point_est = mean(unlist(OJ)) / mean(unlist(VC))) %>%
  group_by(dose) %>%
  mutate(jacknife_lcb = unlist(jacknife_ci)[1],
         jacknife_ucb = unlist(jacknife_ci)[2],
         percentile_lcb = unlist(bootstrap_ci)[1],
         percentile_ucb = unlist(bootstrap_ci)[2],
         basic_bs_lcb = unlist(bootstrap_ci)[3],
         basic_bs_ucb = unlist(bootstrap_ci)[4],
         normal_lcb = unlist(bootstrap_ci)[5],
         normal_ucb = unlist(bootstrap_ci)[6]) %>%
  select(-jacknife_ci, -bootstrap_ci)
knitr::kable(CI_TG)


