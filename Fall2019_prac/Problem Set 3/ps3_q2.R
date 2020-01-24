# Problem Set 3
# Stats 506, Fall 2019
#
# Updated: November 3, 2019
# Author: Sijun Zhang 89934761

# Part a -- Jacknife for MC
mc_jacknife_ci = function(x, y, alpha = 0.05) {
  # if we have ntot samples and nx observations within each x sample,
  # ny observations within each y samples, when dim(x) = c(ntot, nx),
  # dim(y) = c(ntot, ny), the code will be efficient (row represents samples).
  ntot = dim(x)[1]
  nx = dim(x)[2]
  ny = dim(y)[2]
  x_s = rowSums(x)
  y_s = rowSums(y)
  x_m = x_s - x
  y_m = y_s - y
  theta_i_c_x = (x_m / (nx - 1)) / rowMeans(y)
  theta_i_c_y = rowMeans(x) / (y_m / (ny - 1))
  theta_i_c = cbind(theta_i_c_x, theta_i_c_y)
  sums = rowSums((theta_i_c - rowMeans(theta_i_c))^2)
  sigma = sqrt((nx + ny -1) / (nx + ny) * sums)
  return(cbind(matrix(rowMeans(x)/rowMeans(y) + qnorm(alpha/2)*sigma, ncol = 1),
           matrix(rowMeans(x)/rowMeans(y) + qnorm(1-alpha/2)*sigma, ncol = 1) ) )
}

x = matrix(1:4, nrow = 2, byrow = TRUE)
y = matrix(1:6, nrow = 2, byrow = TRUE)
mc_jacknife_ci(x,y)

# Part b -- Bootstrap for MC
mc_bootstrap_ci = function(x, y, nboot = 1e4, alpha = 0.05) {
  ntot = dim(x)[1]
  nx = dim(x)[2]
  ny = dim(y)[2]
  # Vectorized sample using index reference
  index_x = sample(1:nx, nboot * nx, replace = TRUE)
  index_y = sample(1:ny, nboot * ny, replace = TRUE)
  dim(index_x) = c(nx * nboot, 1)
  index_shifter_x = matrix(1, ncol = ntot, nrow = nx * nboot) %*% diag(0:(ntot-1)) * nx
  index_x = as.numeric(index_x) + as.numeric(index_shifter_x)
  x_m = matrix(t(x)[index_x], byrow = TRUE, nrow = ntot * nboot)
  dim(index_y) = c(ny * nboot, 1)
  index_shifter_y = matrix(1, ncol = ntot, nrow = ny * nboot) %*% diag(0:(ntot-1)) * ny
  index_y = as.numeric(index_y) + as.numeric(index_shifter_y)
  y_m = matrix(t(y)[index_y], byrow = TRUE, nrow = ntot * nboot)
  # calc theta bootstrap sets and normal approx standard error
  x_m_mean = matrix(rowMeans(x_m), byrow = TRUE, nrow = ntot)
  y_m_mean = matrix(rowMeans(y_m), byrow = TRUE, nrow = ntot)
  theta_i_star = x_m_mean / y_m_mean
  sigma = diag(sqrt(var(t(theta_i_star))))
  # percentile method
  re1 = unname(t(apply(theta_i_star, 1, quantile, c(alpha/2, 1-alpha/2) )) )
  # basic bootstrap
  re2 = cbind(matrix((2*rowMeans(x)/rowMeans(y) - re1)[,2], ncol = 1),
              matrix((2*rowMeans(x)/rowMeans(y) - re1)[,1], ncol = 1))
  # normal approx
  re3 = cbind(matrix(rowMeans(x)/rowMeans(y)-sigma*qnorm(1-alpha/2), ncol = 1),
              matrix(rowMeans(x)/rowMeans(y)-sigma*qnorm(alpha/2), ncol = 1))
  # combine result
  re = cbind(re1, re2)
  re = cbind(re, re3)
  return(re)
}
mc_bootstrap_ci(x,y)

# Part c -- Verification

# In this part, we assume each x_i and y_i all follow normal distributions and the
# expaction of x is e_x = 3, the expectation of y is e_y = 4, and their variance
# is 2 and 3, respectively, we generated ntot = 1e2 mc samples to find the ratio
# theta = E[X] / E[Y], when calculating the confidence interval, we use the default
# confidence level alpha = 0.05 and use nboot = 10000 to generate bootstrap samples.

nx = 10
ny = 20
e_x = 3
e_y = 4
theta = e_x / e_y
ntot = 1e2
x = rnorm(nx*ntot, mean = e_x, sd = 2)
dim(x) = c(ntot, nx)
y = rnorm(ny*ntot, mean = e_y, sd = 3)
dim(y) = c(ntot, ny)

jk_ci = mc_jacknife_ci(x,y)
bs_ci = mc_bootstrap_ci(x,y)
ci = cbind(jk_ci, bs_ci)

# The coverage probability, the true value of theta is 3/4
cov_prob = cbind(cbind(sum((theta >= ci[,1]) & (theta <= ci[,2])),
                       sum((theta >= ci[,3]) & (theta <= ci[,4]))),
                 cbind(sum((theta >= ci[,5]) & (theta <= ci[,6])),
                       sum((theta >= ci[,7]) & (theta <= ci[,8]))))/ntot
# The average length of confidence interval
avg_len = cbind(cbind(mean(ci[,2] - ci[,1]),
                      mean(ci[,4] - ci[,3])),
                cbind(mean(ci[,6] - ci[,5]),
                      mean(ci[,8] - ci[,7])))
# The average shape of the confidence intervals produced by each method,
point_est = rowMeans(x) / rowMeans(y)
len_ratio = cbind(cbind(mean((ci[,2] - point_est)/(point_est - ci[,1])),
                        mean((ci[,4] - point_est)/(point_est - ci[,3]))),
                  cbind(mean((ci[,6] - point_est)/(point_est - ci[,5])),
                        mean((ci[,8] - point_est)/(point_est - ci[,7]))))
quantities = rbind(rbind(cov_prob,
                         avg_len),
                   len_ratio)
quantities = as.data.frame(quantities, row.names = c("cov_prob",
                                                     "avg_len",
                                                     "len_ratio"))
names(quantities) = c("Jacknife", "Percentile_bootstrap",
                      "Basic_bootstrap", "Normal_approx_bootstrap")
knitr::kable(quantities)
