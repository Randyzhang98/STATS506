# Stats 506, Fall 2019
# Problem Set 5, Question 1
#
# This script contains functions for interpreting the 2015 RECS survey data 
#
# Author: Sijun Zhang (randyz@umich.edu) (umid:889934761)
# Date: Dec 1, 2019
#80: ---------------------------------------------------------------------------
#a.
library(data.table)

# read in the  data: -----------------------------------------------------------
## This data will be used in the question. 
url_base <- 'https://www.eia.gov/consumption/residential/data/2015/csv/'

recs_file <- './recs2015_public_v4.csv'
if ( !file.exists(recs_file) ) {
  recs_url <- sprintf('%s/recs2015_public_v4.csv', url_base)
  recs <- readr::read_delim(recs_url, delim = ',')
  readr::write_delim(recs, path = recs_file, delim = ',')
} else {
  recs <- readr::read_delim(recs_file, delim = ',')
}
recs = data.table(recs)

decode_division = function(x) {
  if(!is.numeric(x)) stop('decode_divsion expects numeric input indexed from 1!')
  y <- x
  re <- switch (y,
                'New England',
                'Middle Atlantic',
                'East North Central',
                'West North Central',
                'South Atlantic',
                'East South Central',
                'West South Central',
                'Mountain North',
                'Mountain South',
                'Pacific')
  return(re)
}

decode_all_division = function(x) {
  return(sapply(x,decode_division))
}

collapse_UC = function(x) {
  if (x == "C") {re <- "U"}
  else {re <- x}
  return(re)
}

collapse_all_UC = function(x) {
  sapply(x, collapse_UC)
}

# Key values for each observation: --------------------------------------------
internet_recs <- recs[, .(DOEID = DOEID, dvi = decode_all_division(DIVISION),
                          utype = collapse_all_UC(UATYP10),
                          internet = INTERNET, weight = NWEIGHT)]

# Convert weights to long: ----------------------------------------------------
brrwts = names(recs)[which(names(recs) == "BRRWT1"):
                       which(names(recs) == "BRRWT96")]

weights_long <- recs[, .SD, .SDcols = c(1, which(names(recs) == "BRRWT1"):
                                          which(names(recs) == "BRRWT96"))]
weights_long <- melt(weights_long, id.vars = c("DOEID"), measure.vars = brrwts)

# Join home type to weights: --------------------------------------------------
internet_weighted <- merge(weights_long, internet_recs, by='DOEID', all.x = TRUE)

if( nrow(weights_long) != nrow(internet_weighted) ) {
  stop("DOEID mismatch!")
}

internet_weighted <- internet_weighted[, `:=`(brrwt_with_internet = internet*value,
                                              nw_with_internet = internet*weight)
                     ][, .(brrwt = sum(value), nweight = sum(weight),
                       nw_with_internet = sum(nw_with_internet),
                       brrwt_with_internet = sum(brrwt_with_internet),
                       repl = variable), by=.(dvi, utype, variable) 
                     ][, `:=`(prop_repl = brrwt_with_internet/brrwt,
                       prop = nw_with_internet/nweight) 
                     ][, .(dvi, utype, repl, prop_repl, prop)]


# calc the confidence interval using brrwt replications
internet_weighted <- dcast(internet_weighted, dvi+repl~utype, value.var=c("prop_repl","prop") )

internet_weighted_ci <-  internet_weighted[, `:=`(diff_prop = prop_U-prop_R, 
                                                  diff_prop_repl = prop_repl_U-prop_repl_R) 
                         ][, `:=`(rsq_prop1 = (prop_U-prop_repl_U)^2/(1-0.5)^2,
                           rsq_prop2 = (prop_R-prop_repl_R)^2/(1-0.5)^2,
                           rsq_prop_diff = (diff_prop-diff_prop_repl)^2/(1-0.5)^2) 
                         ][, .(prop_U = mean(prop_U),  prop_R = mean(prop_R),
                           diff_prop = mean(diff_prop), stderr_prop1 = sqrt(mean(rsq_prop1)), 
                           stderr_prop2 = sqrt(mean(rsq_prop2)), 
                           stderr_prop_diff = sqrt(mean(rsq_prop_diff))), by = .(dvi)
                         ][, `:=`(diff_prop_lwr = diff_prop - 1.96*(stderr_prop_diff), 
                           diff_prop_upr = diff_prop + 1.96*(stderr_prop_diff)) 
                         ][order(-diff_prop)]

# tabling 
options(digits = 4)
knitr::kable(internet_weighted_ci)
internet_weighted_ci[which( internet_weighted_ci[,"diff_prop"] == max(internet_weighted_ci[,"diff_prop"])),1]

# graphing
library(ggplot2)
graph_tmp = internet_weighted_ci[,`:=`(measure = factor(dvi, levels = unique(dvi) ))]

ggplot(graph_tmp, aes( y = measure, x = diff_prop) ) +
  geom_point() +
  geom_errorbarh( aes(xmin = diff_prop_lwr, xmax = diff_prop_upr) ) + 
  geom_vline( xintercept = 0, lty = 'dashed') + 
  xlab('the disparity between the urban proportion and rural proportion') + 
  ylab('') +
  theme_bw()




