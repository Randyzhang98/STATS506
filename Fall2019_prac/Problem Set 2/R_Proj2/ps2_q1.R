# Stats 506, Fall 2019
# Problem Set 2, Question 1
#
# This script contains functions for interpreting the 2015 RECS survey data 
#
# Author: Sijun Zhang (randyz@umich.edu) (umid:889934761)
# Date: Oct 10, 2019
#80: ---------------------------------------------------------------------------
#a.
library(tidyverse)

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

# Input: recc2015_public_v4.csv
# Output: the national average home temperature at night in winter, 
# among homes that use space heating

# Key values for each observation: --------------------------------------------
winter_tmp_heated <- recs %>%
  transmute(DOEID, winter_tmp = TEMPNITE, space_heated = HEATHOME, weight = NWEIGHT) %>%
  filter((space_heated == 1) ) %>%
  replace_na(list(weight=0)) 

# Convert weights to long: ----------------------------------------------------
weights_long <- recs %>%
  filter(HEATHOME == 1) %>%
  select(DOEID, BRRWT1:BRRWT96) %>%
  gather(key = 'repl', value = 'w', BRRWT1:BRRWT96)

# Join home type to weights: --------------------------------------------------
winter_tmp_heated_rep <- weights_long %>%
  left_join(winter_tmp_heated %>% mutate(DOEID = as.integer(DOEID)), by='DOEID')

# Check nothing is lost
if( nrow(weights_long) != nrow(winter_tmp_heated_rep) ) {
  stop("DOEID mismatch!")
}

winter_tmp_heated_avg_rep <- winter_tmp_heated_rep %>%
  group_by(repl) %>%
  summarize(winter_tmp_avg_r = sum(winter_tmp * w) / sum(w))

winter_tmp_heated_avg <- winter_tmp_heated %>%
  group_by(space_heated) %>%
  summarize(winter_tmp_avg = sum(winter_tmp * weight) / sum(weight))

# in this problem we all use \epsilon = 0.5
winter_tmp_heated_avg_num <- as.numeric(winter_tmp_heated_avg[,2])
winter_tmp_heated_stderr <- 2 * sqrt(mean( {sapply(winter_tmp_heated_avg_rep[,2], as.numeric) - winter_tmp_heated_avg_num}^2 ))
winter_tmp_heated_lwr <- winter_tmp_heated_avg_num - qnorm(0.975) * winter_tmp_heated_stderr
winter_tmp_heated_upr <- winter_tmp_heated_avg_num + qnorm(0.975) * winter_tmp_heated_stderr

sprintf(paste0('the national average home temperature at night in winter is %f,',
        ' among homes using space heating, ', 'and the upperbound is %f, and the ',
        'lower bound is %f'), 
        winter_tmp_heated_avg_num, winter_tmp_heated_upr, winter_tmp_heated_lwr )

#80: ---------------------------------------------------------------------------
#b.
decode_fuel = function(x) {
  if(!is.numeric(x)) stop('decode_fuel expects numeric input indexed from 1!')

  re <- as.character( factor(x, levels = c(1, 2, 3, 5, 7, 21),
                             labels = c('Natural_gas',
                                        'Propane',
                                        'Fuel_oil_kerosene',
                                        'Electricity',
                                        'Wood',
                                        'Some_other_fuel')
                             ))
  
  return(re)
}

decode_all_fuel = function(x) {
  return(sapply(x,decode_fuel))
}

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

decode_urban = function(x) {
  re <- as.character( factor(x, levels = c('U', 'C', 'R'),
                            labels = c('Urban Area', 'Urban Cluster', 'Rural')
                            ))
  return(re)
}

decode_all_urban = function(x) {
  return(sapply(x,decode_urban))
}

recs_fuel<- recs %>%
  transmute(DOEID, fuel_type = FUELHEAT, divis = DIVISION, urban_type = UATYP10, 
            space_heated = HEATHOME, weight = NWEIGHT) %>%
  filter((fuel_type > 0) & (space_heated == 1) ) %>%
  mutate(fuel_type = decode_all_fuel(fuel_type),urban_type = decode_all_urban(urban_type), 
         divis = decode_all_division(divis))

recs_fuel_rep <- weights_long %>%
  left_join(recs_fuel %>% mutate( DOEID=as.integer(DOEID) ) , by='DOEID' )

# Check nothing is lost
if( nrow(weights_long) != nrow(recs_fuel_rep) ) {
  stop("DOEID mismatch!")
}

recs_fuel_prop_rep <- recs_fuel_rep %>%
  group_by(divis,urban_type, fuel_type, repl) %>%
  summarize(sum_fuel = sum(w)) %>%
  tidyr::pivot_wider(names_from = fuel_type, 
                     values_from = sum_fuel, values_fill = list(sum_fuel = 0)) %>%
  mutate(Total =Natural_gas + Propane + Fuel_oil_kerosene + Electricity + Wood + Some_other_fuel, 
         Natural_gas_r = 100 * Natural_gas / Total, 
         Propane_r = 100 * Propane / Total,
         Fuel_oil_kerosene_r = 100 * Fuel_oil_kerosene / Total,
         Electricity_r = 100 * Electricity / Total,
         Wood_r = 100 * Wood / Total,
         Some_other_fuel_r = 100 * Some_other_fuel / Total) %>%
  select(divis, urban_type, repl, Natural_gas_r, Propane_r, Fuel_oil_kerosene_r,
         Electricity_r, Wood_r,Some_other_fuel_r)

recs_fuel_prop <- recs_fuel %>%
  group_by(divis,urban_type, fuel_type) %>%
  summarize(sum_fuel = sum(weight)) %>% 
  tidyr::pivot_wider(names_from = fuel_type, 
                     values_from = sum_fuel, values_fill = list(sum_fuel = 0)) %>%
  mutate(Total =Natural_gas + Propane + Fuel_oil_kerosene + Electricity + Wood + Some_other_fuel, 
         Natural_gas = 100 * Natural_gas / Total, 
         Propane = 100 * Propane / Total,
         Fuel_oil_kerosene = 100 * Fuel_oil_kerosene / Total,
         Electricity = 100 * Electricity / Total,
         Wood = 100 * Wood / Total,
         Some_other_fuel = 100 * Some_other_fuel / Total) %>%
  select(-Total)

recs_fuel_prop_rep <- recs_fuel_prop_rep %>% 
  left_join(recs_fuel_prop, by = c ('divis','urban_type'))
  
# Comptue standard errors: ----------------------------------------------------
recs_fuel_prop <- recs_fuel_prop_rep %>%
  group_by(divis, urban_type) %>%
  summarize(Natural_gas = mean(Natural_gas), Propane = mean(Propane), Fuel_oil_kerosene = 
            mean(Fuel_oil_kerosene), Electricity = mean(Electricity), Wood = mean(Wood), 
            Some_other_fuel = mean(Some_other_fuel),
            stderr_ng = 2*sqrt(mean({Natural_gas_r - Natural_gas}^2)),
            stderr_p = 2*sqrt(mean({Propane_r - Propane}^2)),
            stderr_k = 2*sqrt(mean({Fuel_oil_kerosene_r - Fuel_oil_kerosene}^2)),
            stderr_e = 2*sqrt(mean({Electricity_r - Electricity}^2)),
            stderr_w = 2*sqrt(mean({Wood_r - Wood}^2)),
            stderr_o = 2*sqrt(mean({Some_other_fuel_r - Some_other_fuel}^2))
            ) %>%
  group_by(divis, urban_type) %>% 
  transmute(Natural_gas = paste0(as.character(round(Natural_gas,2)),' +- ' ,
                                 as.character(round(qnorm(0.975)*stderr_ng,2)), '%' ),
            Propane = paste0(as.character(round(Propane,2)),' +- ' ,
                             as.character(round(qnorm(0.975)*stderr_p,2)), '%' ),
            Fuel_oil_kerosene = paste0(as.character(round(Fuel_oil_kerosene,2)),' +- ' ,
                                       as.character(round(qnorm(0.975)*stderr_k,2)), '%' ),
            Electricity = paste0(as.character(round(Electricity,2) ),' +- ' ,
                                 as.character(round(qnorm(0.975)*stderr_e,2) ), '%' ),
            Wood = paste0(as.character(round(Wood,2) ),' +- ' ,
                          as.character(round(qnorm(0.975)*stderr_w) ), '%' ),
            Some_other_fuel = paste0(as.character(round(Some_other_fuel,2) ),' +- ' ,
                                     as.character(round(qnorm(0.975)*stderr_o,2) ), '%' )
            )
  

# Output a markdown table: -----------------------------------------------------
cap_title <- '**Table 1.** *Proprtion of homes using each level of “main space heating fuel”.*'
cap_text0 <- 'Each row shows unique combination of census division and census (2010) urban type.'
cap <- paste(cap_title, cap_text0)

cols <- c('division', 'urban type', 'Natural_gas', 
         'Propane', 'Fuel_oil_kerosene', 'Electricity', 'Wood', 'Some_other_fuel')

knitr::kable(recs_fuel_prop, digits=1, caption=cap, col.names = cols)

#80: ---------------------------------------------------------------------------
#c.

# Transforming the inappliable temperature data from label -2 to NA. 
fill_tmp_NA = function(x) {
  if (x == -2) {re <- NA}
  else {re <- x}
  return(re)
}

fill_all_tmp_NA = function(x) {
  sapply(x, fill_tmp_NA)
}


# in the Fay’s balanced repeated replication (BRR) method of estimating standard error
# we set \epsilon = 0.5 and R = the number of replicate subsamples then the estimated
# standard error could be reduced to var(samples) / 

recs_tmp <- recs %>%
  transmute(DOEID, divis = DIVISION, urban_type = UATYP10, d = TEMPHOME,
            dn = TEMPGONE, n = TEMPNITE, weight = NWEIGHT) %>%
  mutate(divis = decode_all_division(divis), urban_type = decode_all_urban(urban_type),
         d = fill_all_tmp_NA(d), dn = fill_all_tmp_NA(dn),
         n = fill_all_tmp_NA(n), weight = weight)
  

recs_tmp_avg <- recs_tmp %>%
  gather(key = 'tmp_type', value = 'tmp', d, dn, n) %>%
  group_by(divis, urban_type, tmp_type) %>%
  summarize(tmp_avg = sum(sapply(tmp, as.numeric) * weight, na.rm = T) / sum(weight))

weights_long_all <- recs %>%
  select(DOEID, BRRWT1:BRRWT96) %>%
  gather(key = 'repl', value = 'w', BRRWT1:BRRWT96)

recs_tmp_rep <- weights_long_all %>%
  left_join(recs_tmp %>% mutate( DOEID=as.integer(DOEID) ) , by='DOEID' )
  
# Check nothing is lost
if( nrow(recs_tmp_rep) != nrow(weights_long_all) ) {
    stop("DOEID mismatch!")
  }  

recs_tmp_avg_rep <- recs_tmp_rep %>%
  gather(key = 'tmp_type', value = 'tmp', d, dn, n) %>%
  group_by(divis, urban_type, repl, tmp_type) %>%
  summarize(tmp_avg_r = sum(sapply(tmp, as.numeric) * w, na.rm = T) / sum(w) )

recs_tmp_avg_rep <- recs_tmp_avg_rep %>% 
  left_join(recs_tmp_avg, by = c ('divis','urban_type', 'tmp_type'))

recs_tmp_avg <- recs_tmp_avg_rep %>%
  group_by(divis, urban_type, tmp_type) %>%
  summarize(tmp_avg = mean(tmp_avg),
            stderr = 2*sqrt(mean({tmp_avg_r - tmp_avg}^2))) %>%
  group_by(divis, urban_type, tmp_type) %>%
  mutate(lwr = tmp_avg - qnorm(.975)*stderr,
         upr = tmp_avg + qnorm(.975)*stderr
         )

p_sfd <- recs_tmp_avg %>%
  ungroup() %>%
  ggplot( aes( x = tmp_type, y = tmp_avg) ) +
  geom_point( col='navy', pch = 15, cex=2) +
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='red' ) +
  facet_wrap(~urban_type+divis, scales='free_x', nrow=3, ncol=10) + 
  theme_bw() +
  theme( axis.text.x =
           element_text(
             angle = 0,
             size = 10
           ), plot.margin = margin(0.2,0.2,0.2,0.2,"cm") ) +
  ylim( c(50, 78) ) +
  xlab('Different home status: n = at night, d = the day someone home, dn = the day no one home') +
  ylab('average winter home temperatures (F degree)')
p_sfd

#80: ---------------------------------------------------------------------------
#d.

# Set one temperature and leave it there most of the time
# Manually adjust the temperature at night or when no one is at home
# Program the thermostat to automatically adjust the temperature during the day and night at certain times
# Turn equipment on or off as needed
# Our household does not have control over the equipment
# Other
# Not applicable

decode_behavior = function(x) {
  if(!is.numeric(x)) stop('decode_behavior expects numeric input indexed from 1!')
  
  re <- as.character( factor(x, levels = c(1, 2, 3, 4, 5, 9),
                             labels = c('Set one temperature and leave it there most of the time',
                                        'Manually adjust the temperature at night or when no one is at home',
                                        paste0('Program the thermostat to automatically adjust the',
                                        'temperature during the day and night at certain times'),
                                        'Turn equipment on or off as needed',
                                        'Our household does not have control over the equipment',
                                        'Other')
  ))
  return(re)
}

decode_behavior_all = function(x) {
  return(sapply(x, decode_behavior))
}

# Key values for each observation: --------------------------------------------
recs_mtmp <- recs %>%
  filter(HEATHOME == 1) %>%
  transmute(DOEID, bhvr = decode_behavior_all(EQUIPMUSE), weight = NWEIGHT, day = TEMPHOME, night = TEMPNITE) %>%
  replace_na(list(weight=0))  %>%
  filter( (bhvr > 0) & (day > 0) & (night >0)) 

# Convert weights to long: ----------------------------------------------------
weights_long <- recs %>%
  filter(HEATHOME == 1) %>%
  select(DOEID, BRRWT1:BRRWT96) %>%
  gather(key = 'repl', value = 'w', BRRWT1:BRRWT96) 

recs_mtmp_rep <- weights_long %>%
  left_join(recs_mtmp %>% mutate( DOEID=as.integer(DOEID) ) , by='DOEID' )

# Check nothing is lost
if( nrow(recs_mtmp_rep) != nrow(weights_long) ) {
  stop("DOEID mismatch!")
}

recs_mtmp_med <- recs_mtmp %>%
  gather(key = 'tmp_type', value = 'tmp', day, night)  %>%
  arrange(tmp) %>%
  group_by(bhvr, tmp_type) %>%
  summarize(tmp_med = tmp[min(which(cumsum(weight) > 0.5*sum(weight)))] )


recs_mtmp_med_rep <- recs_mtmp_rep %>%
  gather(key = 'tmp_type', value = 'tmp', day, night) %>%
  group_by(bhvr, tmp_type, repl) %>%
  summarize(tmp_med_r = tmp[min(which(cumsum(w) > 0.5*sum(w)))] )

recs_mtmp_med_rep <- recs_mtmp_med_rep %>% 
  left_join(recs_mtmp_med, by = c ('bhvr', 'tmp_type'))


recs_mtmp_med <- recs_mtmp_med_rep %>%
  group_by(bhvr, tmp_type) %>%
  summarize(tmp_med = mean(tmp_med),
            stderr = 2*sqrt(mean({tmp_med_r - tmp_med}^2))) %>%
  group_by(bhvr, tmp_type) %>%
  mutate(lwr = tmp_med - qnorm(.975)*stderr,
         upr = tmp_med + qnorm(.975)*stderr
  )



recs_mtmp_med_tib <- recs_mtmp_med %>%
  tidyr::pivot_wider(names_from = tmp_type, values_from = c(tmp_med, stderr,
                                                            lwr, upr))
  
# Output a markdown table: -----------------------------------------------------
cap_title <- '**Table 2.** *The (national) median difference between the daytime (with someone home) and nighttime temperatures”.*'
cap_text0 <- 'Each row shows each level of “main heating equipment household behavior”.'
cap <- paste(cap_title, cap_text0)

cols <- c('equipment behavior', 'tmp_med_day', 'tmp_med_night', 
          'stderr_day', 'stderr_night', 'lwr_day', 'lwr_night', 'upr_day', 'upr_night')

knitr::kable(recs_mtmp_med_tib, digits=1, caption=cap, col.names = cols)


p_med <- recs_mtmp_med %>%
  ungroup() %>%
  ggplot( aes( x = tmp_type, y = tmp_med) ) +
  geom_point( col='navy', pch = 15, cex=2) +
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='red' ) +
  facet_wrap(~bhvr, scales='free_x', nrow=6) + 
  theme_bw() +
  theme( axis.text.x =
           element_text(
             angle = 0,
             size = 10
           ), plot.margin = margin(0.2,0.2,0.2,0.2,"cm") ) +
  ylim( c(20, 110) ) +
  xlab('Different home status: at night, during the day someone home') +
  ylab('The medians of temperatures')
p_med





















