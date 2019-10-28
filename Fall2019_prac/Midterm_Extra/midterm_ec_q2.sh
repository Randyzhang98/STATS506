#!/bin/bash
#--------------------------------------------
# author：Sijun Zhang
# last change date: 10/28/2019
#--------------------------------------------

grep -rn -E '(gather\(|spread\()' *.R*

# ps1_q2.R:143:  spread(origin, prop, fill = '-') %>%
# ps1_q3.R:28:weights_long = gather(weights, key = 'repl', value = 'w', BRRWT1:BRRWT96) %>%
# ps1_q3.R:197:  spread(urban, est) %>%
# ps1_q3.R:206:  spread(urban, r) %>%
# ps1_q3.R:222:               spread(urban, ci),
# PS1_Solutions.Rmd:277:  spread(Rurality, `Average Electricity Usage, kwh/home (95% CI)` ) %>%