# Output a markdown table: -----------------------------------------------------
cap_title = '**Table 1.** *Proprtion of home types by state(s).*'
cap_text0 = 'Each row shows the home type distribution for one or more states.'
cap_text1 = 'Rows are sorted by the proportion of single family attached homes.'
cap = paste(cap_title, cap_text0, cap_text1)

cols = c('state(s)', 'Apartment (2-5 Units)', 'Apartment (5+ units)',
         'Mobile Home', 'Single Family Attached', 'Single Family Detached')

knitr::kable(recs_type_state, digits=1, caption=cap, col.names = cols)