# Stats 506, Fall 2019
# Problem Set 2, Question 2
#
# This script analyzes the mouse tracking data from KH2017
#
# Author: James Henderson
# Date: October 14, 2019
#80: ---------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse); library(lme4); library(lmerTest)

# data: -----------------------------------------------------------------------
df = as_tibble( mousetrap::KH2017_raw )

# utility functions: ----------------------------------------------------------

## (a) functions that compute measures
source('./ps2_q2_funcs.R')

## (b) function to put the data into a useable form
make_numeric = function(x){
  # Inputs: A character vector string of x or y positions of the form
  #         "[x1, x2, ..., xt]"
  # Outputs: a list of numeric vectors, e.g. c(x1, ..., xt)

  # Convert to character if needed
  if ( is.factor(x) ) {
    x = as.character(x)
    warning("make_numeric: converting x to character")
  }

  # Make replacements and conversions
  lapply(str_split(str_replace_all(x,'\\[|\\]', ''), ", "), as.numeric)
}

# (b) extract trajectories to numeric "list" columns: --------------------------
df = df %>%
  mutate(
    xpos = make_numeric( as.character(xpos_get_response) ),
    ypos = make_numeric( as.character(ypos_get_response) ),
    tm   = make_numeric( as.character(timestamps_get_response) )
  )

# (c) compute the curvature measures for trial and attach to original data: ---
df_measures = df %>%
  filter( correct == 1 ) %>%
  group_by(subject_nr, count_trial) %>%
  do(
    convert_vec2tibble(
      comp_measures(
        normalize_traj( cbind(.$xpos[[1]], .$ypos[[1]], .$tm[[1]] ) )
      )
    )
  )

## join measures
df = left_join(df_measures, df, by = c('subject_nr', 'count_trial'))

df_ps4_q1 = df %>% select(subject_nr,count_trial, tot_dist, max_abs_dev,
                          avg_abs_dev, AUC, Condition, Exemplar)

write.table(df_ps4_q1, file="mousetrap_data.csv", row.names=FALSE, col.names=TRUE, sep=",")



