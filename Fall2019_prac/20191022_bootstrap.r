
# Compute relative weight: ----------------------------------------------------
cw = cw %>%
  group_by(Chick) %>%
  mutate( relweight = weight / weight[ Time == 0 ]) %>%
  ungroup() # 若无此ungroup， 则下面filter回依据Chick group_by来filte

# Interactive computation the dplyr way: ---------------------------------
cw %>%
  # Ensure we pick only week 21, not earlier max time if missing data.
  ungroup() %>%
  filter( Time == max(Time) ) %>%
  group_by(Diet) %>%
  summarize( median = median(weight), n = n() )

######################################################################
# scoped dplyr verbs: _all _if _at
help("summarise_at")

head(cw)
# A tibble: 6 x 5
  weight  Time Chick Diet  relweight
   <dbl> <dbl> <ord> <fct>     <dbl>
1     42     0 1     1          1
2     51     2 1     1          1.21
3     59     4 1     1          1.40
4     64     6 1     1          1.52
5     76     8 1     1          1.81
6     93    10 1     1          2.21
cw %>%
     ungroup() %>%
     filter( Time == max(Time) ) %>%
     group_by(Diet) %>%
     summarize_at( .vars = c("weight", "relweight"),
                   .funs = list( median = median, n = length )
     )
# A tibble: 4 x 5
  Diet  weight_median relweight_median weight_n relweight_n
  <fct>         <dbl>            <dbl>    <int>       <int>
1 1              166              3.95       16          16
2 2              212.             5.45       10          10
3 3              281              6.77       10          10
4 4              237              5.92        9           9

#可见是所有的排列组合covered.
> cw %>%
+     ungroup() %>%
+     filter( Time == max(Time) ) %>%
+     group_by(Diet) %>%
+     summarize_at( .vars = c("weight", "relweight"),
+                   .funs = median
+     )
# A tibble: 4 x 3
  Diet  weight relweight
  <fct>  <dbl>     <dbl>
1 1       166       3.95
2 2       212.      5.45
3 3       281       6.77
4 4       237       5.92
#若非list，则colname remains
################################################################

df_group_median = function(df, cols, group ='') {
    #注意这里需要先检查类型
  df %>%
    group_by( .data[[!!group]] ) %>% # 这里!!代表若有colname = group， 不要interpret，这里的group为函数输入的parameter
    summarize_at( .vars = cols, .funs = median )
}

######################################### Preallocation ######################################

# Create 1000 bootstrap samples.
#! How could we vectorize this?
boot_samples = list()
for(i in 1:1e3){
  boot_samples[[i]] =
    df_boot_median(cw_final, c('weight', 'relweight'), 'Diet') %>%
    mutate( b = !! i)
}


nboot = 1e3
boot_samples = vector(mode = 'list', length = nboot)
for(i in 1:1e3){
  boot_samples[[i]] =
    df_boot_median(cw_final, c('weight', 'relweight'), 'Diet') %>%
    mutate( b = !! i)
}

######################################################################################

> pivot_longer(boot_ci,
+              cols = -Diet,
+              names_to = c('var', 'bound'),
+              names_sep = '_',
+              values_to = 'x')
# A tibble: 16 x 4
   Diet  var       bound      x
   <fct> <chr>     <chr>  <dbl>
 1 1     weight    lwr   140.
 2 1     relweight lwr     3.38
 3 1     weight    upr   208.
 4 1     relweight upr     5.13
 5 2     weight    lwr   162.
 6 2     relweight lwr     3.82
 7 2     weight    upr   271
 8 2     relweight upr     6.95
 9 3     weight    lwr   217
10 3     relweight lwr     5.41
11 3     weight    upr   321.
12 3     relweight upr     7.88
13 4     weight    lwr   200
14 4     relweight lwr     4.78
15 4     weight    upr   281
16 4     relweight upr     6.69
>
> pivot_longer(boot_ci,
+              cols = -Diet,
+              names_to = c('var', 'bound'),
+              names_sep = '_',
+              values_to = 'x') %>%
+     pivot_wider(id_cols = c('Diet', 'var'), names_from = bound, values_from = x)
# A tibble: 8 x 4
  Diet  var          lwr    upr
  <fct> <chr>      <dbl>  <dbl>
1 1     weight    140.   208.
2 1     relweight   3.38   5.13
3 2     weight    162.   271
4 2     relweight   3.82   6.95
5 3     weight    217    321.
6 3     relweight   5.41   7.88
7 4     weight    200    281
8 4     relweight   4.78   6.69

#如何选择合适的shape longer与wider组合