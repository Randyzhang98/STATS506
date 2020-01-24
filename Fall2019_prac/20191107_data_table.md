
``` {r}
library(data.table)
nyc14 = fread('https://github.com/arunsrinivasan/flights/wiki/NYCflights14/flights14.csv')
class(nyc14)
```

Question: How can we view the print method for class data.table?
getS3method("print", "data.table"

the index in data.table
data.table[i(filter, arrange), j(transmutate, select), by] (inside is equivalent dplyr func)

multiple mutate
nyc14[,`:=`(delay15 = 1L * {dep_delay > 15 | arr_delay > 15}, delay20 = 2L * {dep_delay > 15 | arr_delay > 15})]

Pay attention to how the result is ordered.
by option will not automatically order the group (also will not put the row within same group together)

keyby will give the ordered output

