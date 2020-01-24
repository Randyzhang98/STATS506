
``` {r}
library(data.table)
nyc14 = fread('https://github.com/arunsrinivasan/flights/wiki/NYCflights14/flights14.csv')
class(nyc14)
```

# data.table quiz

detail in paper

# .SD

DT[, .SD, by]
L1$a, L1$b
L2$c, L2$d

c(L1,L2) = $a$b$c$d

# Copies

Caution:
``` {r}
DT1 = data.table(A=5:1, B=letters[5:1])
DT2 = DT1         # Copy by reference # see through c++ ref and pointer
DT3 = copy(DT1)   # Copy by value
DT4 = DT1[,.SD]   # Copy by value (not by the reference only for the original columns, just create a new data.frame)
```

# Reference Semantics
> tracemem(nyc14[,.(arr_delay)])
[1] "<0000019FC4E2D600>"
> nyc_df = as.data.frame(nyc14)
> tracemem(nyc_df$arr_delay)
[1] "<00007FF423500010>"
> tracemem(nyc14[,'arr_delay'])
[1] "<0000019FC5862A08>"

# Keys and Indexing

key search the corresponding

```{r}
setkey(nyc14, origin, dest) # already give the order to the group_bys
key(nyc14)
```

as ordered, we only need to find the beigin and end of the key want to find, then the search process nyc14[.("LGA", "DTW)] will be quicker than nyc14[origin == 'LGA' & dest = "DTW"]


# Join

