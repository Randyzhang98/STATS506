> mcrep = 10000                 # Simulation replications
> n = 30                        # Sample size we are studying
> xmat = rexp(n * mcrep)        # Simulate standard exponential data
> dim(xmat) = c(mcrep, n)       # Each row is a dataset
> mn = apply(xmat, 1, mean)     # Sample mean of each row
> std = apply(xmat, 1, sd)      # Sample SD of each row
> se = std / sqrt(n)            # Standard errors of the mean
> conf_level = .95              # The nominal confidence level
> m = qt( 1 - {1 - conf_level} / 2, df = n - 1) # Multiplier for confidence level (~2.04)
> lcb = mn - m*se               # lower confidence bounds
> ucb = mn + m*se               # upper confidence bounds
> target = 1                    # value we are estimating
> cvrg_prob = mean( {lcb < target} & {ucb > target} ) # coverage probability
> print(cvrg_prob)
[1] 0.9238
> xmat = c(1, 2, 3, 4, 5, 6)
> dim(xmat) = c(3, 2)
> xmat
     [,1] [,2]
[1,]    1    4
[2,]    2    5
[3,]    3    6
> colMeans(xmat)
[1] 2 5
> rowMeans(xmat)
[1] 2.5 3.5 4.5

> cat("Apply: ", tm1[3], "s, Vectorized: ", tm2[3], "s, Ratio: ",
+     round( tm1[3] / tm2[3], 1), '.\n', sep = '' )
Apply: 1.55s, Vectorized: 0.07s, Ratio: 22.1.

> y = rnorm(10)
> y
 [1]  1.4455015  0.3142857 -1.2782784 -0.3000717 -1.7446780  0.4765848  0.9827047
 [8] -1.2703321  2.0605099  0.4284679
> outer(array(1,10),y) #outer's use
          [,1]      [,2]      [,3]       [,4]      [,5]      [,6]      [,7]      [,8]
 [1,] 1.445502 0.3142857 -1.278278 -0.3000717 -1.744678 0.4765848 0.9827047 -1.270332
 [2,] 1.445502 0.3142857 -1.278278 -0.3000717 -1.744678 0.4765848 0.9827047 -1.270332
 [3,] 1.445502 0.3142857 -1.278278 -0.3000717 -1.744678 0.4765848 0.9827047 -1.270332
 [4,] 1.445502 0.3142857 -1.278278 -0.3000717 -1.744678 0.4765848 0.9827047 -1.270332
 [5,] 1.445502 0.3142857 -1.278278 -0.3000717 -1.744678 0.4765848 0.9827047 -1.270332
 [6,] 1.445502 0.3142857 -1.278278 -0.3000717 -1.744678 0.4765848 0.9827047 -1.270332
 [7,] 1.445502 0.3142857 -1.278278 -0.3000717 -1.744678 0.4765848 0.9827047 -1.270332
 [8,] 1.445502 0.3142857 -1.278278 -0.3000717 -1.744678 0.4765848 0.9827047 -1.270332
 [9,] 1.445502 0.3142857 -1.278278 -0.3000717 -1.744678 0.4765848 0.9827047 -1.270332
[10,] 1.445502 0.3142857 -1.278278 -0.3000717 -1.744678 0.4765848 0.9827047 -1.270332
         [,9]     [,10]
 [1,] 2.06051 0.4284679
 [2,] 2.06051 0.4284679
 [3,] 2.06051 0.4284679
 [4,] 2.06051 0.4284679
 [5,] 2.06051 0.4284679
 [6,] 2.06051 0.4284679
 [7,] 2.06051 0.4284679
 [8,] 2.06051 0.4284679
 [9,] 2.06051 0.4284679
[10,] 2.06051 0.4284679

> # First approach, calculate as a loop
> tm1 = proc.time()
> r1 = NULL
> for (i in 1:m) {
+     r1[i] = cor(xmat[i, ], yvec)
+ }
> tm1 = proc.time() - tm1
>
> # Second approach, functional style with apply
> tm2 = proc.time()
> r2 = apply(xmat, 1, function(v) cor(v, yvec) )
> tm2 = proc.time() - tm2
> all.equal(r1, r2)
[1] TRUE
> tm1
用户 系统 流逝
0.30 0.00 0.32
> y,2
Error: unexpected ',' in "y,"
> tm2
用户 系统 流逝
0.29 0.00 0.33

# Broadcasting
> x = c(1:9)
> y = c(1:2)
> dim(x) = c(3,3)
> x
     [,1] [,2] [,3]
[1,]    1    4    7
[2,]    2    5    8
[3,]    3    6    9
> x-y
     [,1] [,2] [,3]
[1,]    0    2    6
[2,]    0    4    6
[3,]    2    4    8
Warning message:
In x - y : longer object length is not a multiple of shorter object length
> y = 1:3
> x-y
     [,1] [,2] [,3]
[1,]    0    3    6
[2,]    0    3    6
[3,]    0    3    6
> y-x
     [,1] [,2] [,3]
[1,]    0   -3   -6
[2,]    0   -3   -6
[3,]    0   -3   -6

> x
     [,1] [,2] [,3]
[1,]    1    4    7
[2,]    2    5    8
[3,]    3    6    9
> y
[1] 1 2 3


# %*%
> x * y
     [,1] [,2] [,3]
[1,]    1    4    7
[2,]    4   10   16
[3,]    9   18   27
> x %*% y
     [,1]
[1,]   30
[2,]   36
[3,]   42
> dim(y) = c(3,1)
> y
     [,1]
[1,]    1
[2,]    2
[3,]    3
> x %*% y
     [,1]
[1,]   30
[2,]   36
[3,]   42

# Garbage Collection

# function

## Function to estimate nominal coverage probabilities
estimate_nominal_coverage =
  function(rgen, target, mcrep=1e4, n=30, conf_level=.95, ...){
  # rgen       - a function generating a vector of simulated data, i.e rexp(),
  #              with length equal to its first argument.
  # target     - the actual expectation of rgen()
  # mcrep, n   - the number of Monte Carlo replications and sample size, respectively.
  # conf_level - the nominal coverage probability
  # ...        - additional parameters to pass to rgen

  xmat = rgen(n * mcrep, ...)  # Simulate data
  dim(xmat) = c(mcrep, n)      # Each row is a dataset
  mn = apply(xmat, 1, mean)    # Sample mean of each row
  std = apply(xmat, 1, sd)     # Sample SD of each row
  se = std / sqrt(n)           # Standard errors of the mean
  m = qt( 1 - {1 - conf_level} / 2, df = n - 1) # Multiplier for confidence level
  lcb = mn - m * se              # lower confidence bounds
  ucb = mn + m * se              # upper confidence bounds

  mean( (lcb < target) & (ucb > target)) # coverage probability
}
# Now we can use estimate_nominal_coverage for multiple simulations.

# 注意！！！ 这里函数可以作为变量直接输入， 同时， ... 代表rgeom所需之parameter, transport para through ... to ...

# Geometric(p) with mean (1-p)/p
estimate_nominal_coverage(rgeom, target=3, p=.25)