
S3 system in r

print(obj)
> if obj is an int, call # print.int
> if obj class can not find, call # print.deflaut.
Exp.
methods(head)
## [1] head.data.frame* head.default*    head.ftable*     head.function*
## [5] head.matrix      head.table*
## see '?methods' for accessing help and source code

> head(mat)
     [,1] [,2] [,3] [,4] [,5]
[1,]    1   10   19   28   37
[2,]    2   11   20   29   38
[3,]    3   12   21   30   39
[4,]    4   13   22   31   40
[5,]    5   14   23   32   41
[6,]    6   15   24   33   42
> head.matrix(mat)
     [,1] [,2] [,3] [,4] [,5]
[1,]    1   10   19   28   37
[2,]    2   11   20   29   38
[3,]    3   12   21   30   39
[4,]    4   13   22   31   40
[5,]    5   14   23   32   41
[6,]    6   15   24   33   42
> head.data.frame(mat)
Error in head.data.frame(mat) : could not find function "head.data.frame"
Because
The * following some methods is used to denote methods that are not exported as part of the namesapce of the packages in which they are defined. For instance, the head.data.frame method is defined in the (base) package utils, but is not exported.

> sloop::s3_dispatch(head(mat))
   head.green
   head.red
=> head.matrix
 * head.default

 > red_obj = 1:100
> class(red_obj) = 'red'
> head.green(red_obj)
The object is not green!
> getS3method(head(red_obj))
Error in exists(fname, mode = "function", envir = envir) :
  invalid first argument
In addition: Warning message:
In f == getKnownS3generics() :
  longer object length is not a multiple of shorter object length
> head(red_obj)
[1] 1 2 3 4 5 6

POSIXlt(Day-Time Class)
# Day[1] 1 3
# Year[1] 2019 2019
# Mouth[1] 10 10
# hour[1] 10 10

glm family
Usage

family(object, ...)

binomial(link = "logit")
gaussian(link = "identity")
Gamma(link = "inverse")
inverse.gaussian(link = "1/mu^2")
poisson(link = "log")
quasi(link = "identity", variance = "constant")
quasibinomial(link = "logit")
quasipoisson(link = "log")

class(fit0)
## [1] "glm" "lm"
typeof(fit0)
## [1] "list"

