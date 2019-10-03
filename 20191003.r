# S3 is based on generic functions
# generic = function(x)
# usemethod("generic")

# (S3) generic dispatch:
# use this template generic-class

# inheritance: e.g attr(ml, "class") = c("glm", "lm")
# glm is a subclass of lm (可以使用lm的一样可以用glm， 但可以使用glm的不一定可以使用lm)

# most S3 classes are lists

# e.g After the function call print(fit), in what order are the following functions called? Indicate
# your solution as a call graph with arrows indicating the order, i.e. f irst → second → third.
# cat(), print.lm(), UseMethod()
# UseMethod() → print.lm() → cat()

# 可见 UseMethod一定是最先使用的， 然后依照由深入浅顺序执行

# Define a new S3 generic function describe().
# describe = function(x, ...){
# UseMethod('describe')}

# psum = function(a,b,...){}
# f = function(...) {
#   psum(...)}

# 查 ... !!!!!!!!!!!!!!!!!
