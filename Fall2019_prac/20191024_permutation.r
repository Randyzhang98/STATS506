# permutation
# 从全集n个sample中选取m个sample作为sample set
# to construct a reference distribution for determining the significance
# of an observed test statistic by repeatedly permuting group labels.
# e.g 4C2 = 6
# 实际上是算p-value然后做hypothesis testing

# 不是考试重点， 会在作业中出现。

nperm = 1e3
perm_F = sapply(1:nperm, function(i) permute_F(df) )
#注意这里 1：nperm之应用vectorization

> sample(1:10, replace = FALSE)
 [1]  2  3  1  5  6  7  9  8 10  4
> sample(1:10, replace = TRUE)
 [1] 7 8 2 5 6 4 4 4 5 8

# crossvalidation
# In cross validation we try to make efficient use of our available
# !!!!!!!!!non-test!!!!!!!!!!!! data by repeatedly interchanging which
# observations are considered training and which validation data.
# 在分组fold之前，必须对train shuffle


# ridge regression \hat{\beta} = L2_norm(y-X\beta) + \lambda L2_norm(\beta)
# 或者 \hat{\beta} = L2_norm(y-X\beta) + \lambda L1_norm(\beta)
# 或者 \hat{\beta} = L2_norm(y-X\beta) + \alpha \lambda L2_norm(\beta) + (1-\alpha) \lambda L1_norm(\beta)

# separate out the training data
# Xtrain = as.matrix( iso_train[-resp_col] ) #? Why single brackets?
# 双重brack仅仅能用于一列