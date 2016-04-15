library(Hmisc)
## Central limit theorem
set.seed(1)
population <- round(runif(n = 1000, 
                          min = 0,
                          max = 10))
mu <- mean(population)

## Standard deviation without the Bessel's correction
sigma.squared <- sum((population - mu) ^ 2) / length(population)
sigma <- sqrt(sigma.squared)

## Other way to calculation SD without Bessel's correction
sigma2 <- sd(population) * sqrt((length(population) - 1) / length(population))

## Experiment:
## Take 300 samples of couple of 2 from the whole population
sample_vector <-  sample(population, 600, replace = T)
sample <- matrix(data = sample_vector,
                 ncol = 2,
                 byrow = T,
                 dimnames = list(rows = NULL, cols = c('un', 'deux')))
## calcul the mean of each rows
sample <- cbind(sample, mean = rowMeans(x = sample))

## Does total mean equal mu ?
sample.mean <- mean(sample[, c('mean')])
## It is approximatively correct:
## mean of sample means = 5.09333
## mean of population = 4.994

## Standard deviation of the sample mean
n <- dim(sample)[1]
mean.values <- sample[, 'mean']
S2 <- sum( ( mean.values - sample.mean) ^ 2 ) / n
(S <- sqrt(S2))
## Verification
sd(mean.values) * sqrt( (n-1) / n)

## What the cental limit theorem said is that the standard error of the sample mean
## distribution is equal to the population standard deviation divided by
## the square root of n:

##                   SE = sigma / n ^ .5
(SE <- sigma / 2 ^ .5)

## SE = 2.051824
## S = 2.043189
## Pretty cool!
