library(dplyr)
library(mosaic)
library(ggplot2)

set.seed(42)
## 2.ii
n <- 100
R <- 100000

mean <- 0
var <- 1

mu_hat <- numeric(R)
for (r in 1:R) {
    x <- rnorm(n, mean, var)
    mu_hat[r] <- mean(x)
}

bias <- mean(mu_hat) - mean
mse <- bias ^ 2 + var(mu_hat)

round(c(bias, mse), 4)


## 3
R <- 100000
n <- 10
p <- 0.3

binomial <- function(n, p)
{
    x <- runif(n)
    ones <- x <= p
    sum(ones)
}


bernoulli <- function(n, p)
{
    x <- runif(n)
    ones  <- x <= p
    zeros <- x > p
    x[ones]  <- 1
    x[zeros] <- 0
    x
}

obser <- numeric(R)
for (r in 1:R) {
    obser[r] <- binomial(n, p)
}

freq <- table(obser) / R
barplot(freq)


## 4.i
R <- 100000
bin_n <- 100
bin_p <- 0.7

th1 <- 0
th2 <- 0.3

ind_vec1 <- numeric(R)
for (r in 1:R) {
    x <- binomial(bin_n, bin_p)
    y <- rnorm(1, 0,1)
    ind_vec1[r] <- ( (x + 1) * y^3 > 0 )
}
p_hat_1 = mean(ind_vec1)


ind_vec2 <- numeric(R)
for (r in 1:R) {
    x <- binomial(bin_n, bin_p)
    y <- rnorm(1, 0,1)
    ind_vec2[r] <- ( (x + 1) * y^3 > 0.3 )
}
p_hat_2 = mean(ind_vec2)

print(p_hat_1)
print(p_hat_2)

