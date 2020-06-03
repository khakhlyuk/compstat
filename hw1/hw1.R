## Problem 1

set.seed(42)

roll_dice <- function(n){
    x <- runif(n)
    x[x >= 5/6          ] = 6
    x[x >= 4/6 & x < 5/6] = 5
    x[x >= 3/6 & x < 4/6] = 4
    x[x >= 2/6 & x < 3/6] = 3
    x[x >= 1/6 & x < 2/6] = 2
    x[           x < 1/6] = 1
    x
}

R <- 100000

rolls <- roll_dice(R)
roll_freq <- table(rolls) / R
barplot(roll_freq, main="Dice rolls. Observed relative frequencies")

## Problem 2

exponential <- function (n, rate=1) {
    u <- runif(n)
    x <- -log(u) / rate
    x
}

gen_exp <- exponential(R, rate=1)
exp_seq <- seq(0, 10, 0.01)
plot(density(gen_exp),
     xlim=c(0,10), ylim=c(0,1),
     col="blue", xlab="x", ylab="density",
     main="Exp(1) distribution
           Blue: Emperical (kernel) density function.
           Red: True density function (dexp).")
par(new=TRUE)
plot(exp_seq, dexp(exp_seq), type="l",
     xlim=c(0,10), ylim=c(0,1),
     col="red", xlab="", ylab="", main="")

# hist(gen_exp, breaks=100, freq=FALSE, xlab='x', main="Our Exp(1)")
# hist(rexp(R, rate=1), breaks=100, freq=FALSE, xlab='x', main="Library Exp(1)")


## Problem 3

poisson_single <- function(rate=1){
    counter <- 0
    sum <- 0
    repeat{
        sum <- sum + exponential(1, rate)
        if(sum > 1){
            break
        }
        counter <- counter + 1
    }
    counter
}

poisson <- function(n, rate=1){
    vec <- numeric(n)
    for (i in 1:n){
        vec[i] <- poisson_single(rate)
    }
    vec
}

R <- 100000
rate <- 10


blue <- rgb(0, 0, 1, alpha=0.5)
red  <- rgb(1, 0, 0, alpha=0.5)

gen_poi <- poisson(R, rate)
plot_seq <- seq(1, 25)
hist(gen_poi, freq=FALSE, breaks=25,
    xlim=c(0,25),
    col=red, xlab='x',
    main="Poi(10) distribution
          Blue: observed relative frequencies.
          Red: True density function (dpois).")
barplot(dpois(plot_seq, rate), space=0, add=TRUE,
    xlim=c(0,25),
    col=blue)


