r_uv <- function(n){
    n_first <- 1
    n_sample <- n * 4 / pi
    u_appr <- numeric(n)
    v_appr <- numeric(n)

    while(TRUE){
        u <- runif(n_sample, -1, 1)
        v <- runif(n_sample, -1, 1)
        
        appr <- (u^2 + v^2) <= 1
        n_appr <- sum(appr)
        # these are the ones that passed the test

        n_last = n_first + n_appr - 1
        if (n_last > n) {
            n_last <- n
            u_appr[n_first:n_last] <- u[appr][1:(n_last+1-n_first)]
            v_appr[n_first:n_last] <- v[appr][1:(n_last+1-n_first)]
            break
        }
        
        u_appr[n_first:n_last] <- u[appr]
        v_appr[n_first:n_last] <- v[appr]
        n_first = n_last + 1
    }
    return(list(u_appr,v_appr))
}

another_method <- function(n){
    l <- r_uv(n)
    u <- l[[1]]
    v <- l[[2]]
    u2_plus_v2 <- u^2 + v^2
    factor <- sqrt(-2*log(u2_plus_v2)) / sqrt(u2_plus_v2)
    x <- u * factor
    y <- v * factor
    return(list(x,y))
}

box_muller <- function(n){
    u <- runif(n)
    v <- runif(n)
    factor <- sqrt(-2*log(u))
    x <- factor * cos(2*pi*v)
    y <- factor * sin(2*pi*v)
    return(list(x,y))
}

multivariate <- function(n, mu, sigma){
    n_dim <- length(mu)
    # how many samples to draw from another_method. The func returns twice as much
    n_samples <- round(n_dim * n / 2)

    l <- another_method(n_samples)  # generate N(0,1) samples
    gen_norm <- c(l[[1]], l[[2]])  # stack x and y on top of each other

    if (length(gen_norm) > n_dim * n){  # we generated 1 element more
        gen_norm <- head(gen_norm, -1)
    }
    X <- array(gen_norm, dim=c(n,n_dim))  # matrix form
    Y <- X %*% chol(sigma) + mu
    return(Y)
}

n =
gen_m <- multivariate(n, array(c(1,3), dim=c(2,1)), array(c(2,1,1,.5), dim=c(2,2)))