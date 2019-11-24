
if(!require(invgamma)){
  install.packages("invgamma")
  library(invgamma)
}

if(!require(MASS)){
  install.packages("MASS")
  library(MASS)
}

set.seed(100)

gibbs <- function(T){
  mat <- matrix(ncol = 3, nrow = n)
  beta <- 0.5
  sigma <- 1
  mat[1, ] <- c(beta[,1], beta[] sigma)
  for (i in 1:T) {
    sigma <- rinvgamma(1, shape = shape_sigma, scale = scale_sigma)
    beta_sd <- sqrt((sigma * g)/(g+1) * XX)
    beta <- mvrnorm(1, beta_mean, beta_sd)
    mat[i, ] <- c(beta, sigma)
  }
  mat
}

# TODO: In sigma, you have to express beta hat as a function of betas

###### DEFINE VARIANCE DISTRIBUTION #######
y <- rnorm(10)
X <- matrix(data = rnorm(20), ncol = 2, nrow = 10)
n <- 10
XX <- t(X) %*% X
beta_hat <- solve(XX) %*% t(X) %*% y
beta_mean <- c(0,0)
resid_sigma <- (t(y-X %*% beta_hat) %*% (y-X %*% beta_hat))^2
g <- 1
var_sigma <- resid_sigma/2 + 1/(2*(g+1)) %*% t(beta_mean-beta_hat) %*% XX %*% (beta_mean-beta_hat)
#shape_sigma <- (n^2)/4 * var_sigma
#scale_sigma <- 2/n %*% var_sigma
#shape_sigma <- n^2/(4 * var_sigma) + 2
#scale_sigma <- n^3/(8 * var_sigma) + n
shape_sigma <-  n/2
scale_sigma <- resid_sigma/2 + 1/(2*(g+1)) %*% t(beta_mean-beta_hat) %*% XX %*% (beta_mean-beta_hat)
sigma <- rinvgamma(1, shape = shape_sigma, scale = scale_sigma)

###### DEFINE BETA DISTRIBUTION #######

beta_mean <- g/(g+1) * (beta_mean/g + beta_hat)
beta_sd <- sqrt((sigma * g)/(g+1) * XX)
beta <- mvrnorm(1, beta_mean, beta_sd)

              