#######################
# BLASSO GIBS SAMPLER #
#######################

## LOADING PACKAGES
library(invgamma)
library(MASS)
library(statmod)
library(SuppDists)

# HOME
data <- read.table("~/Desktop/data.txt", quote="\"")
# UNI
data <- read.table("//pedley.ads.warwick.ac.uk/user62/u/u1620789/Desktop/data.txt", quote="\"", comment.char="")


###### PREPARE DATA #######

summary(data) # V11 is the dependent variable
y <- data$V11
X <- data[,c(1:10)]
X <- as.matrix(sapply(X, as.numeric))

set.seed(1620789)

###### GIBBS SAMPLER #######
gibbs_blasso <- function(T, b=200, X, y){
  # T - number of iterations
  # b - number of initial iterations to be omitted from the final result - burnin
  
  r <- 1 # shape parameter for lambda - as in Park and Casella
  delta <- 1.78 # scale parameter for lambda as in Park and Casella
  n <- nrow(X) # no of observations
  p <- ncol(X) # no of covariates
  
  # Initial arbitrary D, beta and sigma
  D <- diag(1, p)
  beta <- rep(1, p)
  sigma <- 1
  XX <- t(X) %*% X  
  
  # Result storage
  beta_result <- matrix(ncol = p, nrow = T)
  sigma_result <- matrix(ncol = 1, nrow = T)
  D_result <- matrix(ncol = p, nrow = T)
  lambda_result <- matrix(ncol=1, nrow = T)
  
  for (i in 1: T) {
    
    # Define lambda^2 as a hyperprior
    lambda2 <- rgamma(1, shape = p + r, rate = sum(diag(solve(D))/2) + delta)
    lambda  <- sqrt(lambda2)
    
    # Defining D
    for(t in 1:p){
      D[t,t] <- rinvGauss(1, nu = sqrt((lambda^2 * sigma)/beta[t]^2), lambda = lambda^2)
    }
    
    A <- XX + D
    # Defining beta 
    beta_mean <- solve(A) %*% t(X) %*% y
    beta_var <- sigma * solve(A)
    beta <- mvrnorm(1, beta_mean, beta_var)
    
    # Defining sigma
    resid_sigma <- t((y - X %*% beta)) %*% (y - X %*% beta)
    rate_sigma <- resid_sigma/2 + (t(beta) %*% D %*% beta) / 2 
    sigma <- rinvgamma(1, shape = (n-1)/2 + p/2, rate = rate_sigma)
    
    # Storing results
    beta_result[i,] <- c(beta)
    sigma_result[i,] <- c(sigma)
    D_result[i,] <- c(diag(D))
    lambda_result[i,] <- c(lambda)
  }
  ad_beta_result <- as.matrix(beta_result[c(b:T),])
  ad_sigma_result <- as.matrix(sigma_result[c(b:T),])
  ad_D_result <- as.matrix(D_result[c(b:T),])
  ad_lambda_result <- as.matrix(lambda_result[c(b:T),])
  
  out <- list()
  out$beta <- ad_beta_result
  out$sigma <- ad_sigma_result
  out$D <- ad_D_result
  out$lambda <- ad_lambda_result
  
  return(out)
}

gaby <- gibbs_blasso(T=10000, b=200, X = X, y=y )
gaby$beta
gaby$sigma
gaby$D
gaby$lambda

# performance plots
par(mfrow=c(3,2))
plot(gaby$beta,col=1:1000)
plot(gaby$D,col=1:10000)
plot(gaby$sigma, col=1:1000)
plot(gaby$lambda, col =1:1000)
plot(gaby$beta,type="l")
plot(gaby$D,type="l")
plot(gaby$sigma, type="l")
plot(gaby$lambda, type="l")
plot(ts(gaby$beta[,1]))
plot(ts(gaby$D[,8]))
plot(ts(gaby$sigma))
# the resulting distributions 
par(mfrow=c(2,3))
hist(gaby$beta[,1],40, main = "Distribution of Beta1", xlab = NULL)
hist(gaby$beta[,2],40, main = "Distribution of Beta2",  xlab = NULL)
hist(gaby$sigma,40, main = "Distribution of sigma",  xlab = NULL)
hist(gaby$D[,1][gaby$D[,1]<2],40, main = "Distribution of 1/tilda1^2",  xlab = NULL)
hist(gaby$D[,2][gaby$D[,2]<2],40, main = "Distribution of 1/tilda2^2",  xlab = NULL)
hist(gaby$lambda,40, main = "Distribution of lambda",  xlab = NULL)
par(mfrow=c(1,1))
