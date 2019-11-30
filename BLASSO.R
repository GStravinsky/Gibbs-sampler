#######################
# BLASSO GIBS SAMPLER #
#######################

## LOADING PACKAGES
library(invgamma)
library(MASS)
install.packages("statmod")
library(statmod)


# HOME
data <- read.table("~/Desktop/data.txt", quote="\"")
# UNI
data <- read.table("//pedley.ads.warwick.ac.uk/user62/u/u1620789/Desktop/data.txt", quote="\"", comment.char="")


###### PREPARE DATA #######

summary(data) # V11 is the dependent variable
y <- data$V11
X <- data[,c(1:10)]
X <- as.matrix(sapply(X, as.numeric))
p <- ncol(X)
XX <- t(X) %*% X  

set.seed(1620789)


###### GIBBS SAMPLER #######
gibbs_blasso <- function(T, b=200, r, delta){
  # T - number of iterations
  # b - number of initial iterations to be omitted from the final result - burnin
  # lambda - so far constant
  # p - number of covariates
  
  n <- nrow(X)
  p <- ncol(X)
  
  # Initial arbitrary D and beta and sigma
  D <- diag(10, p)
  beta <- rep(10, p)
  sigma <- 10
  
  # Result storage
  beta_result <- matrix(ncol = p, nrow = T)
  sigma_result <- matrix(ncol = 1, nrow = T)
  D_result <- matrix(ncol = p, nrow = T)
  lambda_result <- matrix(ncol=1, nrow = T)
  for (i in 1: T) {
    
    # Define lambda^2 as a hyperprior
    lambda2 <- rgamma(1, shape = p + r, rate = sum(diag(solve(D)))/2 + delta)
    lambda  <- sqrt(lambda2)
    
    # Defining D
    for(t in 1:p){
      D[t,t] <- rinvgauss(1, mean = sqrt((lambda^2 * sigma)/beta[t]^2), shape = lambda^2)
    }
    
    A <- XX + D
    # Defining beta 
    beta_mean <- solve(A) %*% t(X) %*% y
    beta_var <- sigma * solve(A)
    beta <- mvrnorm(1, beta_mean, beta_var)
    
    # Defining sigma
    resid_sigma <- t(y - X %*% beta) %*% (y - X %*% beta)
    scale_sigma <- resid_sigma/2 + (t(beta) %*% D %*% beta) / 2 
    sigma <- rinvgamma(1, shape = (n-1)/2 + p/2, scale = scale_sigma)
    
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

gaby <- gibbs_blasso(T=5000, b=200, r = 1, delta = 1.78 )
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
plot(ts(gaby$D[,9]))
plot(ts(gaby$sigma))
# the resulting distributions 
par(mfrow=c(2,2))
hist(gaby$beta[,1],40)
hist(gaby$beta[,2],40)
hist(gaby$sigma,40)
hist(gaby$D[,10],40)
hist(gaby$lambda,40)
par(mfrow=c(1,1))
