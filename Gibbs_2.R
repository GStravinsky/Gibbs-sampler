
if(!require(invgamma)){
  install.packages("invgamma")
  library(invgamma)
}

if(!require(MASS)){
  install.packages("MASS")
  library(MASS)
}



###### GENERATE DUMMY DATA #######
y <- rnorm(10)
X <- matrix(data = rnorm(20), ncol = 2, nrow = 10)
n <- 10

#### DEFINE THE CHARARCTERISTICS OF SIGMA DISTRIBUTION #####
g <- 1 # for simplicity
XX <- t(X) %*% X  
beta_hat <- solve(XX) %*% t(X) %*% y # MLE/OLS solution
beta0 <- rep(0, 2) # arbitrary choice of the hyperparameter
resid_sigma <- (t(y-X %*% beta_hat) %*% (y-X %*% beta_hat))^2 # s^2
shape_sigma <-  n/2
scale_sigma <- resid_sigma/2 + 1/(2*(g+1)) * t(beta0-beta_hat) %*% XX %*% (beta0-beta_hat)
sigma <- rinvgamma(1, shape = shape_sigma, rate = scale_sigma)

###### DEFINE THE MOMENTS OF BETA DISTRIBUTION #######
beta_mean <- g/(g+1) * (beta0/g + beta_hat)
beta_sd <- (sigma * g)/(g+1) * XX

set.seed(1620789)

###### GIBBS SAMPLER #######
gibbs <- function(T, b){
  # T - number of iterations
  # b - number of initial iterations to be omitted from the final result - burnin
  result <- matrix(ncol = 2, nrow = T)
  for (i in 1: T) {
    sigma <- rinvgamma(1, shape = shape_sigma, rate = scale_sigma)
    beta_sd <- (sigma * g)/(g+1) * solve(XX)
    beta <- mvrnorm(1, beta_mean, beta_sd)
    result[i, ] <- c(beta[1], beta[2], sigma)
  }
  adjusted_result <- as.matrix(result[c(b:T),])
  adjusted_result
}

output <- gibbs(T=10000, b=200)
output[c(9000:9050),]

# performance plots
par(mfrow=c(3,2))
plot(test_output,col=1:1000)
plot(test_output,type="l")
plot(ts(test_output[,1]))
plot(ts(test_output[,2]))
plot(ts(test_output[,3]))
# the resulting distributions 
par(mfrow=c(1,3))
hist(test_output[,1],40)
hist(test_output[,2],40)
hist(test_output[,3],40)
par(mfrow=c(1,1))





              