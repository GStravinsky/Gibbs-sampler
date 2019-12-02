data <- read.table("~/Desktop/data.txt", quote="\"")


if(!require(invgamma)){
  install.packages("invgamma")
  library(invgamma)
}

if(!require(MASS)){
  install.packages("MASS")
  library(MASS)
}

# PREPARE DATA #
summary(data) # V11 is the dependent variable
y <- data$V11
n <- nrow(data)
X <- data[,c(1:10)]
X <- as.matrix(sapply(X, as.numeric))


set.seed(1620789)

###### GIBBS SAMPLER #######
gibbs <- function(T, b, X, y){
  # T - number of iterations
  # b - number of initial iterations to be omitted from the final result - burnin
  # X - covariates in matrix form
  # y - response
  g <- 1
  p <- ncol(X)
  
  beta0 <- rep(0, 10) # arbitrary choice of the hyperparameter
  beta <- rep(0, 10) # initial value of beta
  XX <- t(X) %*% X  
  beta_hat <- solve(XX) %*% t(X) %*% y # MLE solution
  
  # result matrix
  result <- matrix(ncol = 11, nrow = T)
  for (i in 1: T) {
    # Defining sigma distribution
    rate_sigma <- 1/2 * t(y-X %*% beta) %*% (y-X %*% beta) + 1/(2 * g) *  t(beta -  beta0)  %*% XX %*% (beta-beta0)
    shape_sigma <-  (n+p)/2
    sigma <- rinvgamma(1, shape = shape_sigma, rate = rate_sigma)
    
    # Defining beta distribution
    beta_mean <- g/(g+1) * (beta0/g + beta_hat)
    beta_sd <- ((sigma * g)/(g+1) * solve(XX))
    beta <- mvrnorm(1, beta_mean, beta_sd)
    result[i, ] <- c(beta, sigma)
  }
  adjusted_result <- as.matrix(result[c(b:T),])
  adjusted_result
}

output <- gibbs(T=10000, b=200, X=X, y=y)

# performance plots
par(mfrow=c(3,2))
plot(output[,11],col=1:1000)
plot(output[,11],type="l")
plot(ts(output[,1]))
plot(ts(output[,2]))
plot(ts(output[,11]))
# the resulting distributions 
par(mfrow=c(2,3))
hist(output[,1],40, main = "Histogram of Beta 1", xlab = NULL )
hist(output[,2],40, main = "Histogram of Beta 2", xlab = NULL )
hist(output[,3],40, main = "Histogram of Beta 3", xlab = NULL )
hist(output[,4],40, main = "Histogram of Beta 4", xlab = NULL )
hist(output[,5],40, main = "Histogram of Beta 5", xlab = NULL )
hist(output[,6],40, main = "Histogram of Beta 6", xlab = NULL )
par(mfrow=c(2,3)
hist(output[,7],40, main = "Histogram of Beta 7", xlab = NULL )
hist(output[,8],40, main = "Histogram of Beta 8", xlab = NULL )
hist(output[,9],40, main = "Histogram of Beta 9", xlab = NULL )
hist(output[,10],40, main = "Histogram of Beta 10", xlab = NULL )
hist(output[,11], 40, main = "Histogram of Sigma", xlab = NULL ) 
par(mfrow=c(1,1))
