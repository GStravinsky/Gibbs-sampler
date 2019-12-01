data <- read.table("~/Desktop/data.txt", quote="\"")

# PREPARE DATA #
summary(data) # V11 is the dependent variable
y <- data$V11
# standardize y, otherwise immense coefficients
#y_scale <- scale(y)
n <- nrow(data)
X <- data[,c(1:10)]
X <- as.matrix(sapply(X, as.numeric))

#### DEFINE THE CHARARCTERISTICS OF SIGMA DISTRIBUTION #####
#g <- 1 # for simplicity
XX <- t(X) %*% X  
beta_hat <- solve(XX) %*% t(X) %*% y # MLE/OLS solution
#beta0 <- rep(0, 10) # arbitrary choice of the hyperparameter
#resid_sigma <- t(y-X %*% beta_hat) %*% (y-X %*% beta_hat) # s^2
#shape_sigma <-  n/2
#scale_sigma <- resid_sigma/2 + 1/(2*(g+1)) * t(beta0-beta_hat) %*% XX %*% (beta0-beta_hat)
#sigma <- rinvgamma(1, shape = shape_sigma, rate = scale_sigma)

###### DEFINE THE MOMENTS OF BETA DISTRIBUTION #######
beta_mean <- g/(g+1) * (beta0/g + beta_hat)
#beta_sd <- (sigma * g)/(g+1) * XX

set.seed(1620789)

###### GIBBS SAMPLER #######
gibbs <- function(T, b){
  # T - number of iterations
  # b - number of initial iterations to be omitted from the final result - burnin
  g <- 1
  p <- ncol(X)
  beta0 <- rep(0, 10) # arbitrary choice of the hyperparameter
  beta <- rep(0, 10)
  result <- matrix(ncol = 11, nrow = T)
  for (i in 1: T) {
    rate_sigma <- 1/2 * t(y-X %*% beta) %*% (y-X %*% beta) - 1/(2 * g) *  t(beta -  beta0)  %*% XX %*% (beta-beta0)
    shape_sigma <-  (n+p)/2
    sigma <- rinvgamma(1, shape = shape_sigma, rate = scale_sigma)
    beta_sd <- ((sigma * g)/(g+1) * solve(XX))
    beta <- mvrnorm(1, beta_mean, beta_sd)
    result[i, ] <- c(beta[1], beta[2], beta[3], beta[4], beta[5], beta[6], beta[7], beta[8], beta[9], beta[10], sigma)
  }
  adjusted_result <- as.matrix(result[c(b:T),])
  adjusted_result
}

output <- gibbs(T=10000, b=200)
output[c(9000:9050),]

# performance plots
par(mfrow=c(3,2))
plot(output[,11],col=1:1000)
plot(output[,11],type="l")
plot(ts(output[,1]))
plot(ts(output[,2]))
plot(ts(output[,11]))
# the resulting distributions 
par(mfrow=c(1,3))
hist(output[,1],40, main = "Histogram of Beta 1", xlab = NULL )
hist(output[,2],40, main = "Histogram of Beta 2", xlab = NULL )
hist(output[,3],40, main = "Histogram of Beta 3", xlab = NULL )
hist(output[,11], 40, main = "Histogram of Sigma", xlab = NULL ) 
par(mfrow=c(1,1))





