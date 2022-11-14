# Ex 18_1 
#newtonraphson function
newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  x <- x0 # x0: the initial value
  fx <- ftn(x)
  iter <- 0
  while ((abs(fx[1]) > tol) & (iter < max.iter)) {
    x <- x - fx[1]/fx[2]
    fx <- ftn(x)
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", x, "\n")
  }
  if (abs(fx[1]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else { # abs(fx[1]) <= tol
    cat("Algorithm converged\n")
    return(x)
  }
}

#plotting log likelihood function for mean number of accidents(lambda) in one month with maximum likelihood estimate.
loglike <- function(x, lambda){
  loglikelihood <- 0
  for (i in 1:length(x)){
      loglikelihood <- loglikelihood + log(exp(-lambda)*(lambda^x[i])/factorial(x[i]))
    }
  return(loglikelihood)
}
x <- c(5, 5, 6, 0, 1, 2, 4, 4, 3, 5, 7, 10)
lambda <- seq(0,12,0.01)
fp2 <- loglike(x,lambda)
plot(lambda,fp2,col=1, type="l")
lambdahat <- sum(x)/length(x)
abline(v=lambdahat)

# solving the maximum likelihood estimate using the newton-raphson method
ftn <- function(lambda) {
  f <- (-length(x))+sum(x)/lambda
  df <- -(lambda^(-2))*sum(x)
  return(c(f,df))
}
f <- (-length(x))+sum(x)/lambda
plot(lambda,f, type="l") # plotting graph to find initial value
abline(h=0, col=2)
newtonraphson(ftn, 2, 1e-6)


#Ex S5_1
bmi <- read.csv("Data/BMIrepeated.csv")
head(bmi)

#constructing design matrix
b <-cbind(rep(1, length(bmi$BMI3)),as.numeric(as.factor(bmi$SEX)), bmi$AGE, bmi$Treatment)
b
#calculating regression coefficients
beta <- solve(t(b)%*%b)%*%t(b)%*%matrix(bmi$BMI3,ncol = 1)
beta
#Calculating residuals
res<-bmi$BMI3-b%*%beta

#estimating sigma2
sigma2 <- sum(res^2)/length(bmi$BMI3)
-(length(bmi$BMI3)/2)*(log(2*pi)+log(sigma2)+1)

#Constructing a model to compare our results
model1 <- lm(BMI3~SEX+AGE+Treatment, data = bmi)
logLik(model1)

