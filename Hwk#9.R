#Preparing data
rate<- read.csv("Data/rate.csv", header = T)
head(rate)


rate$Age.f <- factor(rate$Age)
# Ex20-4 (Part 1) Using using Newton-Raphson method to find the MLE of the regression coefficients of the Poisson regression.
Y <- rate$Death # preparing column vector for Y

#constructing design matrix for X
X <- cbind(model.matrix(~Age.f,rate),ifelse(rate$sex=='m', 1,0)) 
colnames(X)[13] <- "Sex"
head(X)
dim(X)



ftn <- function(betacoeff) {
  mu <- exp(X%*%betacoeff+log(rate$PY/100000))
  gradient <- t(X)%*%(Y-mu)
  hessian <- -t(X)%*%diag(c(mu), length(Y))%*%X
  loglike <- sum(-mu+Y*log(mu)-log(factorial(Y)))
  return(list(gradient, hessian, loglike)) #finding loglikelihood of the betacoeffs
}


newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  x <- x0 # x0: the initial value
  fx <- ftn(x)
  iter <- 0
  while ((max(abs(fx[[1]])) > tol) & (iter < max.iter)) {
    x <- x - solve(fx[[2]]) %*% fx[[1]]
    fx <- ftn(x)
    iter <- iter + 1
  }
  if (max(abs(fx[[1]])) > tol) {
    cat('Algorithm failed to converge\n')
    return(NULL)
  } else { # max(abs(fx[[1]])) <= tol
    cat("Algorithm converged\n")
    return(x)
  }
}

newtonraphson(ftn,c(0,0,0,0,0,0,0,0,0,0,0,0,0)) # running HDNR to find intercept and first 3 regression coeffs
glm(Death~Age.f+sex, offset=log(PY/100000), data=rate, family=poisson)


#Ex20-4 (Part 2) finding the variance-covariance (VCOV) matrix for the beta coefficients.

beta <- newtonraphson(ftn,c(0,0,0,0,0,0,0,0,0,0,0,0,0))
model <- glm(Death~Age.f+sex, offset=log(PY/100000), data=rate, family=poisson)
solve(-ftn(beta)[[2]])# finding variance-covariance (VCOV)matrix for mle.
vcov(model) #to check if our calculation for VCOV above is correct


#Ex20-4 (Part 3) finding the log likelihood at the beta coefficients. 
ftn1 <- function(betacoeff) {
  mu <- exp(X%*%betacoeff+log(rate$PY/100000))
  gradient <- t(X)%*%(Y-mu)
  hessian <- -t(X)%*%diag(c(mu), length(Y))%*%X
  loglike <- sum(-mu+Y*log(mu)-log(factorial(Y)))
  return(list(gradient, hessian, loglike)) #finding loglikelihood of the betacoeffs
}

ftn1(beta) [[3]] # retrieving the 'loglike' from the list.

logLik(model) #using base R function 'loglike' to check answers.
