
#Preparing data
resp<- read.csv("Data/resp.csv", header = T)
head(resp)

#Ex20-1 (Part 1) using Newton-Raphson method to find the MLE of the regression coefficients of the logistic regression.

X <- cbind(rep(1, length(resp$outcome)), ifelse(resp$treatment=='P', 1,0), resp$age, resp$baseline) 
head(X)
dim(X)

Y <- resp$outcome

ftn <- function(betacoeff) {
  pi1 <- exp(X%*%betacoeff)/ (1+exp(X%*%betacoeff))
  gradient <- t(X)%*%(Y-pi1)
  hessian <- -t(X)%*%diag(c(pi1*(1-pi1)), length(resp$outcome))%*%X
  return(list(gradient, hessian))
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

newtonraphson(ftn,c(0,0,0,0))
glm(outcome~treatment+age+baseline, family = binomial, data = resp)

#Ex20-1 (Part 2) finding the variance-covariance matrix for the beta coefficients 
beta<-newtonraphson(ftn,c(0,0,0,0))
head(beta)
model<-glm(outcome~treatment+age+baseline, family = binomial, data = resp)
solve(-ftn(beta)[[2]])#varaince-covaraince matrix for mle.
vcov(model) # to check if our calculation above is correct

#Ex20-1 (Part 3) finding the log likelihood at the beta coefficients 
ftn1 <- function(betacoeff) {
  pi1 <- exp(X%*%betacoeff)/ (1+exp(X%*%betacoeff))
  gradient <- t(X)%*%(Y-pi1)
  hessian <- -t(X)%*%diag(c(pi1*(1-pi1)), length(resp$outcome))%*%X
  loglike <- sum(Y*log(pi1/(1-pi1))+log(1-pi1))
  return(list(gradient, hessian, loglike))
}

ftn1(beta) [[3]]

logLik(model)
