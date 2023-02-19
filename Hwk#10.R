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

#Ex 21 (Part 1) using Newton-Raphson method to calculate exact C.I.
no.rep <- 1000 #replication
p <- 0.15; n <- 20 #binomial(n=20,p=0.15)
le95 <- rep(NA,no.rep) #lower bound
re95 <- rep(NA,no.rep) #upper bound
x<-c()
for(i in 1:no.rep){
  set.seed(i)
  x[i] <- rbinom(1,n,p)}
   for(i in 1: no.rep){
      if (x[i] != 0) {
        paste(x[i])
      }else{
         x[i] <- rbinom(1,n,p)
      } 
ftn1 <- function(p) {  
  fp <- (-0.975)
  dfp <- 0
  for (k in 0:x[i]-1) {
    fp <- fp+choose(20,k)*(p^k)*((1-p)^(20-k))
    dfp <- dfp+choose(20,k)*(k*(p^(k-1))*((1-p)^(20-k))-(p^k)*(20-k)*((1-p)^(19-k)))
  }
  return(c(fp, dfp))
}
#PU
ftn2 <- function(p) {  
  fp <- (-0.025)
  dfp <- 0
  for (k in 0:x[i]) {
    fp <- fp+choose(20,k)*(p^k)*((1-p)^(20-k))
    dfp <- dfp+choose(20,k)*(k*(p^(k-1))*((1-p)^(20-k))-(p^k)*(20-k)*((1-p)^(19-k)))
  }
  return(c(fp, dfp))
}
le95[i] <-newtonraphson(ftn1,0.1, 1e-9)
re95[i] <-newtonraphson(ftn2,0.25, 1e-9)
 }
mean(re95-le95) #length
mean((le95<=p) & (p<=re95)) #coverage



#Ex 21 (Part 2) generating random numbers and comparing coverage and length of 95% asymptotic C.I.
l95 <- rep(NA,no.rep) #lower bound
r95 <- rep(NA,no.rep) #upper bound

for(i in 1:no.rep){
  set.seed(i)
  phat <- rbinom(1,n,p)/n
  l95[i] <- phat-qnorm(0.975)*sqrt((phat*(1-phat))/n)
  r95[i] <- phat+qnorm(0.975)*sqrt((phat*(1-phat))/n)
}
l95[l95 < 0] <- 0
mean(r95-l95) #length
mean((l95<=p) & (p<=r95)) #coverage


