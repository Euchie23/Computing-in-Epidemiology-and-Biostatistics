
#Recursive programming
nfact2 <- function(n) {
  # calculate n factorial
  if (n == 1) {
    cat("called nfact2(1)\n")
    return(1)
  } else {
    cat("called nfact2(", n, ")\n", sep = "")
    return(n*nfact2(n-1))
  }
}
nfact2(6)
nfact2(10)


       
       
aa <- cbind(x1 = 3, x2 = c(4:1, 2:5))
rowSums(aa) # apply(x, 1, sum)
colSums(aa) # apply(x, 2, sum)
# “apply” is a very convenient and clever command, for example:
apply(aa, 1, max)
apply(aa, 2, min)
apply(aa, 1, function(y){y[1]^y[2]})
