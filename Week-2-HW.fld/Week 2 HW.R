# Euchie Jn Pierre
# Student ID# R10H44002

#Exercise 1
sample <- sample(1:50, size = 50)

boys <- seq(1,50,2) 
girls <- seq(2,50,2) 
boys <- c(sort(sample[lapply(sample, "%%", 2) == 1]))
girls <- c(sort(sample[lapply(sample, "%%", 2) == 0]))
# done just for practice
boys

midterms <- c(11,16,23,31,36,47,50)
Finals <- c(3,9,16,20,27,31,36,49,50) 

intersect(boys,intersect(midterms, Finals))

#or
boys_passed <- intersect(midterms, Finals)
print(list(boys_passed[lapply(boys_passed, "%%", 2) == 1])) # list of boys who passed both midterms and finals

#boys_passed <- list(midterms[lapply(midterms, "%%", 2) == 1],Finals[lapply(Finals, "%%", 2) == 1])
#boys_passed # list of boys who passed both midterms and finals option number 2

intersect(girls,intersect(midterms, Finals))

#or

girls_passed <- intersect(midterms, Finals)
print(list(girls_passed[lapply(girls_passed, "%%", 2) == 0]))

#girls_passed <- list(midterms[lapply(midterms, "%%", 2) == 0] ,Finals[lapply(Finals, "%%", 2) == 0])
#girls_passed # list of girls who passed both midterm and finals

M <- setdiff(midterms,Finals)
print(list(M[lapply(M, "%%", 2) == 1])) # list of boys who passed midtems but failed finals


H <- setdiff(Finals, midterms)             
print(list(H[lapply(H, "%%", 2) == 0])) # list of girls who failed midterms but passed finals


# Exercise 2

setwd('~/Documents/NTU Third Semester/Computing in Epidemiology and Biostatistics/Data')
Seizure <- read.csv('seizure.csv')    
summary(lm(y ~ ltime, data = Seizure))
x <- cbind(rep(1, nrow(Seizure)), Seizure$ltime)
solve(t(x)%*%x)%*%t(x)%*%matrix(Seizure$y, ncol = 1)

#or

reg <- function(y,x) {
  b <- sum ((x-mean(x)) * (y-mean(y)))/sum ((x-mean(x))^2)
  a <- mean(y) -b*mean(x)
  return (c(a,b))
}

reg(Seizure$y,Seizure$ltime)

