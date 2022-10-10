x <- c(-2,-1,1,2,0)
ifelse(x>0, "Positive", ifelse(x<0,"Negative", "Zero"))
x1 <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)
ifelse(mean(x,na.rm = T)>5000,1,0) #Ex 11

grades <- as.integer(rnorm(10000, 75, 3))
good.grades <- c()
sample.size <- 1
i <- 1
done <- FALSE
while(!done) {
  if(grades[i]>80){
    good.grades[sample.size] <- grades[i]
    sample.size <- sample.size+1
  }
  if(sample.size>100){
    done<-TRUE
  }
  i<- i+1
}
good.grades


setwd('~/Documents/NTU Third Semester/Computing in Epidemiology and Biostatistics/Data')
Ex <- read.table("example.txt", header=T)
head(Ex[,1:7 ])

# Part 2 finding out the height of the fist 100 people over 60 years old
heights <- c()
sample.size <- 1
i <- 1
done <- FALSE
while(!done) {
  if(Ex$Age[i]>60){
    heights[sample.size] <- Ex$Height[i]
    sample.size <- sample.size+1
  }
  if(sample.size>100){
    done<-TRUE
  }
  i<- i+1
}
heights
   
Ex$Height[which(Ex$Age>60)[1:100]]
   

summary(heights)
str(heights)         
dim(heights)

#part 3 finding out the heights of the first 100 female subjects over 60 years old.

heights_F <- c()
sample.size <- 1
i <- 1
done <- FALSE
while(!done) {
  if(Ex$Gender[2]&Ex$Age[i]>60){
    heights[sample.size] <- Ex$Height[i]
    sample.size <- sample.size+1
  }
  if(sample.size>100){
    done<-TRUE
  }
  i<- i+1
}
heights

Ex$Height[which(Ex$Gender[2]|Ex$Age>60)[1:100]]


x1 <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)
i <- 1
done <- FALSE
while(!done) {
  if(is.na(x1[i])) {
    miss <-i
done<-TRUE
  }
i<-i+1
}
miss



prime_nums <- c()
i<-2
done <- FALSE
while(i<100){
  if(any(x == i)) {
    x = c(x[(x %% i) != 0], i)
    prime_nums = c(prime_nums, i)
   
  }
  if(i <=100){
    done<-TRUE
  }
  i<- i+1
}
prime_nums

x
i

Prime <- function()
i <-2
primeno <- c()
for (i in 2:100) {
  if (Prime(i)==1) {
    primeno <- c(primeno,i)
  }
}
