#Ex S2 making a function to calculate number of steps to move 20 disks from rod A to rod C----
disk_mov<- function(n, from, via, to, steps){
  steps <- steps+1
  if(n == 1) {
    print(paste('Move disk', n , 'from', from, 'to', to, steps))
    return()
  } else {
    disk_mov(n - 1, from, to, via,steps)
    print(paste('Move disk', n , 'from', from, 'to', to, steps))
    disk_mov(n - 1, via, from, to, steps) 
    #disk_mov((2^n-1)+1)
    #steps <- 2^n - 1
    #return(paste("No. of steps =", steps))
    #return(disk_mov(2^n-1)+1)
  }
}
disk_mov(3, 'A', 'B', 'C', 0)




 nfact2 <- function(n) {
  # calculate n factorial
  if (n == 1) {
    cat("called nfact2(1)\n")
    return(1)
  } else {
    cat("called nfact2(", n, ")\n", sep = "")
    return(2*nfact2(n-1)+1)
  }
}
nfact2(20)

#Ex S3 using the “apply” command to calculate the medians, maximums, and minimums, of each row and each column----

x <- matrix(c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000,
              3600, 4500, 10000, 8500, 3000, 10000, 1000, NA, 1200, 10000,
              3800, 5500, 9000, 6000, 6600, 3000, 9600, 6500, 8200, 8000,
              5000, 6600, 13000, 4500, 5000, NA, 10600, 9500, 7600, 6000,
              6600, 8000, 17000, 3000, 7000, 1000, 12600, 8500, 6000, NA),5,10, byrow = TRUE)

#Rows
apply(x,1, median, na.rm=TRUE)
apply(x,1, min, na.rm=TRUE)
apply(x,1, max, na.rm=TRUE)
#Columns
apply(x,2, median, na.rm=TRUE)
apply(x,2, min, na.rm=TRUE)
apply(x,2, max, na.rm=TRUE)



#Ex S8 using the 'solve' command to find 'x', 'y' and 'z' from the matrix----
M <- matrix(c(1,-3,1,1,-2,3,1,-1,1),3,3, byrow = TRUE) # the two 3s before the byrow argument arranges the numbers by 3 rows and 3 columns
M
b <-c(4,6,4)
solve(M,b)
