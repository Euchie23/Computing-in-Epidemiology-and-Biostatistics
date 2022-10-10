x<-2:100
prime_nums <- c()
i<-2
done <- FALSE
while(i<100){
  if(any(x == i)) {
    x = c(x[(x %% i) != 0], i)
    prime_nums = c(prime_nums, i)
    
  }
  if(i==100){
    done<-TRUE
  }
  i<- i+1
}
prime_nums
