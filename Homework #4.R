
# Question S1----
# Using for loop to calculate factorials using function.
fctl<-function(x){
  f<-1
for (i in 1:x) {
     f<-f*i 
}
return(f)
}

fctl(10)


# Using while loop to calculate factorials with function.
fctrl<-function(n){
fctrl <-1
while(n >= 1) {
  fctrl <-fctrl*n
    n <- n-1
  }
return (fctrl)
}

fctrl(10)


# Question 12 ----
# finding which index number is NA and using cat() to move cursor to other line.
x1 <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)
i <- 1
done <- FALSE
while(!done) {
  if(is.na(x1[i])) {
    miss <-i
    cat(miss,"\n")
    done<-TRUE
  }
  i<-i+1
}

# Question 15 ----
#plotting BMI curves for ID51-ID60 and ID1-ID10
bmi <- read.csv("Documents/NTU Third Semester/Computing in Epidemiology and Biostatistics/Data/BMIrepeated.csv")
quartz()
x <- seq(0,9,3)
y <- cbind(bmi$BMI0, bmi$BMI1, bmi$BMI2, bmi$BMI3)
par(mfrow = c(1,2))
plot(x,y[1,],type="b",lwd=1,col=1,lty=1,pch=1,ylim=c(15,50),axes =
       F,xlab="months",ylab="BMI",main="Placebo group")
axis(1, at = x, labels = seq(0,9,3))
axis(2)
for(subj in 2:10){
  lines(x,y[subj,],lty=1,lwd=1,col=subj,type="b",pch=subj)
}
legend("topright",bty="n",
       c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10"),lty=1,col=(1:10),lwd=1,pch=(1:10)
)
plot(x,y[51,],type="b",lwd=1,col=1,lty=1,pch=1,ylim=c(15,50),axes =
       F,xlab="months",ylab="BMI",main="Drug group")
axis(1, at = x, labels = seq(0,9,3))
axis(2)
for(subj2 in 52:60){
  lines(x,y[subj2, ],lty=1,lwd=1,col=subj2-50,type="b",pch=subj2-50)
}
legend("topright",bty="n",
       c("ID51","ID52","ID53","ID54","ID55","ID56","ID57","ID58","ID59","ID60"),lty=1,col=(1:10),lwd=1,pch=(1:10))


#Question S2 ----
# 3D pie chart
install.packages("plotrix")
library(plotrix)
subject <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pct <- round(subject/sum(subject)*100)
lbls <- paste(lbls,pct, sep = " ")
lbls <- paste(lbls,"%",sep = "")
pie3D(subject,labels=lbls,explode=0.1,main="Pie Chart of Countries ")
