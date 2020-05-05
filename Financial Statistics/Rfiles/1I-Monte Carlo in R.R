##{ Module 1 Lecture 9: Monte Carlo in R

sample
sample(c(1:4),4,replace=FALSE,prob=c(rep(.25,4)))
sample(c(1:4),5,replace=TRUE,prob=c(rep(.25,4)))
#
sample(c("boy","girl"),3,replace=TRUE)
sample(c("boy","girl"),3,replace=TRUE)=="girl"
sum(sample(c("boy","girl"),3,replace=TRUE)=="girl")>1
sum(sum(sample(c("boy","girl"),3,replace=TRUE)=="girl")>=1)
#
count=0
for(i in 1:1000){
  count<-count+sum(sum(sample(c("boy","girl"),3,replace=TRUE)=="girl")>=1)
}
count
count/1000

# What is the area under the function y=x^2 on(0,1)

runif(5)
x<-runif(5)
y<-runif(5)
y<x^2
sum(y<x^2)
runif(5)<runif(5)^2
sum(runif(500000)<runif(500000)^2)/500000


combination <- function(r,k) {
       c <- (factorial(r))/(factorial(k)*(factorial(r-k)))
       return(c)
 }
combination(8,4)

binomial_prob2 <- function(n,k,p) {
       c <- (factorial(n)/(factorial(k)*(factorial(n-k))))
       m <- ((p^k))*((1-p)^(n-k))
       b <- c * m
       return(b)
}
binomial_prob2(25,0,0.2)

hypergeometric <- function(N,n,r,k,w) {
       vec1 <- combination(r,k)
       vec2 <- combination(w,n-k)
       vec3 <- combination(N,n)
       num1 <- vec1*vec2
       ret1 <- num1/vec3
       return(ret1)
 }
hypergeometric(10,5,8,4,2)


#### Module 2 simulation 
count = 0
for ( i in 1:1000) {
  
}
sample
# 7 games, a has a 0.55 % chance of winning any game. 
sample(c("A Win", "A lose"),7,replace=TRUE,prob=c(0.55 , 0.45))
sample(c("A Win", "A lose"),7,replace=TRUE,prob=c(0.55 , 0.45)) == "A Win"
sum(sum(sample(c("A Win", "A lose"),7,replace=TRUE,prob=c(0.55 , 0.45)) == "A Win")>=4)

count = 0
for ( i in 1:100000) {
 count <- count+ sum(sum(sample(c("A Win", "A lose"),7,replace=TRUE,prob=c(0.55 , 0.45)) == "A Win")>=4)
}
count
count/100000
