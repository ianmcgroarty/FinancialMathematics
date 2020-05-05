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

## Theorem 5.3.1 (299)
confi <- function(n,k,z) {
  kn <- k/n
  srt <- z*sqrt((kn)*(1-kn)/n)
  upper <- kn - srt
  lower <- kn + srt
  print(upper)
  print(lower)
}
confi(540,192,1.96)
confi(2002,1281,1.96)
confi(540,192,1.96)

## Theorem 6.3.1 (pg 354)
zstat <- function(n,s,u,x) {
  z <- (x-u)/(s/sqrt(n))
  print(z)
}
zstat(16,10,120,122.3)
zstat(25,9.50,145.75,149.75)

## Theorem 6.3.1 (pg 354)
binomz <- function(n,k,p) { 
  num <- k - (n*p)
  denom <- sqrt(n*p*(1-p))
  z <- num/denom
  print(z)
}

binomz(124,67,0.5)
binomz(348,16,0.0833)
binomz(747,60,0.25)

## Theorem 7.5.2 (p 409)
chitest <- function(sumy,sumy2,n,sigma2){
  s2 <- ((n*sumy2)-(sumy)^2)/(n*(n-1))
    print (s2)
  x2 <-  ((n-1)*s2)/sigma2  
    print(x2)
  
}
chitest(5261,1469945,19,1)
chitest(758.62,19195.7938,30,1)

## Theorem 9.2.1 pg(451)
pooledvar <- function(n,m,sx2,sy2) {
  num <- (n-1)*sx2 + (m-1)*sy2
  denom <- n+m-2
  sp2 <- num/denom
  sp <- sqrt(sp2)
print(sp2)
print(sp)
}
pooledvar(8,9,0.0002103,0.0000955)
pooledvar(19103,902,543.0984,468.0464)

#Theorem 9.2.2 (pg 452)
twosample.ttest <- function(x,y,sp,n,m) {
  num <- x-y
  denom <- sp*(sqrt((1/n)+(1/m)))
  t = num/denom
  print(t)
}
twosample.ttest(0.2319,0.2097,0.0121,8,10)
twosample.ttest(54.79192,60.14191,23.23183,19103,902)

## Theorem 9.4.1 pg (469)
zstat.2probs <- function(x,n,y,m) {
  pe <- (x+y)/(n+m)
  num <- (x/n)-(y/m)
  denom <- sqrt((pe*(1-pe)/n)+(pe*(1-pe)/m))
  z <- num/denom
  print(z)
}
zstat.2probs(34,40,19,35)
zstat.2probs(53,91,705,1117)

## Theorm 10.2.1  
factorial(5)/(factorial(2)*factorial(2)*factorial(1)*factorial(0))
(0.7132)^2*(0.270)^2*(0.01) *30
multino.dist <- function(n,k1,k2,k3,p1,p2,p3) {
  frac <- (factorial(n))/(factorial(k1)*factorial(k2)*factorial(k3))
    print(frac)
  ps <- p1 * p2 * p3 
    print(ps)
  prob <- frac * ps 
    print(prob)
}

#Theorem 10.3.1 
chi2distr.2probs <- function(p1,k1,k2) {
  p2 <- 1-p1
  kt <- k1+k2
  d1 <- ((k1-(kt*p1))^2)/(kt*p1)
  d2 <- ((k2-(kt*p2))^2)/(kt*p2)
  d <- d1+d2
  print(d)
}
chi2distr.2probs(3/4,40,4)
chi2distr.2probs(1292.1/5139,1383,3756)
chi2distr.2probs(1292.1/5139,1383,3750)

chisq.test(c(40,4),p=c(.75,.25))
    test <- chisq.test(c(40,4),p=c(.75,.25))
      test$observed
      test$statistic
      test$p.value
    curve(dexp(x,1/2),from=0,to=10,type="l")
  
    
    random <- rexp(1000,1/2)
  observed <- length(10)
    
  ## BINNING
    for (i in 1:9){
        observed[i]=sum(random>i-1 & random<=i)
    }
  observed[10] <- sum(random>9)
  observed
  prob <- pexp(c(1:10),1/2)
    prob
    px <- diff(prob,1)
      px
    px <- c(pexp(1,1/2),px[1:8],pexp(9,1/2,lower.tail = FALSE))
      sum(px)
      px*1000
      
  chisq.test(observed,p=px)
    #chisq.test(c(1383,3750,p=c(1292.1/5139, (1-(1292.1/5139)))))
#chisq.test(c(3750,1383,p=c(0.2514302,0.7485698 )))
5139-1383
((1383-5139*(0.2451))^2)/(5139*(0.2514))
((3756-5139*(0.749))^2)/(5139*(0.749))
11.79248 + 2.252379

## Log Regression (pg 535)
axb <- function(n,logxy,logx,logy,logx2) {
  b <- (n*logxy - logx*logy)/(n*logx2 - (logx)^2)
  loga <- (logy - b*logx)/n
  a <- 10^loga
  print(b)
  print(loga)
  print(a)
}
axb(15,156.038,41.77,52.798,126.604)
axb(11,30.44,17.75,18.02,31.07)
  

par(mfrow=c(2,2))
curve(x^3-3*x, -2, 2)
curve(x^2-2, add = TRUE, col = "violet")


curve(exp(-x),0,5, col = "blue")
curve(.5*exp(-.5*x), add= TRUE, col = "red")


integrand <- function(x) {1/((x+1)*sqrt(x))}
integrate(integrand, lower = 0, upper = Inf)


integrand <- function(x) {.5*exp(-.5*x)}
integrate(integrand, lower = 0, upper = 4)

integrand <- function(x) {exp(-x)}
integrate(integrand, lower = 4, upper = Inf)

0.412 - 2.0639*(11.78848/19.505)
0.412 + 2.0639*(11.78848/19.505)
(-1/144)/(0.276^2)
sqrt(1.2)
sqrt(2.6)
sqrt(0.4)
1/(2*3.14*1.09*2.6*0.6324)
(-.5*(1/(1-(0.6^2))))

1/(sqrt(6.28)*(sqrt(1-(0.6^2))))
0.6*55   
.5*sqrt(10)/2
6-(.5*sqrt(10)/2)
0.75*sqrt(10)
(5-5.209)/(2.3717)
(6.5-5.209)/(2.3717)
0.7054-0.4681
11-0.6*(1.612)/(1.09)
(1-0.6^2)*2.6
(10-10.113)/1.2899
(10.5-10.113)/1.2899
0.5910-0.4721
sqrt(1.664)
0.6179-0.4681
1.289/2
(10.5-10.113)/0.6445
(11-10.113)/0.6445
0.9147-0.7257
.6*(1.612/1.09)
rand
??rand


