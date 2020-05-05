
## Theorem 5.3.1 (299)
binom.confi <- function(n,k,z) {
  kn <- k/n
  srt <- z*sqrt((kn)*(1-kn)/n)
  upper <- kn - srt
  lower <- kn + srt
  print(upper)
  print(lower)
}
binom.confi(540,192,1.96)
binom.confi(2002,1281,1.96)
binom.confi(540,192,1.96)
binom.confi(40,15,1.96)
binom.confi(5.084,2.6438,2.345)
(2.345/(2*(1.039615-0.0004326934)))^2

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
binomz(n=30,=10,p=0.5)
binomz(348,16,0.0833)
binomz(747,60,0.25)
binomz(120,72,0.65)

y <- exp(-x)
plot(y, type = "o")
## Theorem 7.4.1 (391)

# Sample standard deviation (pg. 407?)
sample.sd <- function(DATA){
  (length(DATA)*(sum(DATA^2)) - (sum(DATA))^2)/(length(DATA)*(length(DATA)-1))
}
ex <- c(249,254,243,268,253,269,287,241,273,306,303,280,260,256,278,344,304,283,310)
sample.sd(ex)

samplevar <- function(sumx,sumx2,n){
  s2 <- ((n*sumx2)-(sumx)^2)/(n*(n-1))
    print(s2)
  s <- sqrt(s2)
    print(s)
}
  samplevar(28.51,40.7015,20)

confi <- function(y,s,n,alpha) {
  p1 <- alpha/2
  p2 <- 1 - p1
  t <- qt(c(p1, p2), df=n)
  
  a <- s/sqrt(n)
  b <- t*a
  lower <- y - b
    print(lower)
  upper <- y + b
    print(upper)
}
  confi(1.4255,0.05642,20,.1)  
  
## Theorem 7.4.2 (p 395)
  onesample.ttest <- function(y,u,s,n){
    num <- y-u
    denom <- s/(sqrt(n))
    t <- num/denom
      print(t)
  }
onesample.ttest(0.661,0.618,0.093,20)
onesample.ttest(0.7663,0.8,0.0859,19)
  qt(0.05, df=18)
  pt(-abs(-1.710069),df=18)
  
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

## Theorem 9.2.3 (pg 459) Welch's two sample t
welch.twosample.ttest <- function(x,y,sx2,sy2,n,m) {
  w <- (x-y)/(sqrt((sx2/n)+(sy2/m)))
    print(w)
  t <- sx2/sy2 
    print(t)
  df.num <- ((t+(n/m))^2)
    print(df.num)
  df.denom <- (((t)^2)/(n-1))+((1/(m-1))*((n/m)^2))
    print(df.denom)
  df <- (df.num)/(df.denom)
    print(df)
}
welch.twosample.ttest(18.6,21.9,115.9929,35.7604,12,12)
  ## use df as the degrees of freedom    
  qt(0.025, df=17)
      
      
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
dmultinom(c(2,2,2,2,2,2),12,c(1/21,2/21,3/21,4/21,5/21,6/21))
dmultinom(c(2,2,2,2,2,2),12,c(1,2,3,4,5,6)/21)



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


#chisq.test(c(1383,3750,p=c(1292.1/5139, (1-(1292.1/5139)))))
#chisq.test(c(1383,3750,p=c(0.2514302,0.7485698 )))

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
