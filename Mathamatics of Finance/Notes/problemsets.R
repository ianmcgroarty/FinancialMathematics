#install.packages('lpSolve')
#install.packages('matlib')
#install.packages('knitr')
library(lpSolve)
library(matlib)

### EXAMPLE 5.2 (107)
# Set up Problem
# Maximize: 
# 4x1+5x2+3x3+4.3x4+x5+1.5x6+2.5x7+.3x8+2x10
# Subjext to: 
# 2x1+3x2+1.5x3+2.2x4+0.5x5+1.5x6+2.5x7+0.1x8+0.6x9+x10 <= 5
# x1+x2+x3+x4 <= 1
# x5+x6+x7	<= 1
# x8+x9+x10	<= 1	

f.obj <- c(4,5,3,4.3,1,1.5,2.5,.3,1,2)
f.con <- matrix(c(2,3,1.5,2.2,.5,1.5,2.5,0.1,0.6,1,
                  1,1,1,1,0,0,0,0,0,0,
                  0,0,0,0,1,1,1,0,0,0,
                  0,0,0,0,0,0,0,1,1,1), nrow=4, byrow=TRUE)
f.dir <- c("<=","<=","<=","<=")
f.rhs <- c(5,1,1,1)

lp("max", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE)$solution


#################### PROBLEM 2

f.obj <- c(4,5,3,4.3,1,1.5,2.5,.3,1,2)
f.con <- matrix(c(2,3,1.5,2.2,.5,1.5,2.5,0.1,0.6,1,
                  1,1,1,1,0,0,0,0,0,0,
                  0,0,0,0,1,1,1,0,0,0,
                  0,0,0,0,0,0,0,1,1,1,
                  0,1,0,1,1,0,0,0,0,0), nrow=5, byrow=TRUE)
f.dir <- c("<=","<=","<=","<=","<=")
f.rhs <- c(5,1,1,1,1)

lp("max", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE)$solution

########### Example 5.3 (109)
f.obj <- c(109,94.8,99.5,93.1,97.2,92.9,110,104,102,95.2)
f.con <- matrix(c(10,7,8,6,7,5,10,8,7,100,
                  10,7,8,6,7,5,10,8,107,0,
                  10,7,8,6,7,5,110,108,0,0,
                  10,7,8,6,7,105,0,0,0,0,
                  10,7,8,106,107,0,0,0,0,0,
                  110,107,108,0,0,0,0,0,0,0), nrow=6, byrow=TRUE)
f.dir <- c(">=",">=",">=",">=",">=",">=")
f.rhs <- c(100,200,800,100,800,1200)
lp("min", f.obj, f.con, f.dir, f.rhs)$solution


############# PROBLEM 3
f.obj <- c(150,200,100,100,120,150,240)
f.con <- matrix(c(-90,-80,-50,-20,-40,-80,-80,
                  -58,-80,-100,-64,-50,-20,-100), nrow=2, byrow=TRUE)
f.dir <- c(">=",">=")
f.rhs <- c(-250,-250)
lp("max", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE)$solution


############# PROBLEM 3
f.obj <- c(150,200,100,100,120,150,240,0)
f.con <- matrix(c(90,80,50 ,20,40,80,80,1,
                  58,80,100,64,50,20,100,-0.1), nrow=2, byrow=TRUE)
f.dir <- c("=","<=")
f.rhs <- c(250,250)
lp("max", f.obj, f.con, f.dir, f.rhs, binary.vec = 1:7)$solution


#### Example 6.11 (PG 163) ## Note I think the book may be wrong +

#f.obj <- c(0.141,0.401,0.452,0.166,0.440)
#f.obj <- c( 15.1, 12.5, 14.7 ,9.02 ,17.68)
f.obj <- c(1,1,1,1,1)
f.con <- matrix(c(2.30,0.930,0.62,0.74,-0.23,
                  0.93,1.40,0.22,0.56,0.26,
                  0.62,0.22,1.80,0.78,0.27,
                  0.74,0.56,0.78,3.40,-0.56,
                  -0.23,0.26,-0.27,-0.56,2.06), nrow=5, byrow=TRUE)
f.dir <- c("=","=","=","=","=")
f.rhs <- c(1,1,1,1,1)
lp("min", f.obj, f.con, f.dir, f.rhs)$solution
  

A <- matrix(c(2.30,0.93,0.62,0.74,-0.23,
         0.93,1.40,0.22,0.56,0.26,
         0.62,0.22,1.80,0.78,0.27,
         0.74,0.56,0.78,3.40,-0.56,
         -0.23,0.26,0.27,-0.56,2.06), nrow=5,byrow=TRUE)
#b <- c(1,1,1,1,1)
b <- c( 15.1, 12.5, 14.7 ,9.02 ,17.68)
??showEqn(A,b)
(solve(A,b))
A

230.0378-(sum((1/6)*c(1:6))^2)^2

#### Problem Set Question T and VZ
A <- matrix(c(5.38,5.067,
              5.067,5.758), nrow=2, byrow=TRUE)
b <- c(1,1)
v<-solve(A,b)
v
sv <- sum(v)
w <- (v/sv)
w


############# BLACK SCHOLES ###############################

bs.ds <- function(s,k,o,r,t){
  a1 <- (s/k)
  a2 <- log(a1)
  b1 <- (o^2)/2
  b2 <- (r+b1)*t
  denom <- o*(sqrt(t))
  num <- a2 + b2
  sd1 <- num/denom

  sd2 <- sd1 - denom
  
  
  print(sd1)
  print(sd2)
  }

bs.ds(62,60,.2,.1,0.4166667)


cumnorm <- function(x){
  a1 <- 1/(sqrt(2*pi))
  a2 <- -x^2/2
  a3 <- exp(a2)
  npx <- a1*a3
  
  gamma <- 0.2316419
  k <- 1/(1+(gamma*x))
  b1 <- 0.319381530
  b2 <- -0.35653782
  b3 <- 1.781477937
  b4 <- -1.821255978
  b5 <- 1.330274429
  
  c1 <- (b1*k + b2*(k^2) + b3*(k^3) + b4*(k^4) + b5*(k^5))
  nx <- 1 - npx*c1
  
  print(nx)
}
  cumnorm(0.6412)
  cumnorm(0.512188)

  cumnorm(0.5997288)
  cumnorm(0.2857288)

call <- function(s,k,r,t,d1,d2){
  nd1 <- cumnorm(d1)
  nd2 <- cumnorm(d2)
  a1 <- s*nd1
  b1 <- exp(-r*t)
  b2 <- k*b1*nd2
  ans <- a1-b2
  print(ans)
}
call(62,60,0.1,0.4166667,0.6412, 0.512188)


put <- function(s,k,r,t,d1,d2){
  negd1 <- cumnorm(-d1)
  negd2 <- cumnorm(-d2)
  
  a1 <- s*negd1
  b1 <- exp(-r*t)
  b2 <- k*b1*(negd2)
  
  ans <- b2-a1
  print(ans)
}


Gamma <- function(s,o,ct,x){
  a1 <- 1/(sqrt(2*pi))
  a2 <- -x^2/2
  a3 <- exp(a2)
  npx <- a1*a3

  print(npx)
    
  denom <- s*o*(sqrt(ct))
  ans <- npx/denom
  print(ans)
}

Gamma(40,0.3,0.33,0.9356484)
((0.2575201/0.825)/(43*0.2))^2

## Lecture 12A
bs.ds(90,85,0.3,0.1,.5)
put(90,85,0.1,.5,0.6112,0.399)
call(90,85,0.1,.5,0.6112,0.399)

bs.ds(40,45,0.3,0.08,0.33)
call(40,45,0.08,0.33,-0.444,-0.61642)
Gamma(40,0.3,0.33,-0.444)
cumnorm(-0.444)


## Final
bs.ds(202.75,225,0.314,0.02,1)
put(202.75,225,0.02,1,-0.1109195,-0.4249195)
cumnorm(0.1109195)
Gamma(225,0.02,1,-0.1109195)

bs.ds(202.75,180,0.314,0.02,1)
call(202.75,180,0.02,1,0.599728, 0.2857288)
Gamma(180,0.02, 1, 0.599728)

# Example 13.3
bs.ds(43,40,0.2,.1,0.5)
call(43,40,0.1,0.5,0.9356484,0.794227)

###################### TRASH #################################
###################### TRASH #################################
###################### TRASH #################################
yahoo.stocks <- function(SYM,p1,p2,int,freq) {
  x<- as.POSIXct("1/2/2016  5:00:00 AM", tz="GMT", format="%m/%d/%Y  %H:%M:%S %p")
  as.numeric(x)
  
  url <- paste0( "https://finance.yahoo.com/quote/",SYM,
                 "/history?period1=",p1,"&period2=",p2,
                 "&interval=",int,"&filter=history&frequency=",freq)
  read.csv(url (url))
}
read.csv( url ("https://finance.yahoo.com/quote/VZ/history?period1=1451710800&period2=1484456400&interval=1d&filter=history&frequency=1d"))


