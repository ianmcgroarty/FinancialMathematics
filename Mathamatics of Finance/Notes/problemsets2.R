#install.packages('lpSolve')
#install.packages('matlib')
#install.packages('knitr')
library(lpSolve)
library(matlib)
library(pracma)
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
v<-solve(A,b)
v

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

######### MIDTERM QUESTION
A <- matrix(c(2.9658,	1.2313,
              1.2313,	0.8597,
              0.5, 1), nrow=3, byrow=TRUE)
b <- c(1,1,0)
v<-solve(A,b)
v
sv <- sum(v)
w <- (v/sv)
w

f.obj <- c(1,1)
f.con <- matrix(c(2.9658,	1.2313,
                  1.2313,	0.8597), nrow=2, byrow=TRUE)
f.dir <- c("=","=")
f.rhs <- c(1,1)
lp("min", f.obj, f.con, f.dir, f.rhs)$solution



A <- matrix(c(2.9658,	1.2313,
              1.2313,	0.8597), nrow=2, byrow=TRUE)
b <- c(1,1)
v<-solve(A,b)
sv <- sum(v)
w <- (v/sv)
w



A <- matrix(c(0.00566864,	0.002204683,	0.001654137,	0.001760208,
              0.00220468,	0.007438305,	0.003701358,	0.002519245,
              0.00165417,	0.003701358,	0.003803481,	0.002057464,
              0.00176020,	0.002519245,	0.002057464,	0.003926723
              ), nrow=4, byrow=TRUE)
#b <- c(1,1,1,1)
b <- c(0.0088,	0.0339	,0.0154	,0.0130)

v<-solve(A,b)
v
sv <- sum(v)
w2 <- (v/sv)
w2

0.0705/.2454
1/.2454
0.0691/1.105
1/1.105
b <- c(1,1,1,1)
B <- t(rbind(w,w2,b))

wb <- t(rbind(w,w2))
alpha <- matrix(c(0.5,0.5))
wa <- wb %*% alpha

t(wa) %*% 
  (A %*% wa)







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

Sys.setenv(HTTP_PROXY = "c1proxy.frb.org:8080")
Sys.setenv(HTTPS_PROXY = "c1proxy.frb.org:8080")
read.csv( url ("https://query1.finance.yahoo.com/v7/finance/download/%5EIXIC?period1=1546318800&period2=1562126400&interval=1d&events=history&crumb=Duboz7OtwOi.csv"))

