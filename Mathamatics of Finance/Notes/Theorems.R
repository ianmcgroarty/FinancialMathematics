
eqstrem.irr <- function(r, k, x1,x2,x3,x4){
    v1 <- x1/((1+r)^0)
    v2 <- x2/((1+r)^1)
    v3 <- x3/((1+r)^2)
    v4 <- x4/((1+r)^3)
  
  print(sum(v1,v2,v3,v4))

}
eqstrem.irr(0.05,3,1,1,1,4)
eqstrem.irr(0.05,3,2,1,1,2)

eqstream.npv <- function(r,k,x1,x2,x3,x4){
  v1 <- (x1)/((1+r)^0)
  v2 <- (x2)/((1+r)^1)
  v3 <- (x3)/((1+r)^2)
  v4 <- (x4)/((1+r)^3)
  print(sum(v1,v2,v3,v4))
}
eqstream.npv(0.05,3,1,1,1,4)
eqstream.npv(0.05,3,2,1,1,2)

eqstream.npv(0.05,3,-2,1,1,3)
eqstream.npv(0.05,3,-2,0,2,3)


eqstream.npv6 <- function(r,x1,x2,x3,x4,x5,x6){
  v1 <- (x1)/((1+r)^0)
  v2 <- (x2)/((1+r)^1)
  v3 <- (x3)/((1+r)^2)
  v4 <- (x4)/((1+r)^3)
  v5 <- (x5)/((1+r)^4)
  v6 <- (x6)/((1+r)^5)
  print(sum(v1,v2,v3,v4,v5,v6))
}
eqstream.npv6(0.05,-100,30,30,30,30,30)
eqstream.npv6(0.05,-150,42,42,42,42,42)

eqstrem.irr6 <- function(r, x1,x2,x3,x4,x5,x6){
  v1 <- x1/((1+r)^0)
  v2 <- x2/((1+r)^1)
  v3 <- x3/((1+r)^2)
  v4 <- x4/((1+r)^3)
  v5 <- x5/((1+r)^4)
  v6 <- x6/((1+r)^5)
  
  print(sum(v1,v2,v3,v4,v4,v5,v6))
  
}
eqstrem.irr6(0.05,-100,30,30,30,30,30)
eqstrem.irr6(0.05,-150,42,42,42,42,42)

## Annuity formula (pg. 46)
annuity.a <- function(r,n,P){
  num <- r*((1+r)^n)*P
  denom <- ((1+r)^n)-1
  A <- num/denom 
  print(A)
}

annuity.p <- function(r,n,a) {
  ar <- a/r
  rn <- 1-(1/((1+r)^n))
  P <- ar*rn
  print(P)
}

#Example 3.2
annuity.a(0.01,60,1000)


#Example 3.3
annuity.a(0.07883,30,203150)
annuity.p(0.0765,30,17846.47)

annuity.a(0.16,10,100000)

## Bond Price Formula (pg 53)
bond.price <- function(f,C,l,m,n) {
  denom <- (1+(l/m))^n
  cl <- C/l
  first <- f/denom
  second <- cl*(1-(1/denom))
  P <- first + second
  print(P)
}

bond.price(1000,100,.12,2,20)
bond.price(100,10,.10,2,20)
bond.price(100,10,0.5,2,60)
bond.price(1000,50,0.08,1,20)
bond.price(1100,50,0.105,1,10)


## Forward Rate Formula (pg 79)
fwd.rate.2yr <- function(s1,s2) {
  num <- (1+s2)^2
  denom <- (1+s1)
  f <- (num/denom)-1
  print(f)
}
fwd.rate.2yr(0.07,0.08)
fwd.rate.2yr(0.063,0.069)
