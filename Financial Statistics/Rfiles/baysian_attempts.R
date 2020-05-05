#Bayesian Analysis 

theta <- runif(1,0,1)

prior <- function (r,s,n,k){
  rhs <- theta^(r-1) * (1-theta)^(s-1)
  lhs <- factorial(r+s)/(factorial(r)*factorial(s)) 
  lhs.reqs <- factorial(r+s)/(2*factorial(r)) ### WATCH THE SPECIAL CASE
  g <- rhs*lhs
  print(g)
}
prior(1,1,4,3)


posterior <- function(r,s,n,k) {
  lhs <- factorial(n+r+s)/(factorial(k+r)*factorial(n-k+s))
  rhs <- theta^(k+r-1) * (1 - theta)^(n-k+s-1)
  post <- lhs*rhs
  print(post)
}
posterior(1,1,4,3)


beta.prior.post <- function(r,s,n,k) {
  pr.rhs <- theta^(r-1) * (1-theta)^(s-1)
  pr.lhs <- factorial(r+s)/(factorial(r)*factorial(s)) 
  lhs.reqs <- factorial(r+s)/(2*factorial(r)) ### WATCH THE SPECIAL CASE
  prior <- pr.rhs*pr.lhs
  print(prior)
  
  lhs <- factorial(n+r+s)/(factorial(k+r)*factorial(n-k+s))
  rhs <- theta^(k+r-1) * (1 - theta)^(n-k+s-1)
  post <- lhs*rhs
  print(lhs)
  print(post)
}

beta.prior.post(1,1,4,3)
beta.prior.post(1,1,20,11)
beta.prior.post(4,4,4,3)
beta.prior.post(4,4,20,11)


beta.prior <- function(r,s,n,k) {
  pr.rhs <- theta^(r-1) * (1-theta)^(s-1)
  pr.lhs <- factorial(r+s)/(factorial(r)*factorial(s)) 
  lhs.reqs <- factorial(r+s)/(2*factorial(r)) ### WATCH THE SPECIAL CASE
  prior <- pr.rhs*lhs.reqs
  #print(prior)
}
beta.post <- function(r,s,n,k){
  lhs <- factorial(n+r+s)/(factorial(k+r)*factorial(n-k+s))
  rhs <- theta^(k+r-1) * (1 - theta)^(n-k+s-1)
  post <- lhs*rhs
  #print(post)
}

beta.prior(1,1,4,3)
beta.post(1,1,4,3)

priors <- c()
posts <- c()
thetas <- c()
N <- 10000

while( length(posts) < N )
{
  theta <- runif(1,0,1)
  thetas <- append (thetas, theta)
  pri <- beta.prior(4,4,20,11)
  priors <- append(priors, pri)
  
  pos <- beta.post(4,4,20,11)
  posts <- append(posts , pos)
}
png("../Problem Sets/442011_prior2.png")
plot(thetas,priors, main="(4,4),(20,11)")
dev.off()
png("../Problem Sets/442011_post.png")
plot(thetas,posts, main="(4,4),(20,11)")
dev.off()

x = seq(0,1, by=0.01)
pbeta(x,0.5,1)


x <- seq(0, 1, length = 21)
dbeta(x, 1, 1)
?rbeta
?pbeta
betaint(0.5,1,1)
  x <- rbeta(n=500,1,1)
est.par <- eBeta(x); est.par

curve(dbeta(x,1,1)) # plot the prior
