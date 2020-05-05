# inverse transfrom sampling

# This is the number of times you run the process, 
positions <- 1000

# This will be a vector of minimums
samples <- c()

# You add one number to sample each process and you want to run the process position times. 
while( length(samples) < positions )
{
#This is the number computed in the problem. The n needed to produce the minimum
num.samples <-  12 

# The uniform distribution from 0 to 1
U           <-  runif(num.samples,0,1)

# The pdf 
Y <- exp(-U)

# The inverse cdf function
X           <- -log(1-U)

# take the minimum
sortedx <- sort(X) 
minx = min(sortedx)
samples <- append(samples,minx)
}

# Get the ratio of samples <0.2 to total samples. This is your probability. 
less <- sum(samples <0.2)
ratio <- less/positions


# plot
hist(samples, breaks=30, freq=F, xlab='X', main='Generating Exponential R.V.')
curve(dexp(X, rate=2) , 0, 3, lwd=2, xlab = "", ylab = "", add = T)

sqrt(25*04*0.6)
1-25*0.4
-9/7.745967
1-0.1230


# inverse transfrom sampling

# This is the number of times you run the process, 
positions <- 1000

# This will be a vector of minimums
samples <- c()

# You add one number to sample each process and you want to run the process position times. 
while( length(samples) < positions )
{
  #This is the number computed in the problem. The n needed to produce the minimum
  num.samples <-  12 
  
  x <- runif(num.samples,0,1)
  y <- runif(num.samples,1,5)
  pdfx <- .5*x +.5*y
  
  samples = append(samples, pdfx)
}

hist( samples, breaks=30, probability=TRUE, col=gray(.9), lwd=2, main="", ylim=c(0, 1))
