
blackScholes = function(T,N, lambda, mu) {
	
	##   loop over time steps

timeIncrements = seq(0, T,T/N);
W_vec = rep(0, N+1);
X0 = 1;
# Euler-Maruyama (Solution)
EM_soln = rep(0,N+1);
EM_soln[1] = X0;

for (j in 0:N) {
	
	if (j == 0) {
		W = 0;
		EM = X0;
	}
	if (j > 0) {
		dW = rnorm(1,0, sqrt(T/N));
		W = W_vec[j] + dW;
		EM = EM_soln[j] + lambda*EM_soln[j]*T/N + mu*EM_soln[j]*dW;
	}
	W_vec[j+1] = W;
	EM_soln[j+1] = EM;
	
}
Xt = X0*exp((lambda-mu^2/2)*timeIncrements + mu*W_vec);
#par(mfrow = c(2,1));
plot(timeIncrements, Xt);
plot(timeIncrements, EM_soln);

}


blackScholes(0.5,10,0.1,20)







	##   loop over time steps
T <- 1
N <- 10
lambda <- 1
mu <- 20


timeIncrements = seq(0, T,T/N)
W_vec = rep(0, N+1);
X0 = 1
# Euler-Maruyama (Solution)
EM_soln = rep(0,N+1)
EM_soln[1] = X0

for (j in 0:N) {
	
	if (j == 0) {
		W = 0;
		EM = X0;
	}
	if (j > 0) {
	  # Simulate Values of dW 
		dW = rnorm(n=1,mean=0, sd=sqrt(T/N));
		# Add randomeness term 
		W = W_vec[j] + dW;
		# Solution
		EM = EM_soln[j] + lambda*EM_soln[j]*T/N + mu*EM_soln[j]*dW
	}
	W_vec[j+1] = W
	EM_soln[j+1] = EM
	
}


Xt = X0*exp((lambda-mu^2/2)*timeIncrements + mu*W_vec)
plot(timeIncrements, Xt)
plot(timeIncrements, EM_soln)


