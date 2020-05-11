brownianSimulator = function(T,N) {

##   loop over time steps

timeIncrements = seq(0, T,T/N)
W_vec = rep(0, N+1)

for (j in 0:N) {
	
	if (j == 0) {
		W = 0;
	}
	if (j > 0) {
		dW = rnorm(1,0, sqrt(T/N));
		W = W_vec[j] + dW;
	}
	W_vec[j+1] = W;
}
fnctn = exp(timeIncrements + 1/2*W_vec)
plot(timeIncrements, fnctn)
}

brownianSimulator(0.5,100)
