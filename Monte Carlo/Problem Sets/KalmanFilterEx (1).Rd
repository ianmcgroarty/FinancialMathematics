


Kalman_Filtering_Example = function(n) {
	
	#####  Establishing Sigma Values
		sigma_V = .2;
		sigma_W = .1;
	
	##### Creating storage vectors for X, Y, X_pred, X_filt, Omega, Delta and Theta
		X = rep(0, n);
		Y = rep(0, n);
		X_hat_pred = rep(0,n);
		X_hat_filt = rep(0,n);
		Omega = rep(0,n);
		Delta = rep(0,n);
		Theta = rep(0,n);
	
	###### Starting everything with t = 1 
		X[1] = rnorm(1, 0, sigma_V);
		Y[1] = X[1] + rnorm(1,0, sigma_W);
		X_hat_pred[1] = .2;
		Omega[1] = .04;
		Delta[1] = Omega[1] + sigma_W^2;
		Theta[1] = Omega[1]
	
	for (j in 2:n) {
		 
		 #########  doing prediction ##########
			 X_hat_pred[j] = X_hat_pred[j-1] + (Theta[j-1]/Delta[j-1])*(Y[j-1] - X_hat_pred[j-1]);
			 Omega[j] = Omega[j-1] + sigma_V^2 - Theta[j-1]^2/Delta[j-1];
			 Delta[j] = Omega[j] + sigma_V^2;
		 	Theta[j] = Omega[j];
			
		######## simulating next value of X and Y ###########
			X[j] = X[j-1] + rnorm(1, 0, sigma_V);
			Y[j] = X[j] + rnorm(1, 0, sigma_W);
			
		####### filtering   ############
			X_hat_filt[j] = X_hat_pred[j] + (Omega[j]/Delta[j])*(Y[j] - X_hat_pred[j]);
		
		
		
	}
	
	plot(Y, type = "l")
	lines(X_hat_pred, type = "l", col = "red");
	lines(X_hat_filt, type = "l", col = "blue");
	
}