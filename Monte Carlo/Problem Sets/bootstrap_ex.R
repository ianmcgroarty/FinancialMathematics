library(datasets);
presidents


bootstrapExample = function(B)
{
	vctrzdData = as.vector(presidents);
	vctrzdData = vctrzdData[!is.na(vctrzdData)];
	btstrpMeans = rep(0,B);
	for (j in 1:B) {
		btstrpSmpl = sample(vctrzdData,length(vctrzdData), replace = TRUE);
		btstrpMeans[j] = mean(btstrpSmpl);
	}
	hist(btstrpMeans, breaks = 20);
	print("Bootstrap Standard Error");
	btstrpStdErr = sd(btstrpMeans);
	print(btstrpStdErr);
	print("Regular Standard Error");
	regStdErr = sd(vctrzdData)/sqrt(length(vctrzdData));
	print(regStdErr);
	quantile(btstrpMeans, c(.025, .975));
	
}	