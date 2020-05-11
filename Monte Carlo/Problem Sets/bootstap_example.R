regressionBootstrap = function(B) 
{
	
	#   Do first multiple linear regression
	
	unmploymentRate = (longley$Unemployed/10)/longley$Population;
	armedForcesRate = (longley$Armed.Forces/10)/longley$Population;
	origRgrsn = lm(longley$GNP ~ unmploymentRate + armedForcesRate);
	pairBootstrap = rep(0,B);
	residBootstrap = rep(0,B);
	
	for (j in 1:B) {
		
		resampledObs = base::sample(seq(1,16),16,replace=TRUE);
		
		#   bootstrap covariates and response
		
		newDataSet = longley[resampledObs,];
		newUnemploymentRate = (newDataSet$Unemployed/10)/newDataSet$Population;
		newArmedForcesRate = (newDataSet$Armed.Forces/10)/newDataSet$Population;
		newRgrsn = lm(newDataSet$GNP ~ newUnemploymentRate + newArmedForcesRate);
		pairBootstrap[j] = newRgrsn$coefficients[2];
		
		#   bootstrap residuals
		
		resampledResids = origRgrsn$residuals[resampledObs];
		alteredRspns = resampledResids + predict(origRgrsn);
		newResidRgrsn = lm(alteredRspns ~ unmploymentRate + armedForcesRate);
		residBootstrap[j] = newResidRgrsn$coefficients[2];
	}
	
	#par(mfrow = c(2,1));
	hist(pairBootstrap);
	hist(residBootstrap);
	
}

regressionBootstrap(100)
longdat <- longley





	
	unmploymentRate = (longley$Unemployed/10)/longley$Population
	armedForcesRate = (longley$Armed.Forces/10)/longley$Population
	origRgrsn = lm(longley$GNP ~ unmploymentRate + armedForcesRate)
	pairBootstrap = rep(0,100)
	residBootstrap = rep(0,100)
	
		resampledObs = base::sample(seq(1,16),16,replace=TRUE)