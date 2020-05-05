
## Raw data 
year.fixed <- c(3.525, 3.625, 3.383, 3.625, 3.661, 3.791, 3.941, 3.781, 3.660, 3.733)
ARM <-        c(2.923, 3.385, 3.154, 3.363, 3.226, 3.283, 3.427, 3.437, 3.746, 3.438)

## Difference in means
diff.mean.armyear <- abs(mean(ARM) - mean(year.fixed))
  diff.mean.armyear
  
fixed.arm <- c(year.fixed, ARM)  

diffmeans1000 <- c()
N <- 10000
while( length(diffmeans1000) < N) {
  chosen.rates <- sample(fixed.arm, 10, replace = FALSE)
  not.chosen <- setdiff(fixed.arm,chosen.rates)
  
  diff.means <- abs(mean(chosen.rates) - mean(not.chosen))
  
  diffmeans1000 <- append(diffmeans1000, diff.means) 
  length(diffmeans1000)
}

(N - sum(diffmeans1000 <= diff.mean.armyear))/N

