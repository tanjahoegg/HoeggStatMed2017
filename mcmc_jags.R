model {	
	
	# input: 
	# y = (n10, n01, n11 + n00)	
	# n: number of pairs in main cohort 
	# m: number of pairs in validation cohort 
	# m01: number of pairs with (E1 = 0, E2 = 1) in validation cohort
	# (alpha1, alpha2): hyperparameters for ppv prior 
	# (beta1, beta2): hyperparameters for npv prior
	# output: OR (corrected D-E odds ratio)
	# Note: theta10[1:3] corresponds to (theta10|10, theta01|10, theta11|10 + theta00|10) in
	# the paper

	# model
	y[1:3] ~ dmulti(theta[1:3], n)
	theta[1] <- ppv*npv*theta10[1] + (1-ppv)*(1-npv)*theta01[1] + ppv*(1-npv)*theta11[1] +
			(1-ppv)*npv*theta00[1]
	theta[2] <- ppv*npv*theta10[2] + (1-ppv)*(1-npv)*theta01[2] + ppv*(1-npv)*theta11[2] + 
			(1-ppv)*npv*theta00[2]
	theta[3] <- ppv*npv*theta10[3] + (1-ppv)*(1-npv)*(theta01[3]) + ppv*(1-npv)*(theta11[3]) +
			(1-ppv)*npv*(theta00[3])   
	theta01[1] <- theta10[2]
	theta01[2] <- theta10[1]
	theta01[3] <- theta10[3]
	theta11[2] <- theta11[1]
	theta00[2] <- theta00[1]
	theta10[3] <- 1 - theta10[2] - theta10[1]	
	theta11[3] <- 1 - 2*theta11[1]
	theta00[3] <- 1 - 2*theta00[1]
	
	# corrected OR
	OR <- theta10[1]/theta10[2]	 

	### Priors
	ppv ~ dbeta(alpha1, alpha2)
	npv ~ dbeta(beta1, beta2)
	theta10[2] ~ dbeta(gamma1, gamma2)
	gamma1 <- m01 + 1
	gamma2 <- m - m01 + 1
	theta10[1] ~ dunif(0, 1 - theta10[2])				
	theta11[1] ~ dunif(0, 0.5)
	theta00[1] ~ dunif(0, 0.5)
		
}
