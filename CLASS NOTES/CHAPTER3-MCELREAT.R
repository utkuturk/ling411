# the vector is pGrid, probability is posterior, 
#posterior for globe tossing model

pGrid <- seq( from=0 , to=1 , length.out = 1000 )
prior <- rep( 1 , 1000)
likelihood <- dbinom( 6, size=9 , prob=pGrid )
posterior <- likelihood*prior
posterior <- posterior / sum(posterior)
posterior

#sampling
samples <- sample( pGrid , prob=posterior , size=1e4 , replace=TRUE)
# plotting
plot(samples)
#density estimation
library(rethinking)
dens(samples)

# add up posterior probability where p, proportion of water is <0.5
sum( posterior[ pGrid < 0.5 ])

# do it using samples
sum( samples < 0.5 ) / 1e4

#ask about how much posterior probability lies bw/ 0.5 and .75
sum (samples > 0.5 & samples < .75 ) / 1e4

#where the 80th percentile lies
quantile( samples , 0.8)

#middle 60% lies between 0.2 and .8 find the boundaries>>> percentile intervals
quantile( samples, c(.2 , .8))

#percentile intervals can be misleading due to its shape, and for leaving out the data near p = 1

#highest posterior density interval HPDI. narrowest interval containing the specified probabilty mass.
HPDI ( samples, prob=.1)

#hpdi and pi is only different when the poserior distrubition is highly skewed.

#report highest posterior probability
pGrid[ which.max(posterior)]
chainmode( samples, adj=.01)
mean(samples)
median(samples)
#why MAP, mean, or median. 
#LOSS FUNCTION: rule that tells you the cost associated with ausing any particular point estimate.
#DIFFERENT LOSS FUNCTIONS IMPLY DIFFERENT POINT ESTIMATES
#3 out of 3 tosses
likelihood <- dbinom( 3, size=3 , prob=pGrid)
posterior <- likelihood * prior
posterior <- posterior /sum(posterior)
samples <- sample(pGrid, size=1e4, replace=TRUE, prob=posterior)

#expected loss
#if p=.5
sum( posterior*abs(.5 - pGrid))
#sapply ile repeating this calculation for every possible decision
loss <- sapply( pGrid, function(d) sum(posterior*abs(d - pGrid)))
# loss contains list of losss values, one for each possible D, to find the min
pGrid[ which.min(loss)]
#this is actually posterior median. 
median(samples)

#that was absolute loss, for quadratic loss use (d-p)^2 which is actually mean.
furkan <- seq(from=0, to=1, length.out = 1e8)
sum(furkan)
furkan[1e4]
furkan[1]
furkan[2]

######ThoseVampires######
#define prior
prior <- c(vampire = 0.001)
prior <- c(prior, mortal = 1-prior[[1]])
#specify the likelihood
likelihood_positive_conditional <- c(vampire = 0.95, mortal = 0.01)
#compute product of likelihood and prior
(unst_posterior_vamp_pos <-
    likelihood_positive_conditional['vampire'] *
  prior['vampire'])
(unst_posterior_mortal_pos <-
    likelihood_positive_conditional['mortal'])
marginal_prob_positive <- unst_posterior_mortal_pos + unst_posterior_vamp_pos
unst_posterior_vamp_pos/marginal_prob_positive

#####Coins#######
#data n=9 k=5
#first define grid
pGrid = seq( from=0 , to=1 , length.out = 11)
#define prior
prior = rep(1, 11)
#compute the likelihood at each value in grid
likelihood <- dbinom( 6, size=9 , prob=pGrid )
likelihood
#compute product of likelihood and prior
unstdPosterior <- likelihood * prior
plot(unstdPosterior, type = "l")
#standardize it, so it sums up to 1
posterior <- unstdPosterior / sum(unstdPosterior)
plot(posterior, type= "l")
library(rethinking)
dens(posterior)
