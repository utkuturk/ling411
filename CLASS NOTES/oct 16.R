# we have been dealing with 1 parameter. we need something more.
# bernouilli distr is 1 dimensional.
# bern is with fixed n
##  mean n x p
##  variance n x p x (1-p)
##  mean and variance
# why do we need more complex ones?

######Gaussian Distribution##############
# but what if we need to describe something that is not a proportion?
##  for example: height, iq, sums (or averages) of many independently distributed outcomes
##  inferences are little more  complex than the normal.
##  the gaussia ndistribution is the limit of many processes that amount to summation
##                  or averaging of many independetly distributed outcomes.
##  often, it works fine as an approximation. 
##  to describe many other outcomes, we need to work with more than one parameters.
##  our notation: dnorm

?dnorm

######Multiparameter inference ###########
# i want to learn my grandgrand father height. i wanna know the exact distribution.
# distribution of distribution
# we need a bayes theorem. for that we first need prior and likelihood.s
##  we need: a log likelihood function, a prior distribution, a way to combine them
##                                                            lets use grid approximation again
##  if two different data sets are independent, just multiply it. 
##  function of expand.grid, it will give you the matrix of two variables
##  after having grid, we need the likelihood function. we use log=TRUE and sum in the formula.
##  if you take very small number, log of it will be negative number, but not that small.
##                                                            so the computer will get the difference.
##  define the function, and then put it inside the sapply
