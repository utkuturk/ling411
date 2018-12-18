
# load the 'dative' data.frame from the package 'languageR'
library( languageR )
data( "dative" )
View(dative)
# create a data.frame named 'dative_essential' containing only the columns representing the animacy of the recipient and theme, 
# as well as the realization of recepient
library( dplyr )
library( magrittr )
dative_essential <- dative %>% dplyr::select( . , AnimacyOfRec , AnimacyOfTheme , RealizationOfRecipient)
View(dative_essential)
### In the following, let's answer the the following question: What is the overall (marginal) proportion of NP realizations (henceforth: p_NP)?
### Answer it by perforiming a grid approximation of the posterior distribution of p_NP


# use 'dative_essential' to determine the numbers of NP realizations and PP realizations in the dataset
dative_essential$realNP <- ifelse( dative_essential$RealizationOfRecipient == "NP" , T , F )
(NumberOfNP <- length(which(dative_essential$realNP == 1)))
(NumberOfPP <- length(which(dative_essential$realNP == 0)))

# define a grid for p_NP with 100 points
grid <- seq(from = 0 , to= 1, length.out =100)


# define a likelihood *function* with the parameter p_NP, such that it returns the probability of the data for any given 
# value of p_NP (don't calculate the likelihood here, just define a function which computes the likelihood)
likelihoodfn <- function(p_NP) dbinom(length(which(dative_essential$realNP == 1)), size = (as.integer(nrow(dative_essential))), prob = p_NP)
length(which(dative_essential$realNP == 1))
#return data
# calculate the likelihood for each value on the grid (feel free to use or not to use sapply). store the result in 'likelihood'
likelihood <- sapply(1:length(grid), function(p_NP) likelihoodfn(grid[p_NP]))
1:length(grid)
# define a prior distribution *function* with the parameter p_NP, such that it returns the value of the prior at this point
# for for any given value of p_NP (don't calculate the prior values here, just define a function which determines them)
priorDistributionfn <- function(p_NP) dunif(p_NP, min = 0 , max = 100)
# calculate the prior for each value on the grid. store the result in 'prior'
prior <- sapply(1:length(grid), function(p_NP) priorDistributionfn(grid[p_NP]))
# compute the unstandardized posterior. store the result in 'unst_posterior'
unst_posterior <- prior*likelihood
# standardize the posterior vector. store the results in 'posterior'
posterior <- unst_posterior/sum(unst_posterior)
# plot the posterior for each point on the grid
plot(posterior)
plot(sort(posterior))
# sample 10^4 values from the posterior. store the result in 'samples_posterior'
samples_posterior <-  sample( grid , size=10^4 , replace=TRUE , prob= posterior)
###I get incorrect number of probabilities? I didnt understand the reason
# let's look at the histogram of the posterior
df_samples_posterior <- data.frame(samples_posterior)
library(ggplot2)
ggplot(df_samples_posterior, aes(samples_posterior)) + geom_histogram()


### Now, let's perform the same procedure for a subset of the data. Let's look at the cases where the theme is animate.

# take a subset of 'dative_essential', where the theme is animate. store the result as 'dative_animtheme'
dative_animtheme <- dative_essential[!(dative_essential$AnimacyOfTheme %in% "inanimate"),]
# you can use filter
# use 'dative_animtheme' to determine the numbers of NP realizations and PP realizations in the dataset
(NumberOfNPanimth <- length(which(dative_animtheme$realNP == 1)))
(NumberOfPPanimth <- length(which(dative_animtheme$realNP == 0)))
# (no need to define a grid - we'll use the one defined above)

# define a likelihood *function* with the parameter p_NP, such that it returns the probability of the data for the
# animate theme cases for any given value of p_NP (don't calculate the likelihood here, just define a function which computes the likelihood)
likelihoodfnanimth <- function(p_NP) dbinom(length(which(dative_animtheme$realNP == 1)), size = (as.integer(nrow(dative_animtheme))), prob = p_NP)
# add return code.
# calculate the likelihood for each value on the grid (feel free to use or not to use sapply). store the result in 'likelihood_animtheme'
likelihood_animtheme <- sapply(1:length(grid), function(p_NP) likelihoodfnanimth(grid[p_NP]))
# (no need to define a prior function, we'll re-use the one defined above)

# calculate the prior for each value on the grid. store the result in 'prior_animtheme'
prior_animtheme <- sapply(1:length(grid), function(p_NP) priorDistributionfn(grid[p_NP]))
# compute the unstandardized posterior. store the result in 'unst_posterior_animtheme'
unst_posterior_animtheme <- prior_animtheme*likelihood_animtheme
# standardize the posterior vector. store the results in 'posterior_animtheme'
posterior_animtheme <- unst_posterior_animtheme/sum(unst_posterior_animtheme)
# plot the posterior for each point on the grid
plot(posterior_animtheme)
plot(sort(posterior_animtheme))
# sample 10^4 values from the posterior. store the result in 'samples_posterior_animtheme'
samples_posterior_animtheme <-  sample( grid , size=10^7 , replace=TRUE , prob= posterior_animtheme)
# let's look at the histogram of the posterior
df_samples_posterior_animtheme <- data.frame(samples_posterior_animtheme)
library(ggplot2)
ggplot(df_samples_posterior_animtheme, aes(samples_posterior_animtheme)) + geom_histogram()


### BONUS PROBLEM: How can we determine the posterior for the difference in p_NP between the animate and inanimate theme cases?
#i didnot even understand how I came to this point. right now i dont even know what i have been doing, just following the class notes and examples.
#pls help me.
#we have sample for two things. get samples for both of them. 
