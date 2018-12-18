
# load the 'dative' data.frame from the package 'languageR'

# display the first 10 lines of the data.frame
head(dative, 10)
head(dative, n = 15)
# find out how many rows are in the data.frame
nrow(dative)
# from here on, we will be interested only in AnimacyOfRec, AnimacyOfTheme, and RealizationOfRecipient
# ... let's recode the RealizationOfRecipient column
dative$RealizationOfRecipient_isNP <- ifelse(dative$RealizationOfRecipient == "NP" , T, F)
# ... and drop all other columns, and save the result in 'dative_essential'
##dative_essential <- dplyr::select(dative, AnimacyOfRec, RealizationOfRecipient_isNP)
##dative_essential <- dative %>% dplyr::select(AnimacyOfRec, AnimacyOfTheme, RealizationOfRecipient_isNP)
dative_essential <- dative %>% dplyr::select(., AnimacyOfRec, AnimacyOfTheme, RealizationOfRecipient_isNP)
# let's get an overview of the structure of the new object
summary(dative_essential)
# let's aggregate the new structure up to counts, and save it in 'dative_counts'
dative_essential %>% group_by(AnimacyOfTheme, AnimacyOfRec) %>% summarize(n_isNP = sum(RealizationOfRecipient_isNP))

dative_essential %>% group_by(AnimacyOfTheme, AnimacyOfRec) %>% 
                     summarize( N_isNP = sum(RealizationOfRecipient_isNP),
                                N_isPP = sum(!RealizationOfRecipient_isNP),
                                Nall1 = sum(RealizationOfRecipient_isNP) + sum(!RealizationOfRecipient_isNP),
                                Nall2 = length(RealizationOfRecipient_isNP),
                                N_total = N_isNP + N_isPP
                     )
# let's get the proportions of NP realizations (i) overall, (ii) by AnimacyOfRec, (iii) by AnimacyOfTheme, and (iv) by both
# ... let's compute them from 'dative_essential'
#(i)
mean(dative_essential$RealizationOfRecipient_isNP)
#(ii)
perc_is_NP_byRec <- dative_essential %>% group_by(AnimacyOfRec) %>% summarize( Proportion_ofNP = mean(RealizationOfRecipient_isNP))
#(iii)
perc_is_NP_byTheme <- dative_essential %>% group_by(AnimacyOfTheme) %>% summarize( Proportion_ofNP = mean(RealizationOfRecipient_isNP))
#(iv)
perc_is_NP_byRecAndTheme <- dative_essential %>% group_by(AnimacyOfRec, AnimacyOfTheme) %>% summarize(Proportion_ofNP = mean(RealizationOfRecipient_isNP))

# ... let's compute them from 'dative_counts'


# let's visualize them in barplots


ggplot(perc_is_NP_byRec, aes(AnimacyOfRec, Proportion_ofNP)) + geom_bar(stat = "identity")

ggplot(perc_is_NP_byRecAndTheme, aes(AnimacyOfRec, Proportion_ofNP, fill = AnimacyOfTheme, group = AnimacyOfTheme)) + geom_bar(stat = "identity")

ggplot(perc_is_NP_byRecAndTheme, aes(AnimacyOfRec, Proportion_ofNP, fill = AnimacyOfTheme, group = AnimacyOfTheme)) + geom_bar(stat = "identity" , position = position_dodge())

# (optional: let's viualize the sample sizes, too, in separate)


### now let's find out what we should believe in, regarding the rates of NP realizations

# ... overall
# ...... prior, likelihood, unstandardized posterior, proper posterior

# ... only for animate themes
# ...... prior, likelihood, unstandardized posterior, proper posterior






library(languageR)
library(magrittr)
library(dplyr)
library(ggplot2)


data(dative)
help(dative)

dative %>% group_by(AnimacyOfRec, AnimacyOfTheme) %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       N = length(RealizationOfRecipient))

dative %>% group_by(AnimacyOfTheme) %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       N = length(RealizationOfRecipient)) %>% 
            ggplot(aes(AnimacyOfTheme, p_realization_NP)) + geom_bar(stat = "identity")

dative %>% group_by(AnimacyOfRec) %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       N = length(RealizationOfRecipient)) %>% 
            ggplot(aes(AnimacyOfRec, p_realization_NP)) + geom_bar(stat = "identity")


dative %>% group_by(AnimacyOfRec, AnimacyOfTheme) %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       N = length(RealizationOfRecipient)) %>% 
            ggplot(aes(AnimacyOfRec, p_realization_NP, fill = AnimacyOfTheme)) + geom_bar(stat = "identity", position = position_dodge())


dative %>% group_by(AnimacyOfRec, AnimacyOfTheme) %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       N = length(RealizationOfRecipient)) %>% 
            ggplot(aes(AnimacyOfRec, p_realization_NP, group = AnimacyOfTheme)) + geom_point() + geom_line() + scale_y_continuous(labels = scales::percent)



# tests for everything
# tests for AnimacyOfTheme == 'animate'

dative %>% group_by() %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       n_realization_NP = sum(RealizationOfRecipient == "NP"),
                       N = length(RealizationOfRecipient))

p_grid <- seq(0, 1, .01)
prior <- rep(1, length(p_grid))
likelihood <- dbinom(2414, size = 3263, prob = p_grid)
unstd_posterior <- likelihood * prior
posterior <- unstd_posterior / sum(unstd_posterior)
plot(posterior)
#########################################
probability of coin on monday > generic monday
joint probability Pr(coin, Monday) / Pr(Monday)
                  Pr(coin|Monday) the same thing

coin fairness is = .5
buttered toast Pbt > .5

english dative? preference for np or pp? the question of animacy?
Pnp > 0.5

P(NP|animate) > P(NP|inanimate)

WE NEED TO TRANSLATE THE QUESTIONS INTO QUESTIONS ABOUT PROBABILITY DISTRIBUTIONS OVER PARAMETERS.
probabilty of probability of the parameter being .5, given the data.
that s why we use bayesian.
p(hypothesis|data) = p(data|hypothesis) * p(hypothesis)
p(d|h) = p(h,d) / p(d)
p(h|d) = p(h,d) / p(h)

we essentially get this:

p(h|d) = (P(data|hypothesis)*p(hypothesis)) / p(data)

p(data) is the model. it is the small world. it is only in bayesian statistics. it is the plausability of the data given the my world knowledge. 

likelihood function, prior distribution, a way to combine them to arrive at a posterior distribution.

LIKELIHOOD
likelihood functions: bernoulli distributions: prb. of success where only one of 2 outcomes is possible. success or failure.
p(x=1) = p,P(x=0)=1 - p

binomial distribution: the prb of K successes in n trials where only one of 2 outcames is possible. parameters n p
for us: dbinom

other likelihood functions: poisson: is just like binominal. prb of K successes in a fixed interval of time, or area. 
for us: dpois

goussiaan: the distribution many sampling distributions converge to, as the sample size increases.
for us: dnorm

likelehood is closely related to the sampling distribution.

#########################################
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
###################################
###let' go through the grid approximation example again
# N = 9 , k =6
Size = 9
Head = 6

#define grid
p_grid <- seq( from = 0 , to = 1 , length.out = 10000)


#set up prior
prior <- rep( 1 , times = length( p_grid ))

#likelihood
likelihood <- sapply(p_grid, function(p) dbinom( Head ,  Size , prob = p))

# unstandardized posterior
unstPosterior <- likelihood * prior
sum(unstPosterior)
#standardize it
posterior <- unstPosterior / sum(unstPosterior)

#plot it
plot(p_grid, posterior)

#sample
##sample sample
set.seed(12345) #defining seed, nothing changes
sample(1:10 , size=10 , replace = TRUE , prob = rep(.1,10))

#our sample
posteriorSample <- sample(p_grid , size = 1e3 , replace = TRUE , prob = posterior)
c(
mean(posteriorSample > .55),
mean(posteriorSample > .2),
mean(posteriorSample > .3)
)

#summarizing
plot(posteriorSample) #this one is just dumb. 
#what should meter is when you sort them
plot(sort(posteriorSample))

quantile(posteriorSample, c(.1,.9))

library(ggplot2)
ggplot(data.frame(x=posteriorSample), aes(x)) + geom_density() + theme_bw() +geom_ribbon(aes(x=x))
#########################
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
################################
# we do not need those long stuff for it, we can use rethinking, map. 
# if we have a yayvan gauissian one we will have a big standard variation, and vice versa. numerical optimization.

# now let s write it downnin a much more compact manner

library(rethinking)
modelCoinDefinition <- alist(
  nHeads ~ dbinom(size = nTosses, prob = p),
  p ~ dunif(min = 0, max = 1)
)

coinModel <- map(flist = modelCoinDefinition,
                 data = list(nHeads = 6, nTosses = 9))
summary(coinModel)

coinModel@call
coinModel@vcov
coinModel@data
coinModel@formula

install.packages("HistData")
library(HistData)
library(dplyr)

# our data is in this package
library(HistData)

# let's extract only some columns from it - rename them and map inches to cm
height <- dplyr::select(PearsonLee, gender = chl, height_inches = child) %>% 
  mutate(height_cm = height_inches * 2.54)

# let's use only a small sample of 200 rows from the data, to speed up computation
height %<>% .[sample(1:nrow(height), size = 200),]

#grid
#create the mu part of the grid, x axis
mu.list <- seq( from=120, to=200, length.out = 100)
#create sigma, z axis
sigma.list <- seq(from=4, to=30, length.out = 100)
#create all combinatios of mu and sigma
grid <- expand.grid(mu=mu.list, sigma=sigma.list)

#likelihood
#define the log-likelihood function
loglik_height_fn <- function(m, sigma) { sum( dnorm(height$height_cm, mean=m, sd=sigma, log=TRUE))}
#use this function to compute log likelihood for every for every po'nt on the gr'd
loglik_height <- sapply( 1:nrow(grid), function(i) loglik_height_fn(grid$mu[i], grid$sigma[i]))

#prior
#define 


#specift this model in a more compact manner as well
modelHeightDefinition <- alist(
  height_cm ~ dnorm( mu, sigma), 
  mu ~ dunif(120, 200),
  sigma ~ dunif(0, 30)
  )
modelHeight <- rethinking::map( modelHeightDefinition, data=height)
summary(modelHeight)

#how to plot it from here
#get samples for mu
nSamples <- 1e4
samplesMu <- rnorm(nSamples, mean = 167.53, sd= 0.94)
samplesSigma <- rnorm(nSamples, mean = 13.25, sd = 0.66)

#visualize
df_plot <- data.frame(mu = samplesMu, sigma = samplesSigma)
library(magrittr)
df_plot %>% ggplot(aes(samplesMu, samplesSigma)) + geom_point(alpha = .1)


#what if we had two coins?

dataTwoCoins <- list(nHeads = c(1,7) , nTosses = c(5, 12))

modelTwoCoinsDefinition <- alist(
  nHeads ~ dbinom(size=nTosses, c(p1, p2)), 
  p1 ~ dunif(0, 1),
  p2 ~ dunif(0, 1)
)

modelTwoCoins <- map(flist = modelTwoCoinsDefinition,
                     data= dataTwoCoins
                     )
summary(modelTwoCoins)


####################################
install.packages("readr")
library(readr)
# read in, and format the frequency data
df <- readr::read_delim("../DATABASE/word_forms_stems_and_frequencies_full.txt", delim = "\t" , comment = "#",
                        col_names = c("wordform", "stem", "morph", "frequency"))
#we have to define what delim is.
#we have a comment in it with hash. so we have to define that too.
#define column names
head(df)
library(dplyr)
morphNoun <- dplyr::filter(df, morph == "Noun+A3sg+P3sg+Nom")

#we want to select the top ten thousand, first we need to order them
library(magrittr)
df %<>% dplyr::arrange(frequency)
### df <- dplyr::arrange(df, frequency)
## but this gave us the least frequent ones at the top. we want the top ones first.
df %<>% dplyr::arrange(desc(frequency))
head(df)
#do nrow for the rows.
nrow(df)

#sum for token frequency

sum(df$frequency)
df$frequency_per_million <- 10^6 * df$frequency / sum(df$frequency)
# extract a subset to work with
df_analysis <- head(df, 10000)
#df_analysis <- df[1:10000]
#deternmine the length
install.packages("stringr")
library(stringr)
df_analysis$wordform_length <- stringr::str_length(df_analysis$wordform)
df_analysis$system_length <- stringr::str_length(df_analysis$stem)

head(df_analysis)


# create a log-frequency column
df_analysis$log_frequency_per_million <- log(df_analysis$frequency_per_million)

idx_sample <- sample(1:nrow(df_analysis), 100)
library(ggplot2)

ggplot(df_analysis[idx_sample,], aes(wordform_length, frequency_per_million)) + geom_point()

# not this one, but we need a log one, that s the meaningful one.
ggplot(df_analysis, aes(wordform_length, log_frequency_per_million)) + geom_point()

# fit a linear model for log(frequency) ~ length
library(rethinking)
data_model_definition_freq_len <- list(
  log_frequency_per_million = df_analysis$log_frequency_per_million,
  wordform_length = df_analysis$wordform_length
)

data_model_definition_freq_len_subset <- list(
  log_frequency_per_million = df_analysis$log_frequency_per_million[idx_sample],
  wordform_length = df_analysis$wordform_length[idx_sample]
)

data_model_definition_freq_len_subset_standardized <- list(
  log_frequency_per_million = df_analysis$log_frequency_per_million[idx_sample],
  wordform_length = scale(df_analysis$wordform_length[idx_sample], center = T, scale = F)
)

model_definition_freq_len <- alist(
  log_frequency_per_million ~ dnorm(mean = mu_predicted, sd = sigma),
  mu_predicted <- intercept + slope * wordform_length,
  intercept ~ dunif(0, 11),
  slope ~ dunif(-10 , 0),
  sigma ~ dunif(0,10)
)

m <- rethinking::map(model_definition_freq_len, data = data_model_definition_freq_len)
m_subset <- rethinking::map(model_definition_freq_len, data = data_model_definition_freq_len_subset)
m_subset_scale <- rethinking::map(model_definition_freq_len, data = data_model_definition_freq_len_subset_standardized)


summary(m)
precis(m)

precis(m_subset_scale, corr = T)
precis(m_subset, corr = T)


m_subsetscaleposterior <- extract.samples(m_subset_scale)
plot(m_subsetscaleposterior)


summary(m_subset)
precis(m_subset)



# hmmm, maybe all these morphemes are obscuring things?

### let's determine the root frequencies then

# let's determine which elements are multimorphemic (grepl)

# let's determine how many morphemes are in each word form (gregexpr)

# let's run another linear model

# what's with all the correlations???


# let's return to Alice's data

### so is there an effect of animacy or what

# model 1

# model 2

# model 3
#########################################

myData = list( nHeads = c( 1 , 7 ) , nTossess = c( 9 , 9 ) )

library( magrittr )
library( rethinking )


mdcm1 <- alist(
  nHeads ~ dbinom( size = nTossess , prob = c( p1 , p2 )) ,
  p1 ~ dunif( 0 , 1 ) ,
  p2 ~ dunif( 0 , 1 )
)

m1 <- map( mdcm1 , data = myData)
precis( m1 , corr = T)

###########
# model 2, we need more detail
mdcm2 <- alist(
  nHeads ~ dbinom( size = nTossess , prob = c( p1 , p2 )) ,
  logit(p1) ~ lop1 , 
  logit(p2) ~ lop2 ,
  lop1 ~ dunif( -100 , 100) ,#p1 ~ dunif( 0 , 1 ) ,
  lop2 ~ dunif( -100 , 100) #p2 ~ dunif( 0 , 1 )
)

m2 <- map(mdcm2 , data = myData)
precis(m2 , corr = T)

inv_logit(c( -3.77 , -2.08 , -.38))


m2Samples <- extract.samples(m2)
smoothScatter(m2Samples)
plot(m2Samples)
rethinking::HPDI(m2Samples$lop1)
rethinking::HPDI(m2Samples$lop2)


rethinking::HPDI(inv_logit(m2Samples$lop1))
rethinking::HPDI(inv_logit(m2Samples$lop2))

# but we need more proof if they are different or biased.
# that s why we need linear model


# before model 3 we need to look at priors
# why do we use uniform? it is meaningless, check the 7B
# when we do not know what to do, we use normal distribution

samplesNormalLOPrior <- dnorm(10^5 , m = 0 , sd = 1)
hist(samplesNormalLOPrior , xlim = c(-10 , 10))

# BETTER:

plot(function(x) dnorm(logit(x) , m = 0 , sd = 1) , xlim = c(0 , 1)) #why it is logit, and not inv_logit

# model 3 is model 2 with this prior

mdcm3 <- alist(
  nHeads ~ dbinom( size = nTossess , prob = c( p1 , p2 )) ,
  logit(p1) ~ lop1 , 
  logit(p2) ~ lop2 ,
  lop1 ~ dnorm(0 , 5) , #lop1 ~ dunif( -100 , 100) ,#p1 ~ dunif( 0 , 1 ) ,
  lop2 ~ dnorm(0 , 5) #lop2 ~ dunif( -100 , 100) #p2 ~ dunif( 0 , 1 )
)

m3 <- map(mdcm3 , data = myData)
precis(m3 , corr = T)
m3Samples <- extract.samples(m3)
smoothScatter(m3Samples)
plot(m3Samples)
rethinking::HPDI(m3Samples$lop1)
rethinking::HPDI(m3Samples$lop2)


rethinking::HPDI(inv_logit(m3Samples$lop1))
rethinking::HPDI(inv_logit(m3Samples$lop2))

# model 4

myDataRevized = list( nHeads = c( 1 , 7 ) , nTossess = c( 9 , 9 ) , coinID = c(0 , 1))
mdcm4 <- alist(
  nHeads ~ dbinom(nTossess , pPredicted) , 
  logit(pPredicted) ~ loP + diff * coinID , 
  loP ~ dnorm( 0 , 20) ,
  diff ~ dnorm( 0 , 20)
)

m4 <- map(mdcm4 , data = myDataRevized)

precis(m4 , corr = T)
m4Samples <- extract.samples(m4)

head(m4Samples)
mean(m4Samples$diff <= 0)

ggplot(m4Samples , aes(diff)) + geom_density()





###########LOG ODDS#######
# transform to log odds
logit(c(.1 , .5 , .9))
#transform from log odds
inv_logit(c( -100 , -2.2 , 0 , 2.2 , 100))


################################
# we have learned the linear modelling.
# we just add other predictors to likelihood function with +, for multivariate models. 
# predicted mean = a + b(parentsheight) + b(polution)
# can we have two different models? two different marginal probability, disregarding each other.
# height ~ N(Pheight) this one disregards the polution and it is a marginal one.

# but what if, we want to determine whether 2 or 3 variables influence another?
# what if we want to determine whether one variable modulates the other's effect?
# what if we want to control for one variable while looking at the effect of another?
#   but why would we want to do that?
#     spurious association, simpson's paradox. #about correlations
#     masked association #about correlations
#     better estimates  #about having better estimates.
# one variable can be affecting the other one? polution affects the child's height. Does it affect the parent height? yes of course.
####      Spurious Asociation
# if the influence of predictors on the dependent variable is analyzed separately, the analysis reveal spurious relationships, 
##                                                                           if the predictors temselves are correlated.
# let's use simulation to understand things better. first we will generate data set.

age <- runif(100, 20, 60)
salary <- 1000 +30*age + rnorm(length(age) , sd = 200)
weight <- 30 +2*age + rnorm(length(age) , sd = 30)
data <- data.frame(age, weight, salary)
plot(data)


model1_def <- alist(
  salary ~dnorm(mu, sigma) ,
  mu <- a +b*weight , # + c*age
  a ~ dnorm(1000 , 20) ,
  b ~ dnorm(0 , 20),
  # c ~ dnorm(30, 20)
  sigma ~ dunif(0 , 500)
)
library(rethinking)
model1 <- map(flist = model1_def, data = data)
precis(model1)

# b confidence interval is really short. it is really sure about that.
model2_def <- alist(
  salary ~ dnorm(mu, sigma) ,
  mu <- a + c*age,
  a ~ dnorm(1000 , 20) ,
  c ~ dnorm(0, 20),
  sigma ~ dunif(0 , 500)
)
model2 <- map(flist = model2_def, data = data)
precis(model2)

# why we do both of them together?

model3_def <- alist(
  salary ~dnorm(mu, sigma) ,
  mu <- a +b*weight + c*age ,
  a ~ dnorm(1000 , 20) ,
  b ~ dnorm(0 , 20),
  c ~ dnorm(0 , 20),
  sigma ~ dunif(0 , 500)
)
model3 <- map(flist =  model3_def , data = data)
precis(model3)

#lets look at the corelation
m3Samples <- extract.samples(model3)
plot(m3Samples)

# lets define Pavel Continuum of Health. 
age <- runif(100, 20, 60)
salary <- 1000 +30*age + rnorm(length(age) , sd = 200)
weight <- 30 +2*age + rnorm(length(age) , sd = 30)
health <- 1000 + -50 * age + 1.5*salary + rnorm(length(age) , sd = 40)
data <- data.frame(age, weight, salary, health)
plot(data)
# we have more health with more salary, less health as we age.

model10_def <- alist(
  health ~ dnorm(mu, sigma) ,
  mu <- a +b*age + c*salary ,
  a ~ dnorm(1000 , 20) ,
  b ~ dnorm(0 , 20),
  c ~ dnorm(0 , 20),
  sigma ~ dunif(0 , 500)
)
model10 <- map(flist =  model10_def , data = data)
precis(model10 , corr = T)
library(magrittr)
precis(model10 , corr = T) %>% plot()
# if you exclude the salary, you will end up with a really wrong implication.
# there is no harm adding new variables, but there is harm excluding ones.

# we need better estimates
age <- runif(1000, 20, 100)
packsSmokedPerDay <- runif(length(age), 0 , 2)
recallScore <- 200 - 0.5*age -
  2*packsSmokedPerDay +rnorm(length(age), sd=5)

data <- data.frame(age, packsSmokedPerDay, recallScore)
plot(data)


modelBaddef <- alist(
  recallScore ~ dnorm(mu, sigma),
  mu <- a + c*packsSmokedPerDay,
  a ~ dnorm(0 , 30),
  c ~ dnorm(0, 20),
  sigma ~ dunif(0 , 500)
)
library(rethinking)
model1Bad <- map(flist = modelBaddef , data = data, control =list(maxit = 10^3))
precis(model1Bad, corr = F)

modelGooddef <- alist(
  recallScore ~ dnorm(mu, sigma),
  mu <- a + c*packsSmokedPerDay + b*age ,
  a ~ dnorm(0 , 30),
  c ~ dnorm(0, 20),
  b ~ dnorm(0 , 20),
  sigma ~ dunif(0 , 500)
)
model1Good <- map(flist = modelGooddef, data = data)
library(magrittr)
precis(model1Good , corr = F) %>% plot 



#multicollinearity

N <- 100 # number of indv
height <- rnorm(N, 10, 2) #sim total height of each
legProp <- runif(N, 0.4 , 0.5) #leg as proportion of height
legLeft <- legProp * height + rnorm( N, 0, 0.02) #sim left leg as proportion plus error
legRight <- legProp * height + rnorm( N, 0 , 0.02)
d<- data.frame(height, legLeft, legRight)

m5.8 <- map(alist(
  height ~ dnorm(mu , sigma),
  mu <- a + bl*legLeft + br*legRight,
  a ~ dnorm(10 , 100),
  bl ~ dnorm(2, 10),
  br ~ dnorm(2 , 10) ,
  sigma ~ dnorm( 0 , 10)
)
, data = d)

precis(m5.8) %>% plot
# why this happens?
# strongly correlated predictors makes you have really wide intervals.
# there is no enough information, and two variables are just the same, omit one.
m5.9 <- map(alist(
  height ~ dnorm(mu , sigma),
  mu <- a + bl*legLeft ,
  a ~ dnorm(10 , 100),
  bl ~ dnorm(2, 10),
  sigma ~ dnorm( 0 , 10)
)
, data = d)

precis(m5.9) %>% plot

#you can also look at the samples to see the correlations between the predictors.

#######################################
library(dplyr)
library(magrittr)
library(rethinking)
library(languageR)

data(dative)

dative$RealizationRec_isNP <- ifelse(dative$RealizationOfRecipient=="NP", 1, 0)
dative %<>% mutate(cAnimacyRec_animate = ifelse(AnimacyOfRec=="animate", 1, 0), 
                   cAnimacyTheme_animate = ifelse(AnimacyOfTheme=="animate", 1, 0) )
dative_essential <- dative %>% dplyr::select(RealizationRec_isNP, cAnimacyRec_animate, cAnimacyTheme_animate)

dative_aggregated <- dative_essential %>% group_by(cAnimacyRec_animate, cAnimacyTheme_animate)  %>% 
  summarize( mean = mean(RealizationRec_isNP) , 
             N_Total = length(RealizationRec_isNP)) 
dative_aggregated$NPs <- dative_aggregated$mean * dative_aggregated$N_Total

# we may want to test if there is a correlation between them.
# we know the Animacy Th and Animacy Rec have an effect on Realization.
# but do these two modify each other?
# we need to have an interaction of them as a parameter.

dative_aggregated$thRecInt <- with(dative_aggregated, cAnimacyRec_animate * cAnimacyTheme_animate)
dative_aggregated

# model, now we add the interaction too 
modelTreatmentContrastDef <- alist(
  NPs ~ dbinom( N_Total , p_predicted ) ,
  logit( p_predicted ) <- intercept + 
    slopeTh * cAnimacyTheme_animate + 
    slopeRec * cAnimacyRec_animate + 
    slopeInt * thRecInt ,
  intercept ~ dnorm( 0 , 1 ) ,
  slopeTh ~ dnorm( 0 , 1 ) ,
  slopeRec ~ dnorm( 0 , 1 ) ,
  slopeInt ~ dnorm( 0 , 1 )
)
m10 <- map( modelTreatmentContrastDef , data = as.list( dative_aggregated ) )
precis( m10 , corr = T) %>% plot()


# slope of theme is only there when the recipient is inanimate. 
# when the recipient is animate, the difference between the R+Th- and R+Th+ is slopeTh plus slopeInt

###############################
# what did we do till today
##  multiariate linear models are useful...
### if we want to determine whether two or three variables influence another.
### if we want to determine whether one variable modulates the other's effect.
### if we want to control for one variable while looking at the effect of another.
####  control for Spurious and masked associations, and get cleaner estimates.
### but less so when predictors are collinear.
### in brief, if we want to find out what s going on.

# a sample of students is measured for height each year for 3 years, After the third year, you want to fit a linear regression
#     predicting height using year as a predictor. write down the mathematical model definition for this regression,
#     using any variable names and priors you choose. Be prepared to defend your choice of priors.
# assume that height is provided in centimeters.
# Explain how to best represent the predictor year, as its representation will change the interpretation of the intercept.
#     Discuss how it will be affected by your choice of predictor coding.
# Choose priors for the intercept and the slope that best encode your a-priori knowledge about the problem. 
#     Explain why you choose these priors. Remember we have only used uniform and gaussian priors, so use one of these.

##PREDICTOR CODING FOR CONTINUOUS VARIABLES
# Centering
#           Centering the predictor (subtracting the mean) affects the interpretation of (i) the intercepts, 
#           and (ii) the slopes of other predictors (including interaction terms)

# Scaling
#           scaling the predictor (dividing by the standard deviation) does not affect the interpretation of the intercept
#           but of the slope for this predictor, and (including interaction terms involving it)

# Contrasts
#           the contrasts that stand in for categorical predictors with two levels can be:
##           treatment contrasts (contr.treatment())
##           sum contrasts (contr.sum())
#           Categorical predictors with three or more levels may require:
##           sliding differences contrasts (MASS::contr.sdif())
##           helmert contrasts (contr.helmert())

# The meaning of interaction terms, too, will depend on how predictors are coded.
recall <- a + b1*isold + b2*issmoker + b3*isold*issmoker
# if b1 and b2 are positive, it means being old, and smoker affects recall positively.
# how do the contrast chosen affect the interpretation of b3?
#   we add the new interaction between isold and is smoker, so we can have an explanation for the things we cannot account without interaction.

# how does scaling of the predictors affect the interpretation of b3?

# bunch of data pavel says here.


######################################################
# TO PLAY WITH INTERACTONS AND ALL OTHER NUMBERS.

library(magrittr)

# use sum contrasts (-.5 / .5) for three factors
contrasts <- list()
contrasts$a_mainef <- c(-1, -1,  1,  1) * 0.5
contrasts$b_mainef <- c(-1,  1, -1,  1) * 0.5
contrasts$ab_interaction <- with(contrasts, a_mainef * b_mainef)

labels <- list()
labels$A <- ifelse(contrasts$a_mainef == -.5, "A1", "A2")
labels$B <- ifelse(contrasts$b_mainef == -.5, "B1", "B2")

coefs <- list(a_mainef = 0, b_mainef = 1, ab_interaction = -10)

effects_byrow <- data.frame(a_mainef = contrasts$a_mainef * coefs$a_mainef, 
                            b_mainef = contrasts$b_mainef * coefs$b_mainef,
                            ab_interaction = contrasts$ab_interaction * coefs$ab_interaction
                            )
effects_byrow$y_pred <- rowSums(effects_byrow)

effects_byrow$A <- labels$A %>% as.factor
effects_byrow$B <- labels$B %>% as.factor 


library(ggplot2)

ggplot(effects_byrow, aes(x = A, y = y_pred, color = B, group = B)) + geom_point() + 
  geom_line() + scale_y_continuous(breaks = seq(-5, 5 , .5))




library(brms)
library(dplyr)
library(languageR)
dative_data <- languageR::dative %>% group_by(AnimacyOfRec, AnimacyOfTheme) %>%
                summarize(N = length(RealizationOfRecipient), N_NP = sum(RealizationOfRecipient == "NP"))
dative_data %<>% within({
  cAnimacyOfRec <- ifelse( AnimacyOfRec == "animate", .5, -.5)
  cAnimacyOfTheme <- ifelse( AnimacyOfTheme == "animate", .5, -.5)
})

# serial
m <- brms::brm(cbind(N, N_NP) ~ cAnimacyOfTheme + cAnimacyOfRec, dative_data, chain = 1, family = "binomial")
m
#####################################



num_weeks <- 1e3

positions <- rep(NA,num_weeks)

current <- 10

for ( i in 1:num_weeks ) {
  positions[ i ] <- current # record current position
  proposal <- current + sample( c( -1 , 1 ) , size= 1 ) # flip coin to generate proposal
  if ( proposal < 1 ) proposal <- 10 # now make sure he loops around the archipelago
  if ( proposal > 10 ) proposal <- 1
  prob_move <- proposal/current   # move?
  current <- ifelse( runif( 1 ) < prob_move , proposal , current )
}

plot(positions , type = "l")
hist(positions)

### FRIDAY CLASS

d <- read.csv("language.csv")

#################################30

library(dplyr)
library(magrittr)
library(languageR)
library(rethinking)
data(dative)

dative %<>% mutate(isNP = ifelse(RealizationOfRecipient == "NP", 1, 0), 
                   cAnTheme = ifelse(AnimacyOfTheme == "animate", 1, 0) - 0.5, 
                   cAnRec = ifelse(AnimacyOfRec == "animate", 1, 0) - 0.5) 
summary(dative %>% dplyr::select(AnimacyOfTheme, AnimacyOfRec, RealizationOfRecipient))

# let's get a sense of where the estimates would lie 
m_lm <- glm(isNP ~ cAnTheme * cAnRec, family = "binomial", data = dative)
summary(m_lm)

# let's get the logit of the weighted overall mean
logit(mean(dative$isNP))

# let's get the logit of the non-weighted mean
unweighted_mean_p_isNP <- 
dative %>% group_by(AnimacyOfTheme, AnimacyOfRec) %>% 
            summarize(isNP = mean(isNP)) %>% 
            ungroup() %>%
            summarize(isNP = mean(isNP))

logit(unweighted_mean_p_isNP)


library(rethinking)
library(brms)

m_map <- rethinking::map(alist(
  isNP ~ dbinom(size = 1, p_predicted),
  logit(p_predicted) <- intercept + 
                          slope_theme * cAnTheme + 
                          slope_rec * cAnRec + 
                          slope_interaction * cAnTheme * cAnRec,
  intercept ~ dnorm(0, 1),
  slope_theme ~ dnorm(0, 1),
  slope_rec ~ dnorm(0, 1),
  slope_interaction ~ dnorm(0, 1)
), data = dative)
summary(m_map)


# The first brms/rstan model. Let's use the bernoulli distribution, and let's run a single chain.
m1 <- brm(isNP ~ cAnTheme * cAnRec, family = "bernoulli", data = dative, chains = 4, cores = 4)
summary(m1)
plot(m1)

m1$model

library(shinystan)
shinystan::launch_shinystan(m1)
########################################################

setwd("~/Dropbox/Academia/Masters/REMEDIAL 1ST/METHODOLOGY/DATABASE/wals-language.csv")

df <- read.csv("language.csv")

View(df)
library(magrittr)
library(dplyr)

df$iso_code
df$X1A.Consonant.Inventories %>% head()
df$X19A.Presence.of.Uncommon.Consonants %>% head()
unique(as.character(df$X1A.Consonant.Inventories))
unique(as.character(df$X19A.Presence.of.Uncommon.Consonants))

dfEssential <- df %>% dplyr::select(iso_code, Name, 
                                    X1A.Consonant.Inventories, 
                                    X19A.Presence.of.Uncommon.Consonants) %>%
  rename(isoCode = iso_code,
         name = Name,
         consInvSize = X1A.Consonant.Inventories,
         uncommCons = X19A.Presence.of.Uncommon.Consonants)
dfEssential$usableLanguage <- with(dfEssential, consInvSize != "" & uncommCons != "")

head(dfEssential , 10)


dfEssential %>% group_by(usableLanguage) %>% 
  summarize(N = length(usableLanguage)) %>%
  mutate(share = N/sum(N))

dfAnalysis <- dfEssential %>% subset(usableLanguage) %>% dplyr::select(-usableLanguage)

head(dfAnalysis , 10)
View(dfAnalysis)
dfAnalysis %<>% group_by(consInvSize) %>% 
                mutate(hasUncomm = ifelse(uncommCons == "1 None" , 0 , 1))

dfAnalysis %>% group_by(consInvSize) %>% summarize(uncommCons = mean(hasUncomm) , 
                                                   N = length(hasUncomm))

# MORE EXPLANATORY WAY
library(ggplot2)

#Funny one
dfAnalysis %>% group_by(consInvSize , uncommCons) %>% summarize(N = length(consInvSize)) %>%
                                                      ggplot(aes(consInvSize, uncommCons)) + geom_point(aes(size = N))
ggplot(dfAnalysis , aes(uncommCons)) + geom_histogram( stat = "count") + facet_wrap(~consInvSize) 


library(brms)

View(dfAnalysis)

dfAnalysis$consInvSize %<>% as.character() %>% as.factor()

m1 <- brm(hasUncomm ~ consInvSize, data = dfAnalysis , family = bernoulli("logit"))
m1

contrasts(dfAnalysis$consInvSize)
# we have too much difference inside the confidence interval of consInvSize2Moderatelysmall,
# is it because we have too wide intercept?
# how can we be sure about a category's interval if we are not sure about the intercept?

# but we see that average one's confidence interval is above zero for sure.
# so we can say that there is definitely a difference between small and average, yet no between small and moderately small.
# but we compare everything to the small languages in this model. but we are interested in comparing small to moderately small,
# moderately small to average, average to moderately large...
# so we need to change contrasts.

contrasts(dfAnalysis$consInvSize) <- MASS::contr.sdif(5)
# why this is set up this way? verify that.

m2 <- brm(hasUncomm ~ consInvSize, data = dfAnalysis , family = bernoulli("logit"))
m2

# now it is compared to each other row by row. now chech m2. we are only sure about that average one matters, but not the other ones.
# so our sample size is too small.

#########################################################

library(rethinking)
library(ggplot2)
library(magrittr)
library(dplyr)

data(Trolley)
d <- Trolley
df <- d %>% group_by(response) %>% summarize(N = length(response))

ggplot(df , aes(response , N)) + geom_bar(stat = "identity")

# one thing can be helpful would be computing the cumulative sum

df$cum_N <- cumsum(df$N)

ggplot(df, aes(response , cum_N)) + geom_bar(stat = "identity")

# we can do percentage too

ggplot(df, aes(response , cum_N/max(cum_N))) + geom_bar(stat = "identity")

# the model

m11.1 <- map(
  alist(
    response ~ dordlogit( phi , c(a1,a2,a3,a4,a5,a6) ),
    phi <- 0, # our linear model is nothing, we are just simply looking for intercepts.
    c(a1,a2,a3,a4,a5,a6) ~ dnorm(0,10)
  ) ,
  data=d ,
  start=list(a1=-2,a2=-1,a3=0,a4=1,a5=2,a6=2.5) )


precis(m11.1)

# we can estimate confidence intervals

m11.2 <- map(
  alist(
    response ~ dordlogit( phi , c(a1,a2,a3,a4,a5,a6) ) ,
    phi <- bA*action + bI*intention + bC*contact,
    c(bA,bI,bC) ~ dnorm(0,10),
    c(a1,a2,a3,a4,a5,a6) ~ dnorm(0,10)
  ) ,
  data=d ,
  start=list(a1=-1.9,a2=-1.2,a3=-0.7,a4=0.2,a5=0.9,a6=1.8) )
precis(m11.2)


# start working on week 11 data

dfAnalysis$consInvSize %<>% droplevels()

dfAnalysis$consInvSizeNum <- as.integer(dfAnalysis$consInvSize)

dfAnalysis %>% dplyr::select(consInvSize ,consInvSizeNum) %>% unique()

noyan <- map(
  alist(
    consInvSizeNum ~ dordlogit( phi , c(a1,a2,a3,a4) ) ,
    phi <- 0, #bA*action + bI*intention + bC*contact,
    #c(bA,bI,bC) ~ dnorm(0,10),
    c(a1,a2,a3,a4) ~ dnorm(0,10)
  ) ,
  data=as.list(dfAnalysis) ,
  start=list(a1=-1.9,a2=-1.2,a3=-0.7,a4=0.2) )
precis(noyan)

inv_logit(coef(noyan))

inv_logit(coef(noyan)) %>% c(0 , . , 1) %>% diff()

head(dfAnalysis)

noyan <- map(
  alist(
    consInvSizeNum ~ dordlogit( phi , c(a1,a2,a3,a4) ) ,
    phi <- bA*hasUncomm,
    bA ~ dnorm(0,10),
    c(a1,a2,a3,a4) ~ dnorm(0,10)
  ) ,
  data=as.list(dfAnalysis) ,
  start=list(a1=-1.9,a2=-1.2,a3=-0.7,a4=0.2) )
precis(noyan)

# the precis telss us that the numbers provide the percentages of the???
# derse gidip burayi tekrar sor.

#############################################

setwd("~/Dropbox/Academia/Masters/REMEDIAL 1ST/METHODOLOGY/DATABASE")
dfmatrix <- read.csv2("matrix.csv")
View(dfmatrix)

library(brms)
fitmatrix <- brm(formula = rating ~ 1 , data = dfmatrix , family = cumulative)
summary(fitmatrix)

head(dfmatrix$movie)

contrasts(dfmatrix$movie)

contrasts(dfmatrix$movie) <- contr.sum(2)

fit1matrix <- brm(formula = rating ~ 1 + movie, data = dfmatrix , family = cumulative)
summary(fit1matrix)

samples <- rstan::extract(fit1matrix$fit)

mean(samples$b < 0)

fit2matrix <- brm(formula = rating ~ 1 + movie + (1|participant.id), data = dfmatrix , family = cumulative)
# too less data

library(languageR)
data("danish")

ggplot(danish , aes(PrevError , LogRT)) + stat_boxplot()

danish$cPrevError <- ifelse(danish$PrevError == "CORRECT" , -.5 , .5)

fitDanishog <- brm(formula = LogRT ~ 1 + cPrevError, data = danish , chains = 4, cores = 4)
summary(fitDanishog)
exp(6.79)

fitDanish <- brm(formula = LogRT ~ 1 + cPrevError + (1|Subject), data = danish , chains = 4, cores = 4)
summary(fitDanish)

#########################3

library(ggplot2)
library(languageR)
library(magrittr)
library(dplyr)

data(danish)
ggplot(danish , aes(PrevError , LogRT)) + stat_boxplot()
head(danish)

danish$cPrevError <- ifelse(danish$PrevError == "CORRECT" , -.5 , .5)

# plot showing average logRTs after cdorrect and incorrect responses on the previous trial, broken down by gender

danishAverages <- danish %>% group_by(Sex, PrevError) %>%
  summarize(avLogRT = mean(LogRT) , 
            avg_RT = mean(exp(LogRT))) %>% as.data.frame()

plotBase <- danishAverages %>% ggplot(aes(x = PrevError , y = avg_RT))
plotBaseLog <- danishAverages %>% ggplot(aes(x = PrevError , y = avLogRT))
plotLine <- geom_line(aes(group = Sex , color = Sex))
plotDot <- geom_point(aes(group = Sex , color = Sex))

plotBaseLog + plotLine + plotDot

plotBase + plotLine + plotDot

# bar or column plot is not really meaningful because we care about the difference
plotBase + geom_col(aes(group = Sex , fill = Sex) , position = "dodge")
plotBase + geom_bar(aes(group = Sex , fill = Sex) , stat = "identity" , position = "dodge")
plotBaseLog + geom_col(aes(group = Sex , fill = Sex) , position = "dodge")
plotBaseLog + geom_bar(aes(group = Sex , fill = Sex) , stat = "identity" , position = "dodge")

p <- plotBaseLog + plotLine + plotDot + facet_wrap(~Sex)

# there may be participants who have not data in everycell, plot by participant

danishAverages_bySubject <- danish %>% group_by(Subject, PrevError) %>%
  summarize(avLogRT = mean(LogRT) , 
            avg_RT = mean(exp(LogRT))) %>% as.data.frame()
danishAverages_bySubject %>% ggplot(aes(x = PrevError , y = avLogRT , group = Subject)) + geom_line() + geom_point()
danishAverages_bySubject %>% ggplot(aes(x = PrevError , y = avg_RT , group = Subject)) + geom_line() + geom_point()

p <- plotBaseLog + plotLine + plotDot + facet_wrap(~Sex)
print(p)
p_pretty <- p + ggplot2::xlab("") + ylab("Average log RT") + ggtitle("Post Error")
ggsave(p_pretty , file = "./example.pdf" , width = 5 , height = 5 )

# you see 2s21 do not have post error data. we need to exclude 2s21, by subsetting, do it on raw data

danish_Errorsubset <- danish %>% subset(Subject != "2s21")

# if we wanna exclude more than one
# danish_Errorsubset <- danish %>% subset(!Subject %in% c("2s21" , "2s22"))

# removed unused levels
danish_Errorsubset %<>% droplevels()
unique(danish_Errorsubset$Subject) %>% sort()

# now we can work on data again as we have deleted the empty cells, model

danish_Errorsubset$cPrevError <- ifelse(danish_Errorsubset$PrevError == "CORRECT" , -.5 , .5)

# model 1 : no random effects
### (1|Subjects) : varying intercepts by Subjects, we use this when we do not wanna account for this variation

library(brms)
m1 <- brm(formula = LogRT ~ 1 + cPrevError , data = danish_Errorsubset , family = gaussian() , chains = 1)
summary(m1)

plot(m1)
# we could be looking at differences of participants

# model with varying effects
m2 <- brm(formula = LogRT ~ 1 + cPrevError + (1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 4 , core = 4 )

plot(m2)
summary(m2)
summary(m1)

# our model was logRT ~ a + b*cPrevError
# we have to dısregard participant differences
#     so our model now: logRT~ a + a_predicted + b*cPrevError => model m2
# but now we have to disregard stupidly crazy edges and differences
#     so our model now: logRT~ a + a_predicted + (b+b_predicted)*cPrevError => model m3
# we do this to account for the big deviations and also not to deal with them.

m3 <- brm(formula = LogRT ~ 1 + cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 4 , core = 4 )
summary(m3)
plot(m3)

# negative correlation in corr(intercept , cPrevError) means that the higher slopes can affect less 
# than the lower ones. but the credible interval is almost 2 which can be max 2. n
# with more participants we will get more clear picture. we have nothing to say about correlation.

# lets include the sex
# get the contrast matrix for sex
contrasts(danish_Errorsubset$Sex)
# set the contrast to sum contrasts
contrasts(danish_Errorsubset$Sex) <- contr.sum(2)

# dont use it
contrasts(danish_Errorsubset$Sex) %<>% contr.sum(nrow(.))


m4 <- brm(formula = LogRT ~ 1 + Sex + cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 4 , core = 4 )
# but this model doesnt inform us. The difference is what we wanna now. So interaction.

m5 <- brm(formula = LogRT ~ 1 + Sex * cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 4 , core = 4 )
danish_Errorsubset %>% select(Sex, Subject) %>% unique %>% .$Sex %>% summary()

summary(m5)
# model 6: no sex, but word freq categorical
# we will include a 3-way prior
# we will use log word freq

words <- danish_Errorsubset %>% dplyr::select(Word, LogWordFreq) %>% unique()
head(words)
plot(words$LogWordFreq)


# low high or middle word freq

danish_Errorsubset$LogWordFreqCat <- danish_Errorsubset$LogWordFreq %>% Hmisc::cut2(g = 3)
levels(danish_Errorsubset$LogWordFreqCat) <- c("low" , "mid" , "high")

contrasts(danish_Errorsubset$LogWordFreqCat)
contrasts(danish_Errorsubset$LogWordFreqCat) <- contr.sum(3)

m6 <- brm(formula = LogRT ~ 1 + LogWordFreqCat * cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 1)
summary(m6)
# look at the contrast to get an idea.
# 7th model
# it is always better to use continous variable.
# remember the reason

m7 <- brm(formula = LogRT ~ 1 + LogWordFreq * cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 1)
summary(m7)

# center the predictor, contrasts affect each other.
m8 <- brm(formula = LogRT ~ 1 + scale(LogWordFreq) * cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 1)
summary(m8)

