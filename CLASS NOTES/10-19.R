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


