###4M4####
# so, the real question here how we distribute our "year." It is either Normal distribution or a uniform one.
# first let me write the first draft:

# height ~ dnorm(mu, sigma)
# mu = a + b*year       ### here the a is our prior height, and b is how it changes every year.
# a ~ dnorm(170, 185)   ### thinking that university students have a normal distribution with a mean of 185, centered around 170.
# b ~ dunif(0, 5)       ### why do I choose uniform? I am not really sure, 
                        ### but I feel like I do not really want a uninformative prior for my slope.
                        ### it is alright for intercept, because I really have no idea regarding their heights.
                        ### but I do know that, for sure, every year their height increases, not decreasese.
                        ### so it has to be over 0, and they show height increase uniformly.
                        ### how about normal distribution? It cannot be centered around 0.
# sigma ~ dunif(0 , 30) ### I have no idea regarding sigma, and I think it is not really relevant yet.

# so, I represented with year's slope with uniform distribution because I think I need to be more informative
# then a normal distribution. 
# Question here, I am parroting the class notes and youtube videos here. 
# Why I do not want to use a  normal distribution not centered around 0? I do not use dnorm(0 , 5)
# because I know year effects the height, and it cannot be centered around zero. 
# but why not something like dnorm(5, 5)

####Programming Assignment####
# the libraries

library(magrittr)
library(rethinking)
library(dplyr)
library(languageR)

# the data
data("dative")
View(dative)

# modify the data to work easily
dativeEssential <- dative %>% dplyr::select( . , AnimacyOfRec , AnimacyOfTheme , RealizationOfRecipient) %>% 
  mutate(NP = ifelse( RealizationOfRecipient == "NP" , 1 , 0 ),
         AnimateRec = ifelse(AnimacyOfRec == "animate" , 1 , 0),
         AnimateTheme = ifelse(AnimacyOfTheme == "animate" , 1 , 0))

# just a reality check if I did something wrong

sum(dativeEssential$AnimateRec)
sum(!dativeEssential$AnimateRec)
sum(dativeEssential$AnimateTheme)
sum(!dativeEssential$AnimateTheme)

# center the predictors around 0
dativeEssential$AnimateRecDiff <- dativeEssential$AnimateRec - .5
dativeEssential$AnimateThemeDiff <- dativeEssential$AnimateTheme - .5

# check
View(dativeEssential)
length(which(dativeEssential$AnimateRec == 1))
length(which(dativeEssential$AnimateTheme == 1))

nrow(dativeEssential)
length(which(dativeEssential$NP == 1))
length(which(dativeEssential$NP == 0))


# linear model definition

modelRealizationDef <- alist(
  NP ~ dbinom(3263, pPredicted), 
  logit(pPredicted) ~ loP + b*AnimateRecDiff + c*AnimateThemeDiff,
  loP ~ dnorm(0, 10),
  b ~ dnorm(0 , 10),
  c ~ dnorm(0 , 10)
)

# map function and precis

mRealization <- map(flist = modelRealizationDef , data = dativeEssential)

precis( mRealization , corr = T)

# sampling

realizationSamples <- extract.samples(mRealization)

smoothScatter(realizationSamples)

plot(realizationSamples)


#HPDIs

rethinking::HPDI(realizationSamples$loP)
rethinking::HPDI(realizationSamples$b)
rethinking::HPDI(realizationSamples$c)

# this HPDIs shows that our confidence interval do not overlap, 
# and the Animacy of Recipient and Animacy of Theme have contrasting effects on the Realization of Dative argument.
# Indeed both of them affect the Realization of dative argument quite powerfully.
# Especially, the animacy of Theme, given that we have mean of -0.92 and quite a small negative number,
                        ### i know -1 is smaller than -0.5, but do we say smallar negative number or a bigger number?
# we can quite confidently say that Inanimate Theme tends to end up creating PP dative argument,
# and Animate Recipient tends to end up creating NP dative argument, even if it is not as strong as the Animacy of Theme situation.

# Density graph of the effect of the Animacy of the Recipient on the Dative Argument Realization, positive x-axis being NP realization
ggplot(realizationSamples , aes(b)) + geom_density()

# Density graph of the effect of the Animacy of the Theme on the Dative Argument Realization, negative x-axis being PP realization
ggplot(realizationSamples , aes(c)) + geom_density()

