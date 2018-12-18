
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
