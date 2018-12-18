library(languageR)
library(magrittr)
library(ggplot2)
library(dplyr)
library(brms)

data("etymology")

#1. Create a new column called WrittenFrequencyCat ('cat' for categorical), which contains the value "high" when WrittenFrequency is high 
# (above the median), and the value "low" when the frequency is below the median.default
etymology$WrittenFrequencyCat <- Hmisc::cut2(etymology$WrittenFrequency, g = 2)
levels(etymology$WrittenFrequencyCat) <- c("low", "high")
summary(etymology$WrittenFrequencyCat)

# or
freq_median <- median(etymology$WrittenFrequency)
etymology$WrittenFrequencyCat <- ifelse(etymology$WrittenFrequency > freq_median, "high", "low")



#2. Create a barplot of the average regularity of verbs (column 'Regular') as a function of WrittenFrequencyCat and Auxiliary. 
# (Using several facets, faceting by frequency: "+ facet_wrap(~WrittenFrequencyCat)")

etymology$isRegular <- ifelse(etymology$Regularity == "regular", 1, 0)

etymology_essential <- etymology %>% dplyr::select(., Regularity, isRegular, WrittenFrequencyCat, Auxiliary, WrittenFrequency)
etymology_aggregated <- etymology_essential %>% group_by(Auxiliary, WrittenFrequencyCat) %>% dplyr::summarize(share_Regular = mean(isRegular), N = length(isRegular))
etymology_aggregated

p <- ggplot(etymology_aggregated, aes(Auxiliary, share_Regular)) + geom_bar(stat = "identity") + facet_wrap(~WrittenFrequencyCat) 
print(p)

# if you want to get fancy about the labels (and you should) you can do this, too
p_fancy <- p + scale_y_continuous(labels = function(x){ paste0(100 * x, "%") }) + ylab("Percentage of verbs with regular inflection")
print(p_fancy)



# 3. Create a *single* *line plot* of the average regularity of verbs as a function of WrittenFrequencyCat and Auxiliary. 
# (Yes, all in one single plot. Put 'Auxiliary' on the x-axis.)

p_lines <- ggplot(etymology_aggregated, aes(Auxiliary, share_Regular, group = WrittenFrequencyCat, color = WrittenFrequencyCat)) + geom_line() + geom_point()
print(p_lines)

p_lines_fancy <- p + theme_bw() + scale_y_continuous(labels = function(x){ paste0(100 * x, "%") }) + ylab("Percentage of verbs with regular inflection")
print(p_lines_fancy)

# 4. Discuss whether verb regularity is modulated by its frequency, the auxiliary it uses, and whether the two predictors interact. 
# (Based on the graphs only.)

# It appears that both factors play a role and that they interact. 
# (A) Verbs with 'hebben' (i) have a stronger tendency towards regularity than than those with 'zijnheb'. These, in turn, (ii) have a stronger tendency towards
# regularity than those with 'zijn'. (B) Fewer high-frequency verbs are regular than low-frequency verbs.
# (C) The differences due to auxiliary are more pronounced in high-frequency verbs than in low-frequency verbs. 



# 5a. Fit a generalized linear model using brms to determine whether verb regularity depends on the above predictors, and whether 
# the predictors interact. Use the categorical predictor for frequency in this model. Give the contrast coding some thought!

etymology_essential$WrittenFrequencyCat %<>% as.factor
contrasts(etymology_essential$WrittenFrequencyCat)
contrasts(etymology_essential$WrittenFrequencyCat) <- -1 * contr.sum(2)
contrasts(etymology_essential$WrittenFrequencyCat)

contrasts(etymology_essential$Auxiliary)
contrasts(etymology_essential$Auxiliary) <- contr.sum(3)
contrasts(etymology_essential$Auxiliary)

# make everything reproducible
set.seed(1234)

m1 <- brm(isRegular ~ 1 + WrittenFrequencyCat + Auxiliary + WrittenFrequencyCat:Auxiliary, 
          data = etymology_essential, family = bernoulli(link = logit), chains = 4, cores = 4)
summary(m1)
# This model specification fails to converge with an error message that might sound like gibberish to you.
# In most instances, it is safe to assume that if the model produces errors or warnings, or generally behaves weirdly
# after triple-checking it's specification, it's probably not the right model for the present data.

# Technically, the problem seems to be that the surface of the posterior distribution is very flat (likely due to relatively little data). 
# As a result, the sampling algorithm can't find its way to the maximum posteriors probability region. You can see that in that the trace plots on the
# right hand side do *not* look like fat caterpillars, but rather like (nearly-constant) lines. This means that in each chain, the algorithm stays stuck 
# near its starting values. 
plot(m1)

# ... TODO interactions.

# Most problems go away if you use (even relatively uninformative) priors, to give the sampler a push in the right direction. In our case, 
# the model has simply too little data to estimate the interaction effects.
#
# The fact that we end up with obscenely large (or small) coefficients suggests that because the the posterior distribition is almost flat
# for some coefficients (because of little data), this flatness may prevent the sampler from finding the way to where the most likely parameter 
# estimates are. (This especially affects the estimation of the interaction, because there are very few data points in both, zijn-low-freq and 
# zijn-high-freq.)

# Let's see which predictors cause the problem: We'll set relatively tight priors and see what happens.
m1A <- brm(isRegular ~ 1 + WrittenFrequencyCat + Auxiliary + WrittenFrequencyCat:Auxiliary,
          data = etymology_essential, family = bernoulli(link = logit), chains = 4, cores = 4,
          prior = c( set_prior("normal(0, 2)") ) )
summary(m1A)
plot(m1A)
# Suddenly, the estimates look more or less OK, Rhat is around 1, and the trace plots look fine as well.

# The intercept is roughly the right order of magnitude.
etymology_essential %>% group_by(WrittenFrequencyCat, Auxiliary) %>% summarize(percRegular = mean(isRegular)) %>% .$percRegular %>% mean()
rethinking::logit(.37)

# I suspect that the interaction terms are the culprits. The model should still be fine if we set informative priors for the interaction terms only.
# Let's do it:
m1B <- brm(isRegular ~ 1 + WrittenFrequencyCat + Auxiliary + WrittenFrequencyCat:Auxiliary,
           data = etymology_essential, family = bernoulli(link = logit), chains = 4, cores = 4,
           prior = c( set_prior("normal(0, 2)", coef = "WrittenFrequencyCat1:Auxiliary1"),
                      set_prior("normal(0, 2)", coef = "WrittenFrequencyCat1:Auxiliary2") ) )
summary(m1B)
plot(m1B)
# The model and trace plots look fine, don't they?



# 5b. Fit a second model, this time using frequency as a continuous predictor instead.

# use centered and scaled frequency
etymology_essential$scWrittenFrequency <- scale(etymology_essential$WrittenFrequency)

m2 <- brm(isRegular ~ 1 + scWrittenFrequency + Auxiliary + scWrittenFrequency:Auxiliary, 
               data = etymology_essential, family = bernoulli(link = logit), chains = 4, cores = 4)
summary(m2)



# 6. Discuss the conclusions based on *each* estimate (and HPDI) in the above models.

m1_hpdis <- coda::HPDinterval(as.mcmc(m1B, combine_chains = TRUE), prob = 0.95)
m2_hpdis <- coda::HPDinterval(as.mcmc(m2, combine_chains = TRUE), prob = 0.95)

m1_hpdis %>% round(2)
m2_hpdis %>% round(2)

m1_posterior_samples <- brms::posterior_samples(m1)
m2_posterior_samples <- brms::posterior_samples(m2)
# plot(m2_posterior_samples)

# Both models suggest that (unweighted) average proportion of regular verbs is *roughly* between .15 and .46 (95% CrIs around -1.69 to -0.13 logit-units).
# Both models demonstrate a *main effect* of frequency (that is, a "marginal effect" of frequency), such that verbs tend to be regular less often with 
# increasing frequency. (The 95% HPDI for scWrittenFrequency is largely below 0. In spite of the fact that the HPDI contains 0, however, the posterior 
# probability of scWrittenFrequency having a negative slope is ~94%. This means that it is 17 times more likely that the slope is negative rather than 
# positive. ...
mean(m2_posterior_samples$b_scWrittenFrequency < 0) # (compute posterior probability of b_scWrittenFrequency < 0)
hypothesis(m2, "scWrittenFrequency < 0") # (compute evidence ratio for b_scWrittenFrequency > 0: Evidence ratio = P(b_scWrittenFrequency < 0) / P(b_scWrittenFrequency > 0)  )

# ... The 95% HPDI for WrittenFrequencyCat is entriely above 0, but note that WrittenFrequencyCat is coded with 1 for low frequency, which means that
# the interpretation of the slope is reversed.)  
contrasts(etymology_essential$WrittenFrequencyCat)

# Both models demonstrate main effects of auxiliary on regularity: The HPDIs for the slopes for for both auxiliary contrasts exclude 0. 
# b_Auxiliary1 is positive, while b_Auxiliary2 is negative. Looking at the contrast matrix below reveals that 'Auxiliary1' represents the difference 
# between the average of all verb types and the verbs with 'hebben', while 'Auxiliary2' stands for the difference between verbs with 'zijn' and 
# the average of all verb types. This means that verbs with 'hebben' tend to be more regular than the average, while verbs with 'zijn' tend to be 
# regular less often.
contrasts(etymology_essential$Auxiliary)

# For interactions, see below.

#7. Do you see any noteworthy differences between the two models? Why?
- Effect of frequency is clear for discrete predictor, but not for the continuous predictor.

# One noteworthy difference between the two models is in the effect of frequency. While model 1 shows a clear effect of frequency on regularity 
# (HPDI excludes 0, and poster probability of the slope being above 0 is almost 1), model 2 is not quite as clear-cut: the HPDI includes 0, and
# the posterior probability of the slope being smaller than 0 is 'only' 94.5%.
mean(m1_posterior_samples$b_WrittenFrequencyCat1 > 0)
mean(m2_posterior_samples$b_scWrittenFrequency < 0)

# The reason for this difference is that the two models answer slightly different questions. While model 1 tells us whether there is an effect of
# *frequency class* (high or low) on regularity, model 2 tells us whether there is a continuous effect of (scaled and centered) frequency on regularity.
# Closer inspection of the data suggests that, actually, both models are sort of right:

etymology_essential$WrittenFrequencyFineGrain <- Hmisc::cut2(etymology_essential$WrittenFrequency, g = 10) 
etymology_essential %>% group_by(WrittenFrequencyFineGrain) %>% 
                        dplyr::summarize(median_WrittenFrequency = median(WrittenFrequency), 
                                         perc_regular = mean(isRegular)) %>%
ggplot(aes(median_WrittenFrequency, perc_regular)) + geom_point()

# As you've seen in the very first plots we created, there seems to be an effect of frequency class. The above plot demonstrates, however,
# shows that the this effect is caused almost entirely by the verbs with the highest frequency. 
# So, yes, there is an effect of frequency class in that the upper 50% of the verbs are on average less regular than the lower 50%. However,
# there doesn't seem to be a continuous effect of frequency on regularity. Moreover, if we remove the highest-frequency verb group, the effect
# of frequency on regularity might even be described as positive, not negative.
# Thus, the presence of information about the actual frequency affects the model coefficients in a non-trivial way.



# The models also differ what they say about the interactions. According to model 1, the posterior probability of a positive slope for 
# WrittenFrequencyCat:Auxiliary, a negative slope for WrittenFrequencyCat:Auxiliary is high (cf. HPDIs, or compute the posterior probabilities or 
# evidence ratios). This means that model 1 says that the effect of WrittenFrequencyCat is larger for 'hebben' verbs than for the average. But because 
# the main effect of WrittenFrequencyCat is negative (-2.36; -0.22), and the interaction slope is positive (0.08; 2.24), this means that the magnitude 
# of the (negative) effect is much smaller in 'hebben' verbs. Model 1 also says that the magnitude of the effect of WrittenFrequencyCat is even stronger
# in 'zijn' verbs than in the average verb type. (In this case, the magnitude is larger because the main effect of WrittenFrequencyCat as well as the 
# interaction slope are both negative.)
# Model 2, on the other hand shows rather wide HPDIs for the interaction slopes, which are nearly symmetrical around 0. The posterior probabilities for 
# either being positive or negative are quite close to 50%:
mean(m2_posterior_samples$`b_scWrittenFrequency:Auxiliary1` < 0)
mean(m2_posterior_samples$`b_scWrittenFrequency:Auxiliary2` < 0)

# In part, this difference must be related to the fact that the main effect of frequency differs between models. (Remember, the interaction describes how
# the effect of one variable changes when a second variable changes?) I suspect that the remainder of the discrepancy between models may be due to similar
# reasons as the difference in main effects, but that's a bit of an open question.

