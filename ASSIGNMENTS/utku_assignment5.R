
library(dplyr)
library(magrittr)
library(rethinking)
library(languageR)

data(dative)

dative$RealizationRec_isNP <- ifelse(dative$RealizationOfRecipient=="NP", 1, 0)
dative %<>% mutate(cAnimacyRec_animate = ifelse(AnimacyOfRec=="animate", 1, 0), 
                   cAnimacyTheme_animate = ifelse(AnimacyOfTheme=="animate", 1, 0) )
dative_essential <- dative %>% dplyr::select(RealizationRec_isNP, cAnimacyRec_animate, cAnimacyTheme_animate)
View(dative_essential)


# TODO: 
# Now, create a data.frame called 'dative_aggregated' with four rows and four columns, 
# which shows us for each combination of cAnimacyRec_animate and cAnimacyTheme_animate: 
dative_aggregated <- dative_essential %>% group_by(cAnimacyRec_animate, cAnimacyTheme_animate)  %>% 
  summarize( mean = mean(RealizationRec_isNP) , 
             N_Total = length(RealizationRec_isNP)) #RealizationRec_isNP = sum(RealizationRec_isNP))

# (i) the proportion of NP realizations
# (ii) the sample size (i.e., the number of cases)
# Keep these numbers in mind, because that's what you're modelling. If the model estimates do not match these numbers at least approximately, your model is wrong. 



model_alice_definition_incorrect <- alist(
  RealizationRec_isNP ~ dbinom (nrow(dative_essential), p_NP), # INCORRECT
  logit(p_NP) <- a + b * cAnimacyTheme_animate + c * cAnimacyRec_animate, # this is fine
  a ~ dnorm(0, 1), # this is fine
  b ~ dnorm(0, 1), # this is fine
  c ~ dnorm(0, 1) # this is fine
)

model_alice_incorrect <- map(model_alice_definition_incorrect, data = dative_essential)
precis(model_alice_incorrect)

# Surprisingly, the intercept (a) is very, very low. -8.73 corresponds to a probability of almost 0:
model_alice_incorrect@coef['a']
inv_logit(model_alice_incorrect@coef['a'])

# Remember, in a model with uncentered predictors, the intercept corresponds to the estimate of the dependent variable for the cases where the predictors are zero.
# Let's compute that percentage. It should be approximately equal to the intercept. (*Approximately*, because the intercept will be affected by its prior, while the empirical mean below is not.)
dative_essential_all_inanimate <- dative_essential %>% subset(cAnimacyRec_animate == 0 & cAnimacyTheme_animate == 0)
mean(dative_essential_all_inanimate$RealizationRec_isNP)
# Woooow!!! How did this happen???
# Our model tells us that the intercept is almost zero, but the data tell us that its almost 50%. What is going on?


# Now let's sample from the fitted model.
samples_alice_incorrect <- extract.samples(model_alice_incorrect)

# Let's compare the slope HPDIs on the logit scale and on the probability scale.
rethinking::HPDI(samples_alice_incorrect$c)
rethinking::HPDI(inv_logit(samples_alice_incorrect$c)) # INCORRECT #
# If we transform it this way, it looks like the slope for the animacy of the recipient corresponds to a change of 55 to 63 percentage points.
# But that's not correct. 
 
# TODO:
# Add five columns to the 'samples_alice_incorrect' data.frame: 
View(samples_alice_incorrect)
# (i) One column that corresponds to the logit of the probability of NP realizations when cAnimacyRec_animate == 0 (and cAnimacyTheme_animate == 0)
samples_alice_incorrect$logNpR0 <- samples_alice_incorrect$a

# (ii) Another column that corresponds to the logit of the probability of NP realizations when cAnimacyRec_animate == 1 (and cAnimacyTheme_animate == 0)
samples_alice_incorrect$logNpR1 <- samples_alice_incorrect$a+samples_alice_incorrect$c

# (iii) & (iv) Columns containing the probabilities corresponding to the logit values in (i) and (ii).
samples_alice_incorrect$PrNpR0 <- inv_logit(samples_alice_incorrect$logNpR0)
samples_alice_incorrect$PrNpR1 <- inv_logit(samples_alice_incorrect$logNpR1)
# (v) A column that corresponds to the difference between (iii) and (iv).
samples_alice_incorrect$diff <- samples_alice_incorrect$PrNpR0 - samples_alice_incorrect$PrNpR1
# Use the samples data.frame to create a scatterplot with the slope c (on the x-axis) and column (v) on the y-axis.
plot(samples_alice_incorrect$c, samples_alice_incorrect$diff)


# If there was a one-to-one correspondence between them, we should see a perfect straight line. We don't. Why not? Please explain.
plot(samples_alice_incorrect$c)
####EXPLANATION, It is different because the big chunk of representations in the logarithmic frame corresponds to the very narrow space in the proportions. 
# even though in log we have the equally distributed scenario. As we go up, it diverts from the straight line. 


rethinking::HPDI(samples_alice_incorrect$c)
rethinking::HPDI(inv_logit(samples_alice_incorrect$c)) # INCORRECT

# TODO: 
# Perform the above procedure for slope b now. The discrepancy is even bigger here. The values obtained by applying inv_logit() even differ in sign from
# our samples of b. Why? Please explain.
samples_alice_incorrect$logNpR2 <- samples_alice_incorrect$a+samples_alice_incorrect$b
samples_alice_incorrect$PrNpR2 <- inv_logit(samples_alice_incorrect$logNpR2)
samples_alice_incorrect$diff2 <- samples_alice_incorrect$PrNpR1 - samples_alice_incorrect$PrNpR2
plot(samples_alice_incorrect$b, samples_alice_incorrect$diff2)


plot(samples_alice_incorrect$b)


#####EXPLANATION: It is because right now we have a both bigger log interval and probability interval, which means the fluctuations,
# and the mass falling from the log-world to probabilt-world is even more unbalanced.



# TODO: Given all you've seen above about why the first model's specification is incorrect, please specify a correct likelihood function to be used when the model is applied to the 'dative_essential' data.frame.
#       IMPORTANT: In the lines up to the precis() call, do not change anything except the likelihood function line.
#       Please verify that your estimates approximately match the data.
model_alice_definition_correct1 <- alist(
  RealizationRec_isNP ~ dbinom(1, p_predicted) ,
  logit(p_predicted) <- a + b * cAnimacyTheme_animate + c * cAnimacyRec_animate,
  a ~ dnorm(0, 1),
  b ~ dnorm(0, 1),
  c ~ dnorm(0, 1)
)
model_alice_correct1 <- map(model_alice_definition_correct1, data = dative_essential)
precis(model_alice_correct1)
model_alice_correct1@coef['a']
inv_logit(model_alice_correct1@coef['a'])
mean(dative_essential_all_inanimate$RealizationRec_isNP)

# they are really close.

# TODO: There is an alternative way of specifying a likelihood function, if we run the model on aggregate data. Please specify such a likelihood function below.
#       IMPORTANT: In the lines up to the precis() call, do not change anything except the likelihood function line.
#       Please verify that your estimates approximately match the data.
model_alice_definition_correct2 <- alist(
  NPs ~ dbinom(N_Total, p_predicted),
  logit(p_predicted) <- a + b * cAnimacyTheme_animate + c * cAnimacyRec_animate,
  a ~ dnorm(0, 1),
  b ~ dnorm(0, 1),
  c ~ dnorm(0, 1)
)
model_alice_correct2 <- map(model_alice_definition_correct2, data = as.list(dative_aggregated))
precis(model_alice_correct2)


# TODO:
# Verify that the estimates of both your models match each other, and that they both match the data.
# If they don't, the above code doesn't do what it is supposed to do.

model_alice_correct1@coef['a']
inv_logit(model_alice_correct1@coef['a'])
model_alice_correct2@coef['a']
inv_logit(model_alice_correct2@coef['a'])
mean(dative_essential_all_inanimate$RealizationRec_isNP)



# TODO: Compute HPDIs on the slopes of either of the correctly specified models and discuss (again) *how* animacy affects the realization of the dative argument. 
samples_alice_correct1 <- extract.samples(model_alice_correct1)
samples_alice_correct2 <- extract.samples(model_alice_correct2)

rethinking::HPDI(samples_alice_correct1$b)
rethinking::HPDI(samples_alice_correct1$c)
rethinking::HPDI(samples_alice_correct2$b)
rethinking::HPDI(samples_alice_correct2$c)
#### EXPLANATION: According to the HPDI analyses of the slope b, we can confidently say that with animate Themes, 
#                 the probability of having a PP Recipient of the dative structure is strinkingly high.
#                 As for the slope c, it strongly indicates that with Animate Recipients, 
#                 the probability of having a NP Recipient in a dative structure is definitely positive, but not as high as effect of the Animacy of Theme.

### PROBABILITY SPACE ESTIMATES
# how many objects do you have?
ls()
# attach the samples, i m working on this right now, dont make me write it everytime.
attach(samples_alice_correct2)

# detach
detach(samples_alice_correct2)


# to briefly attach and detach again right after the computation: with function

with(samples_alice_correct2 , HPDI(b))

with( samples_alice_correct2 , HPDI( inv_logit( a + b ) - inv_logit( a ) ) )
with( samples_alice_correct2 , HPDI( inv_logit( a + c ) - inv_logit( a ) ) )
# you can write cheeky percentage stuff in there.
