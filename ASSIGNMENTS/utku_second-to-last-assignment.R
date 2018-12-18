library(magrittr)
library(languageR)
library(dplyr)
library(brms)
library(ggplot2)
library(rethinking)
library(shinystan)
data("etymology")


# Create a new column called WrittenFrequencyCat ('cat' for categorical), which contains the value "high" 
# when WrittenFrequency is high (above the median), and the value "low" when the frequency is below the median.default

writtenFrequencyMedian <- median( etymology$WrittenFrequency )

etymology$WrittenFrequencyCAT <- ifelse( etymology$WrittenFrequency >= writtenFrequencyMedian , "high" ,  "low" ) 

etyEssential <- etymology %>% dplyr::select( Verb, 
                                             Auxiliary , 
                                             Regularity, 
                                             WrittenFrequency, 
                                             WrittenFrequencyCAT )

etyEssential$Regular <- ifelse( etyEssential$Regularity == "regular" , 1 ,  0 ) 
# etyEssential$aux.id <- ifelse( etyEssential$aux == "hebben" , 0 , ifelse( etyEssential$aux == "zijn", 1 , 2))


ety <- etyEssential %>% group_by( WrittenFrequencyCAT , Auxiliary ) %>% 
  summarize( RegularAv = mean(Regular))

View(ety)
# Create a barplot of the average regularity of verbs (column 'Regular') as a function of WrittenFrequencyCat 
# and Auxiliary. (Using several facets, faceting by frequency: "+ facet_wrap(~WrittenFrequencyCat)")


ggplot(ety, aes(Auxiliary, RegularAv)) + geom_bar(stat="identity") + facet_wrap(~WrittenFrequencyCAT)

# Create a *single* *line plot* of the average regularity of verbs as a function of WrittenFrequencyCat and Auxiliary. 
# (Yes, all in one single plot. Put 'Auxiliary' on the x-axis.)


ggplot(ety, aes( x = Auxiliary , y = RegularAv , group = WrittenFrequencyCAT , color = WrittenFrequencyCAT)) +
  geom_line() + geom_point()

# Discuss whether verb regularity is modulated by its frequency, 
# the auxiliary it uses, and whether the two predictors interact. (Based on the graphs only.)

### I do not think frequency plays a significant role in auxiliary hebben. However, there are small changes in zijn and zijnheb.
### zijn only possess verb regularity when the very is not so frequent, and zijn have more regularity in the same conditions.
### I think the auxiliary types plays a much bigger role. Whether the verb is frequent or not hebben is  most of the time, almost 60%, is regular.
### Whereas the other have much fewer regularity.
### I do think there is an interaction in these two condition, yet I do not think it is really that significant.


# Fit a generalized linear model using brms to determine 
# whether verb regularity depends on the above predictors, and whether the predictors interact.
# Use the categorical predictor for frequency in this model. Give the contrast coding some thought!

contrasts(etyEssential$Auxiliary) <- contr.sum(3)
contrasts(etyEssential$WrittenFrequencyCAT) <- contr.sum(3)

m1 <- brm(formula = Regular ~ 1 + Auxiliary * WrittenFrequencyCAT ,
          data = etyEssential , 
          family = bernoulli("logit") , 
          chains = 4 , core = 4)
m1gaus <- brm(formula = Regular ~ 1 + Auxiliary * WrittenFrequencyCAT ,
          data = etyEssential , 
          family = gaussian())


# it was problematic with bernoulli, did it with gaus.
summary(m1gaus)
summary(m1)

# Fit a second model, this time using frequency as a continuous predictor instead.

m2 <- brm(formula = Regular ~ 1 + Auxiliary * WrittenFrequency , 
          data = etyEssential , 
          family = bernoulli("logit") ,
          chains = 4 , core = 4)

summary(m2)

# Discuss the conclusions based on *each* estimate (and HPDI) in the above models.
summary(m1)
summary(m1gaus)
summary(m2)
shinystan::launch_shinystan(m1)
shinystan::launch_shinystan(m1gaus1)
shinystan::launch_shinystan(m2)
samples1 <- rstan::extract(m1$fit)
samples1gaus <- rstan::extract(m1gaus$fit)
samples2 <- rstan::extract(m2$fit)

mean(samples1$b < 0)
mean(samples1gaus$b < 0)
mean(samples2$b < 0)

### first of all, i really do not know anything about how to get HPDI's of brms models. 
### i tried to do every little thing we did in the class, including shinystan and rstan extract.
### I really do not think there is a way to interpret model m1's estimates, like -300 etc.
### as for the m2, estimate of the intercept is kind of meaningless as it is only there when the frequency is 0.
### I really need help to get around What does these intercepts and other elements in summary means. 
### I have got lost in the process of writing code and learning what to write in code.
### but my best guess is, even though there are some log numbers in estimate, which seem important in the first glance,
### we cannot say anything about the data because the interval is so wide 

# Do you see any noteworthy differences between the two models? Why?

# Yes, I do. First one does not seem to work, becacuse it is categorical. There is too much standard deviation inside the categories. 
# We cannot account for all the data accumulated in two separate group. Yet, when it is continous it is much more easy to account for the data. 
