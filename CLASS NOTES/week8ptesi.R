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

