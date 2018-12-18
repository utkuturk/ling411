######Assignment3 Utku Turk#########

####4E1####

# the first line is the likelihood. the other two lines are priors for other two parameters.

####4E2####

# so, basically posterior is likelihood * prior, and there are two priors.

# as the first line is likelihood and define the relation between 2 parameters, the second and the thirtd lines are the parameters in the posterior.

####4E4####

# first line is likelihood, the second line is mu predicted, thus our linear model.

####4E5####

# it seems like a 4 but mu predicted is just a number we have with the equations,

# so, it is not a parameter anymore, that's why 3; alpha, beta, and sigma.

####Programming Assignment####

#Load the 'PearsonLee' from the 'HistData' packages.

library( HistData )
data( PearsonLee )

#Look up information on the data set, and the meaning of the columns in the data frame.

?PearsonLee
View( PearsonLee )

#Create new columns representing parents and children heights in centimeters. 

?cm

PearsonLee$childCm <- cm( PearsonLee$child )
PearsonLee$parentCm <- cm( PearsonLee$parent )

#Define a linear model which predicts the child's height as a function of the parents' height. Use reasonable uniform priors.

library( rethinking )

model1 <-   alist(
    childCm ~ dnorm( mu , sigma ) ,
    mu ~ a + b * parentCm ,
    a ~ dnorm( 133 , 200 ) ,
    b ~ dnorm( -30 , 30 ) ,
    sigma ~ dunif( 0 , 50 )
  )

#Run MAP on the model.

m1.0 <- rethinking::map( model1 , data=PearsonLee )


#Explain the meaning of the estimated coefficients (intercept and slope), and the significance of the 89% credible intervals.
summary( m1.0 )
### our confidence interval of intercept is not that narrow. 20 cm is pretty high, or is it? I do not really know.
nrow( PearsonLee )
### but I guess for 746 entry, it is alright.
### for our beta, it means for every 1 cm of parents height, we should expect 0.39 cm increase on average.
### and our confidence interval is also pretty narrow for beta.

####My Questions#####
# I choose dnorm for beta, but I do not know why I didnt use dunif. 
# I also do not know why I chose -30 and 30 for beta.
# Regarding 4E1, isn't it every likelihood is some sort of prior? when does likelihood become a posterior, and then a prior?
