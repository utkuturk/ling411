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

