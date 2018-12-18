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