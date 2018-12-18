
myData = list( nHeads = c( 1 , 7 ) , nTossess = c( 9 , 9 ) )

library( magrittr )
library( rethinking )


mdcm1 <- alist(
  nHeads ~ dbinom( size = nTossess , prob = c( p1 , p2 )) ,
  p1 ~ dunif( 0 , 1 ) ,
  p2 ~ dunif( 0 , 1 )
)

m1 <- map( mdcm1 , data = myData)
precis( m1 , corr = T)

###########
# model 2, we need more detail
mdcm2 <- alist(
  nHeads ~ dbinom( size = nTossess , prob = c( p1 , p2 )) ,
  logit(p1) ~ lop1 , 
  logit(p2) ~ lop2 ,
  lop1 ~ dunif( -100 , 100) ,#p1 ~ dunif( 0 , 1 ) ,
  lop2 ~ dunif( -100 , 100) #p2 ~ dunif( 0 , 1 )
)

m2 <- map(mdcm2 , data = myData)
precis(m2 , corr = T)

inv_logit(c( -3.77 , -2.08 , -.38))


m2Samples <- extract.samples(m2)
smoothScatter(m2Samples)
plot(m2Samples)
rethinking::HPDI(m2Samples$lop1)
rethinking::HPDI(m2Samples$lop2)


rethinking::HPDI(inv_logit(m2Samples$lop1))
rethinking::HPDI(inv_logit(m2Samples$lop2))

# but we need more proof if they are different or biased.
# that s why we need linear model


# before model 3 we need to look at priors
# why do we use uniform? it is meaningless, check the 7B
# when we do not know what to do, we use normal distribution

samplesNormalLOPrior <- dnorm(10^5 , m = 0 , sd = 1)
hist(samplesNormalLOPrior , xlim = c(-10 , 10))

# BETTER:

plot(function(x) dnorm(logit(x) , m = 0 , sd = 1) , xlim = c(0 , 1)) #why it is logit, and not inv_logit

# model 3 is model 2 with this prior

mdcm3 <- alist(
  nHeads ~ dbinom( size = nTossess , prob = c( p1 , p2 )) ,
  logit(p1) ~ lop1 , 
  logit(p2) ~ lop2 ,
  lop1 ~ dnorm(0 , 5) , #lop1 ~ dunif( -100 , 100) ,#p1 ~ dunif( 0 , 1 ) ,
  lop2 ~ dnorm(0 , 5) #lop2 ~ dunif( -100 , 100) #p2 ~ dunif( 0 , 1 )
)

m3 <- map(mdcm3 , data = myData)
precis(m3 , corr = T)
m3Samples <- extract.samples(m3)
smoothScatter(m3Samples)
plot(m3Samples)
rethinking::HPDI(m3Samples$lop1)
rethinking::HPDI(m3Samples$lop2)


rethinking::HPDI(inv_logit(m3Samples$lop1))
rethinking::HPDI(inv_logit(m3Samples$lop2))

# model 4

myDataRevized = list( nHeads = c( 1 , 7 ) , nTossess = c( 9 , 9 ) , coinID = c(0 , 1))
mdcm4 <- alist(
  nHeads ~ dbinom(nTossess , pPredicted) , 
  logit(pPredicted) ~ loP + diff * coinID , 
  loP ~ dnorm( 0 , 20) ,
  diff ~ dnorm( 0 , 20)
)

m4 <- map(mdcm4 , data = myDataRevized)

precis(m4 , corr = T)
m4Samples <- extract.samples(m4)

head(m4Samples)
mean(m4Samples$diff <= 0)

ggplot(m4Samples , aes(diff)) + geom_density()





###########LOG ODDS#######
# transform to log odds
logit(c(.1 , .5 , .9))
#transform from log odds
inv_logit(c( -100 , -2.2 , 0 , 2.2 , 100))


