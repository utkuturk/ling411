###let' go through the grid approximation example again
# N = 9 , k =6
Size = 9
Head = 6

#define grid
p_grid <- seq( from = 0 , to = 1 , length.out = 10000)


#set up prior
prior <- rep( 1 , times = length( p_grid ))

#likelihood
likelihood <- sapply(p_grid, function(p) dbinom( Head ,  Size , prob = p))

# unstandardized posterior
unstPosterior <- likelihood * prior
sum(unstPosterior)
#standardize it
posterior <- unstPosterior / sum(unstPosterior)

#plot it
plot(p_grid, posterior)

#sample
##sample sample
set.seed(12345) #defining seed, nothing changes
sample(1:10 , size=10 , replace = TRUE , prob = rep(.1,10))

#our sample
posteriorSample <- sample(p_grid , size = 1e3 , replace = TRUE , prob = posterior)
c(
mean(posteriorSample > .55),
mean(posteriorSample > .2),
mean(posteriorSample > .3)
)

#summarizing
plot(posteriorSample) #this one is just dumb. 
#what should meter is when you sort them
plot(sort(posteriorSample))

quantile(posteriorSample, c(.1,.9))

library(ggplot2)
ggplot(data.frame(x=posteriorSample), aes(x)) + geom_density() + theme_bw() +geom_ribbon(aes(x=x))