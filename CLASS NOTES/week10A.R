

num_weeks <- 1e3

positions <- rep(NA,num_weeks)

current <- 10

for ( i in 1:num_weeks ) {
  positions[ i ] <- current # record current position
  proposal <- current + sample( c( -1 , 1 ) , size= 1 ) # flip coin to generate proposal
  if ( proposal < 1 ) proposal <- 10 # now make sure he loops around the archipelago
  if ( proposal > 10 ) proposal <- 1
  prob_move <- proposal/current   # move?
  current <- ifelse( runif( 1 ) < prob_move , proposal , current )
}

plot(positions , type = "l")
hist(positions)

### FRIDAY CLASS

d <- read.csv("language.csv")

