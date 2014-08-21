#' Simulation of Let's Make a Deal game
#' 
#' This function simulates and computes long-run averages of success
#' by strategy for Monty Hall problem.
#' 
#' @param games  integer number of games in simulation
#' @param doors  integer number of doors in each game
#' @param strategy  character argument: 'switch', 'random', 'stay'
#' @param set.seed  integer number for pseudorandom number generator
#' @param plot  logical to plot cumulative avg plot, default FALSE
#' @param output logical return sim data to var or print to console
#' @return list 1. mean: numeric success probability 
#' 2. cumu.avg: vector of cumulative avg success probability
#' 3. data.frame: df of simulation values
#' @author Stephen Downing
#' @details
#' This function runs a simulation for on of three strategies in the 
#' Monty Hall problem and returns a list of the relevant simulation results.
#' Plotting the cumulative average success probability is an optional logical
#' argument, default FALSE.
#' @export

MontyHallStrat <- function(games,             #number of games
                           doors=3,           #number of doors
                           strategy="switch", # {switch, random, stay}
                           set.seed=1,        #for reproducibility
                           plot=F,            #logical to plot cumu avg
                           output=F           #logical return data
                           ) {
  set.seed(set.seed)
  # contestant chooses door:
  x1 <- sample(x = 1:doors, size = games, replace = T)
  # car behind door:
  x2 <- sample(x = 1:doors, size = games, replace = T)
  x3 <- rep(NA,games)  # vec to record door contestant switches to
  x4 <- rep(NA,games)  # vec to record if contestant wins
  r <- rep(NA,games)   # vec to record door removed (opened) by Monty Hall
  x <- rbind(x1,x2,x3,x4,r)
  d <- 1:doors  #door list
  
  for (i in 1:dim(x)[2]) {
    if (doors==3) {
      if (x[1,i] == x[2,i]) { 
        #if contestant chose correctly then Monty chooses between other two doors
        #with equal probability to open one
        x["r",i] <- sample(x = d[-x[2,i]],size = 1) 
      } else {
        #if chose incorrectly then Monty has to open door without car
        x["r",i] <- d[-c(x[1,i],x[2,i])]
      } 
    } else {
      #generalize to >3 doors
      if (x[1,i] == x[2,i]) { 
        #if contestant chose correctly then Monty chooses between other doors
        #with equal probability to open one
        x["r",i] <- sample(x = d[-x[2,i]],size = 1) 
      } else {
        #if chose incorrectly then Monty has to open a door without car
        x["r",i] <- sample(x = d[-c(x[1,i],x[2,i])],
                           size=1,
                           prob=rep(1/(doors-2),doors-2) )
      } 
    }
    
    if(strategy == "switch") {
      #1. switch strategy 
      if(doors==3) {
        x[3,i] <- d[-c(x["r",i],x[1,i])]
      } else {
        #generalize to >3 doors
        x[3,i] <- sample(x = d[-c(x["r",i],x[1,i])],size = 1)
      }
      
    }else if (strategy == "random") {
      #2. random strategy to switch or not
      x[3,i] <- sample(x = d[-c(x["r",i])], size = 1)
    } else {
      #3. always stay
      x[3,i] <- x[1,i]
    }
  }
  
  #assign binary response (1=win; 0=lose)
  x[4,which(x[2,]==x[3,])] <- 1  #if car door equals switched-to door
  x[4,which(x[2,]!=x[3,])] <- 0  #if car door not equal switched-to door
  
  m <- mean(x[4,])
  
  # cumulative average over games
  vec <- c()
  for (i in 1:dim(x)[2]){
    vec[i] <- mean(x[4,1:i])
  }
  
  if (plot) {
    par(xpd=F) # keep abline inside plot frame
    par(mfrow=c(1,1))
    plot(vec,type='l',main=paste("Mean After",games,"Games :",m),ylab="probability",lwd=2,ylim=c(0,1));abline(h= 2/3, col='red')
  }
  
  if (output) {
    return(list(mean=m,cumu.avg=vec,data.frame=x))
  }
} #end function
