library(LaplacesDemon)
library(tidyverse)
library(plyr)

#### Data ####
n.rem <- 0 #number of removal locations
n.obs <- NA
n.sims <- 10
eps <- 0.6 #eradication probability 
p <- 0.6 #detection probability
n.sites <- 50 #number of sites
n.months <- 12
n.year <- 10 #time steps

##### Initial Removal Sites #####

site.rem <- array(0, c(n.rem,n.year,n.sims))

#vector of locations where removal occurs this year 1 for removal, 0 for no removal
rem.vec <- array(0, c(n.sites, n.year, n.sims))

#transition probabilities
State <- array(NA, c(n.sites, n.months, n.year, n.sims))

##### Initial True State #####
#Initial state/observation
#start with random states?
#State[1:n.sites,1,1,] <- sample(c(1,2), n.sites, replace = T) 
#Here I am starting with first 5 upstream segments are invaded
State[1:5,1,1,] <- 2
State[6:n.sites,1,1,] <- 1

ps<- array(NA, c(2,n.sites,n.months, 2)) #state transition array

gamma <- array(NA, c(n.sites,n.months,n.year,n.sims)) #invasion
gamma0 <- -2 #intrinsic invasion
gamma1 <- 0.5 #effect of neighboring locations on invasion probability
D <- array(NA, c(n.sites,n.months, n.year,n.sims)) #neighbor states

year <- 1

for(s in 1:n.sims){
  for(i in 2:(n.sites-1)){
    D[i,1,year,s] <- State[i+1,1,year,s] + State[i-1,1,year,s]
    
    gamma[i,1,year,s] <- invlogit(gamma0 + gamma1*D[i,1,year,s]) #invasion probability
    
    ps[1,i,1,1] <- 1-gamma[i,1,year,s]
    ps[1,i,1,2] <- gamma[i,1,year,s]
    ps[2,i,1,1] <- (eps*rem.vec[i,year,s])
    ps[2,i,1,2] <- 1- (eps*rem.vec[i,year,s]) 
    
  }
  
  D[1,1,year,s] <- State[2,1,year,s]
  D[n.sites,1,year,s] <- State[(n.sites-1),1,year,s]
  
  gamma[1,1,year,s] <- invlogit(gamma0 + gamma1*D[1,1,year,s])
  ps[1,1,1,1] <- 1-gamma[1,1,year,s]
  ps[1,1,1,2] <- gamma[1,1,year,s]
  ps[2,1,1,1] <- (eps*rem.vec[1,year,s])
  ps[2,1,1,2] <- 1- (eps*rem.vec[i,year,s]) 
  
  gamma[n.sites,1,year,s] <- invlogit(gamma0 + gamma1*D[n.sites,1,year,s])
  ps[1,n.sites,1,1] <- 1-gamma[n.sites,1,year,s]
  ps[1,n.sites,1,2] <- gamma[n.sites,1,year,s]
  ps[2,n.sites,1,1] <- (eps*rem.vec[n.sites,year,s])
  ps[2,n.sites,1,2] <- 1- (eps*rem.vec[n.sites,year,s]) 
  
}

#### Run ####

years <- n.year

for(year in 1:years){
  
  #### 1. Simulate truth #####
  ##### 1b. Year 1+, month 1 ######
  if(year > 1){
    for(s in 1:n.sims){
      for (i in 2:(n.sites-1)){
        # State process: 
        State[i,1,year,s] <- rcat(1,ps[State[i,n.months,year-1,s], i, n.months, ])
        State[1,1,year,s] <- rcat(1,ps[State[1,n.months,year-1,s], 1, n.months, ])
        State[n.sites,1,year,s] <- rcat(1,ps[State[n.sites,n.months,year-1,s], n.sites, n.months, ])
      }
      for (i in 2:(n.sites-1)){ 
        D[1,1,year,s] <- State[2,1,year,s]
        D[i,1,year,s] <- State[i-1,1,year,s] + State[i+1,1,year,s]
        D[n.sites,1,year,s] <- State[n.sites-1,1,year,s]
        
        #probability calculations
        gamma[i,1,year,s] <- invlogit(gamma0 + gamma1*D[i,1,year,s]) #invasion probability
        ps[1,i,1,1] <- 1-gamma[i,1,year,s]
        ps[1,i,1,2] <- gamma[i,1,year,s]
        ps[2,i,1,1] <- (eps*rem.vec[i,year,s])
        ps[2,i,1,2] <- 1- (eps*rem.vec[i,year,s]) 
        
        
        #probability calculations for edge
        gamma[1,1,year,s] <- invlogit(gamma0 + gamma1*D[1,1,year,s]) #invasion probability
        ps[1,1,1,1] <- 1-gamma[1,1,year,s]
        ps[1,1,1,2] <- gamma[1,1,year,s]
        ps[2,1,1,1] <- (eps*rem.vec[1,year,s])
        ps[2,1,1,2] <- 1- (eps*rem.vec[1,year,s]) 
        
        
        gamma[n.sites,1,year,s] <- invlogit(gamma0 + gamma1*D[n.sites,1,year,s]) #invasion probability
        ps[1,n.sites,1,1] <- 1-gamma[n.sites,1,year,s]
        ps[1,n.sites,1,2] <- gamma[n.sites,1,year,s]
        ps[2,n.sites,1,1] <- (eps*rem.vec[n.sites,year,s])
        ps[2,n.sites,1,2] <- 1- (eps*rem.vec[n.sites,year,s]) 
      }
    }
  } #ends year > 1 things
  
  ##### 1b. Months 1+ ####
  # State transition
  for(s in 1:n.sims){
    for (t in 2:n.months){ #12
      for (i in 2:(n.sites-1)){
        # State process: 
        State[i,t,year,s] <- rcat(1,ps[State[i,t-1,year,s], i, t-1, ])
        
        #Edge state process 
        State[1,t,year,s] <- rcat(1,ps[State[1,t-1,year,s], 1, t-1, ])
        State[n.sites,t,year,s] <- rcat(1,ps[State[n.sites,t-1,year,s], n.sites, t-1, ])
      }
      for (i in 2:(n.sites-1)){
        D[1,t,year,s] <- State[2,t,year,s]
        D[i,t,year,s] <- State[i-1,t,year,s] + State[i+1,t,year,s]
        D[n.sites,t,year,s] <- State[n.sites-1,t,year,s]
        
        #probability calculations
        gamma[i,t,year,s] <- invlogit(gamma0 + gamma1*D[i,t,year,s]) #invasion probability
        ps[1,i,t,1] <- 1-gamma[i,t,year,s]
        ps[1,i,t,2] <- gamma[i,t,year,s]
        ps[2,i,t,1] <- (eps*rem.vec[i,year,s])
        ps[2,i,t,2] <- 1- (eps*rem.vec[i,year,s]) 
        
        
        #probability calculations for edge
        gamma[1,t,year,s] <- invlogit(gamma0 + gamma1*D[1,t,year,s]) #invasion probability
        ps[1,1,t,1] <- 1-gamma[1,t,year,s]
        ps[1,1,t,2] <- gamma[1,t,year,s]
        ps[2,1,t,1] <- (eps*rem.vec[1,year,s])
        ps[2,1,t,2] <- 1- (eps*rem.vec[1,year,s]) 
        
        
        gamma[n.sites,t,year,s] <- invlogit(gamma0 + gamma1*D[n.sites,t,year,s]) #invasion probability
        ps[1,n.sites,t,1] <- 1-gamma[n.sites,t,year,s]
        ps[1,n.sites,t,2] <- gamma[n.sites,t,year,s]
        ps[2,n.sites,t,1] <- (eps*rem.vec[n.sites,year,s])
        ps[2,n.sites,t,2] <- 1- (eps*rem.vec[n.sites,year,s]) 
      }
      
    } #t
  } #s
}
  