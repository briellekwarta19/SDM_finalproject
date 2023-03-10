library(LaplacesDemon)
library(rjags)
library(R2jags)
library(MCMCvis)
library(tidyverse)
library(strex)
library(plyr)

#Remove at downstream 5 locations but collect monitoring data
#at next 5 downstream locations

start.time <- Sys.time()

#### jags model ####
sink("Flower_mod.txt")
cat("
model{

# -------------------------------------------------
# Parameters:
# gamma: invasion probability
# eps: erradication probability
# p: probability of observing the plant
# -------------------------------------------------
# States (S):
# 1 empty
# 2 invaded
# 
# Observations (O):  
# 1 absent
# 2 present
# -------------------------------------------------

# Priors 
  eps ~ dbeta(eps_a,eps_b) #erradication
  p ~ dbeta(p_a,p_b) #detection
  gamma0 ~dnorm(gamma0_a,gamma0_b) #intrinsic invasion probability 
  gamma1 ~ dnorm(gamma1_a, gamma1_b) #effect of neighboring locations on invasion probability


# Define state-transition and observation matrices
for (i in 1:n.sites){  
  # State transition probabilities: probability of S(t+1) given S(t)
  for (t in 1:n.months){
    #index = [current state, location, time, future state]
    #empty stay empty
    ps[1,i,t,1] <- 1-gamma[i,t]
    
    #empty to invaded
    ps[1,i,t,2] <- gamma[i,t]

    #invaded to empty
    ps[2,i,t,1] <- (eps*rem.vec[i])
    
    #invaded to invaded
    ps[2,i,t,2] <- 1- (eps*rem.vec[i]) 

      
    # Observation probabilities of given S(t)
    #index = [current state, location, time, current observation]
   
    #empty observed absent   
    po[1,i,t,1] <- 1
    
    #empty observed present  
    po[1,i,t,2] <- 0
 
    #invaded observed absent 
    po[2,i,t,1] <- 1-p
    
    #invaded observed present
    po[2,i,t,2] <- p
   
   #### PRIORS ####
   logit(gamma[i,t]) <- gamma0 + gamma1*D[i,t] #invasion probability

   
  } #t
} #i

  # Likelihood 
  for (i in 2:(n.sites-1)){
    # Define state at beginning
    State[i,1] <- S.init[i] #we know state at the start
    D[i,1] <- D.init[i]
    
    for (t in 2:n.months){ #12
      # State process: 
      State[i,t] ~ dcat(ps[State[i,t-1], i, t-1, ])
      
      
      D[i,t] <- State[i-1,t] + State[i+1,t]
      
     
    } #t
      
      #Derived parameter:
      
      State.fin[i] <- State[i,n.months]
  } #i
  
  #Fill in above for edge sites
  State[1,1] <- S.init[1] 
  State[n.sites,1] <- S.init[n.sites] 
  D[1,1] <- D.init[1]
  D[n.sites,1] <- D.init[n.sites]
  
   for (t in 2:n.months){
      State[1,t] ~ dcat(ps[State[1,t-1], 1, t-1, ])
      State[n.sites,t] ~ dcat(ps[State[n.sites,t-1], n.sites, t-1, ])
      D[1,t] <- State[2,t]
      D[n.sites,t] <- State[n.sites-1,t]
    } 

  State.fin[1] <- State[1,n.months]
  State.fin[n.sites] <- State[n.sites,n.months]
  
  
  for(h in 1:n.obs){
     # Observation process: draw O(t) given S(t) 

      y[site.obs[h]] ~ dcat(po[State.fin[site.obs[h]], site.obs[h], n.months-1, ])
    
  } #obs sites
  

} #end model
", fill = TRUE)
sink()


#### Data ####
n.rem <- 5 #number of removal locations
n.obs <- 10 #we collect detection/non detection at 3 locations
n.sims <- 5
eps <- 0.6 #eradication probability 
p <- 0.6 #detection probability
n.sites <- 50 #number of sites
n.months <- 12
n.year <- 10 #time steps

##### Initial Removal Sites #####
#initially remove at random locations
site.rem <- array(NA, c(n.rem,n.year,n.sims))
site.rem[,1,] <- sample(seq(1,n.sites), n.rem)

#vector of locations where removal occurs this year 1 for removal, 0 for no removal
rem.vec <- array(0, c(n.sites, n.year, n.sims))
rem.vec[site.rem[,1,],1,] <- 1

n.rem.year <- array(NA, c(n.year, n.sims))
n.rem.year[1,] <- n.rem

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

#observation matrix:
#Let 1 = absent, 2 = present
#we make one observation each year
Obs <- array(NA, c(n.sites, n.year, n.sims))

#assign year 1 locations for detection/non detection
#find locations where removal is not happening and choose those locations randomly
available.obs <- setdiff(seq(1,n.sites), site.rem[,1,1])

site.obs <- array(NA, c(n.obs, n.year, n.sims))
site.obs[,1,] <- sort(sample(available.obs, n.obs))

po<- array(NA, c(2,2))
po[1,1] <- 1
po[1,2] <- 0
po[2,1] <- 1-p
po[2,2] <- p

# JAGS data
gamma0_a <- array(NA, c(n.year,n.sims))
gamma0_b <- array(NA, c(n.year,n.sims))

gamma1_a <- array(NA, c(n.year,n.sims))
gamma1_b <- array(NA, c(n.year,n.sims))

eps_a <- array(NA, c(n.year, n.sims))
eps_b <- array(NA, c(n.year, n.sims))

p_a <- array(NA, c(n.year, n.sims))
p_b <- array(NA, c(n.year, n.sims))

S.init <-  array(NA, c(n.sites, n.year, n.sims))
D.init <- array(NA, c(n.sites,n.year,n.sims))
y.dat <- array(NA, c(n.sites,n.year, n.sims))
rhat_vals <- array(NA, c(n.year, n.sims))
my.data <- list()
outs <- rep(NA,n.sims)
outputsfull <- rep(NA, n.sims)
outputs <- rep(NA, n.sims)
mcmcs <- rep(NA, n.sims)
x <- list()
final.states <- rep(NA, n.sims)
segments <- list()

gamma0.est <- rep(NA, n.sims)
gamma1.est <- rep(NA, n.sims)
eps.est <- rep(NA, n.sims)
p.est <- rep(NA, n.sims)

all.final.states <- rep(NA, n.sims)
all.gamma0.est <- rep(NA, n.sims)
all.gamma1.est <- rep(NA, n.sims)
all.eps.est <- rep(NA, n.sims)
all.p.est <- rep(NA, n.sims)
final.states.mean <- array(NA, dim = c(n.sites, n.year, n.sims))
site.rem.options <- list()

alpha.gammas <- rep(NA, n.sims)
beta.gammas <- rep(NA, n.sims)

alpha.eps <- rep(NA, n.sims)
beta.eps <- rep(NA, n.sims)

alpha.ps <- rep(NA, n.sims)
beta.ps <- rep(NA, n.sims)

max.down <- rep(NA, n.sims)
min.down <- rep(NA, n.sims)
site.obs.next <- array(NA, c(n.obs,n.sims))
next.down <- list()
add.obs <- list()
inbetween.rem <- list()
add.obs1 <- list()
add.obs2 <- list()
available.obs.site <- list()

#### Path Name ####
path <- here::here("Final_Project", "results", "remrand5_obsrand10")
res <- c('Final_Project/results/remrand5_obsrand10') #subset of path for plot save

####################################################################################
#### Run Adaptive Management ####

years <- n.year
#years <- 2
#year <- 1

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
    
    for(i in 1:n.sites){
      # Observation process: draw O(t) given S(t) 
      Obs[i,year,s] <- rcat(1,po[State[i,n.months,year,s], ])
    } 
  } #s
  
  #### 2. Learning: ####
  
  ##### 2a. Update priors #####
  
  #------------------------Year 1 Priors------------------------#
  if(year == 1){
    eps_a[1,] <- 1
    eps_b[1,] <- 1
    p_a[1,] <- 1
    p_b[1,] <- 1
    
    gamma0_a[1,] <- 0
    gamma0_b[1,] <- 1
    gamma1_a[1,] <- 0
    gamma1_b[1,] <- 0.1
    
    #random initial states for year 1
    #S.init[,year,] <- sample(c(1,2), n.sites*n.sims, replace = T) 
    
    #initial 5 are invaded but no clue about the rest
    ##### Initial Estimated State #####
    S.init[1:5,year,] <- 2
    S.init[6:n.sites,year,] <- sample(c(1,2), (n.sites-5)*n.sims, replace = T) 

    
    
    
  } else{
    for(s in 1:n.sims){
    #------------------------Year 1+ Priors------------------------#
      
      
    #-------gamma priors: invasion probability -------#
    
      gamma0_a[year,s] <- get(gamma0.est[s])$mean
      gamma0_b[year,s] <- get(gamma0.est[s])$sd
      gamma1_a[year,s] <- get(gamma1.est[s])$mean
      gamma1_b[year,s] <- get(gamma1.est[s])$sd
    
    #-------eps priors: eradication probability -------#
    
    alpha.eps[s]<- paste("alpha.eps", s, sep = "_")
    #assigning alpha values for beta: alpha = (1-mean)*(1+cv^2)/cv^2
    assign(alpha.eps[s],
           (1 - get(eps.est[s])$mean*(1 + get(eps.est[s])$cv^2))/(get(eps.est[s])$cv^2))
    
    beta.eps[s]<- paste("beta.eps", s, sep = "_")
    #assigning beta values for beta: beta = (alpha)*(1-mean)/(mean)
    assign(beta.eps[s],
           get(alpha.eps[s])*(1 - get(eps.est[s])$mean)/get(eps.est[s])$mean)
    
    #assigning these values for the next jags run:
    eps_a[year,s] <- get(alpha.eps[s])
    eps_b[year,s] <- get(beta.eps[s])
    
    
    #-------p priors: detection probability -------#
    alpha.ps[s]<- paste("alpha.p", s, sep = "_")
    #assigning alpha values for beta: alpha = (1-mean)*(1+cv^2)/cv^2
    assign(alpha.ps[s],
           (1 - get(p.est[s])$mean*(1 + get(p.est[s])$cv^2))/(get(p.est[s])$cv^2))
    
    beta.ps[s]<- paste("beta.p", s, sep = "_")
    #assigning beta values for beta: beta = (alpha)*(1-mean)/(mean)
    assign(beta.ps[s],
           get(alpha.ps[s])*(1 - get(p.est[s])$mean)/get(p.est[s])$mean)
    
    #assigning these values for the next jags run:
    p_a[year,s] <- get(alpha.ps[s])
    p_b[year,s] <- get(beta.ps[s])
    
    
    #-------gamma priors: invasion probability -------#
    
    S.init[,year,s] <- final.states.mean[,(year-1),s]
    
    for(i in 1:n.sites){
      if(S.init[i,year,s] > 1.8){
        S.init[i,year,s] <- 2
      } else{
        S.init[i,year,s] <- 1
      }
    }  
    
    }
  }
 
  ###### 2b. JAGS data ######
  
  for(s in 1:n.sims){
    y.dat[site.obs[,year,s],year,s] <- Obs[site.obs[,year,s],year,s]
  }
  
  #D.init 
  for(s in 1:n.sims){
    for(i in 2:(n.sites-1)){
      D.init[i,year,s] <- S.init[i+1,year,s] + S.init[i-1,year,s]
    }
    
    D.init[1,year,s] <- S.init[2,year,s]
    D.init[n.sites,year,s] <- S.init[(n.sites-1),year,s]
    
  }

  
  #y.dat
  
  #Parameters monitored
  parameters.to.save <- c("State.fin", "eps",
                          "gamma0","gamma1",
                          "p")
  
  
  #settings
  n.burnin <- 1000
  n.iter <- 100000 + n.burnin
  n.chains <- 3
  n.thin <- 1
  
  for(s in 1:n.sims){
    my.data[[s]] <- list(n.sites = n.sites,
                         n.months = n.months, 
                         n.obs = n.obs,
                         S.init = S.init[,year,s],
                         D.init = D.init[,year,s],
                         y = y.dat[,year,s],
                         rem.vec = rem.vec[,year,s],
                         site.obs = site.obs[,year,s], 
                         
                         #priors
                         gamma0_a = gamma0_a[year,s],
                         gamma0_b = gamma0_b[year,s],
                         gamma1_a = gamma1_a[year,s],
                         gamma1_b = gamma1_b[year,s],
                         eps_a = eps_a[year,s], 
                         eps_b = eps_b[year,s],
                         p_a = p_a[year,s], 
                         p_b = p_b[year,s]
    )
  }
  
  ###### 2c. Run JAGS #####
  State.init <- array(NA, c(n.sites,n.months))
  State.init[,2:n.months] <- 2
  
  #State.init[,2:n.months] <- State[,2:n.months,year]
  initial.values <- function(){list(State = State.init)}
  
  for(s in 1:n.sims){
    outs[s]<- paste("out", s, sep = "_")
    assign(outs[s],
           jagsUI::jags(data = my.data[[s]],inits = initial.values,
                        parameters.to.save = parameters.to.save, model.file = "Flower_mod.txt",
                        n.chains = n.chains, n.thin = n.thin, n.iter = n.iter , n.burnin = n.burnin))
  }
  
  #### 3. Decision for year 2 ####
  ###### 3a. Extract parameters #####
  for(s in 1:n.sims){ 
    outputsfull[s]<- paste("outputfull", s, sep = "_")
    assign(outputsfull[s], 
           get(outs[s]))
    
    outputs[s]<- paste("output", s, sep = "_")
    assign(outputs[s], 
           as.data.frame((get(outputsfull[s]))$summary))
    
    assign(outputs[s], 
           cbind(get(outputs[s]), param = rownames(get(outputs[s]))))
  }
  

  
  for(s in 1:n.sims){
    mcmcs[s]<- paste("mcmc", s, sep = "_")
  }
  
  mcmc_1 <- out_1$samples
  mcmc_2 <- out_2$samples
  mcmc_3 <- out_3$samples
  mcmc_4 <- out_4$samples
  mcmc_5 <- out_5$samples
 
  for(s in 1:n.sims){
    MCMCtrace(get(mcmcs[s]), 
            params = 'gamma0', 
            type = 'density', 
            ind = TRUE, 
            pdf = TRUE, 
            open_pdf = FALSE,
            filename = paste0(res,'/gamma0_sim_', s, '_year', year))
    
    MCMCtrace(get(mcmcs[s]), 
              params = 'gamma1', 
              type = 'density', 
              ind = TRUE, 
              pdf = TRUE, 
              open_pdf = FALSE,
              filename = paste0(res,'/gamma1_sim_', s, '_year', year))
    
    MCMCtrace(get(mcmcs[s]), 
              params = 'eps', 
              type = 'density', 
              ind = TRUE, 
              pdf = TRUE, 
              open_pdf = FALSE,
              filename = paste0(res,'/eps_sim_', s, '_year', year))
    
    MCMCtrace(get(mcmcs[s]), 
              params = 'p', 
              type = 'density', 
              ind = TRUE, 
              pdf = TRUE, 
              open_pdf = FALSE,
              filename = paste0(res,'/p_sim_', s, '_year', year))
    }
    
  
  #save outs into a csv file!
  # file_name <- rep(NA, S)
  # for(s in 1:S){
  #   file_name[s] = paste(path, paste0('Output_',s,'year', year, '.csv'),sep = '/')
  #   write.csv(get(outputs[s]),file_name[s])
  # }
  # 
  
  #save rhat outputs
  for(s in 1:n.sims){
    x[[s]] <- as.numeric(unlist(get(outputsfull[s])$Rhat))
    rhat_vals[year,s] <-  sum(x[[s]] > 1.1, na.rm = TRUE)/ length(x[[s]])
  }
  

  #-------Final States -------#
  for(s in 1:n.sims){
    final.states[s]<- paste("final.states", s, sep = "_")
    assign(final.states[s], filter(get(outputs[s]), grepl("^S", param)))
    
    segments[[s]] <- as.numeric(str_nth_number((get(final.states[s]))$param, n = 1))
    
  }
  
  
  for(s in 1:n.sims){
    assign(final.states[s], 
           cbind(get(final.states[s]), segment = segments[[s]])) #adding segment column  
  }

  #-------gamma -------#
  for(s in 1:n.sims){
    gamma0.est[s]<- paste("gamma0.est", s, sep = "_")
    assign(gamma0.est[s], filter(get(outputs[s]), grepl("^gamma0", param)))
    
    gamma1.est[s]<- paste("gamma1.est", s, sep = "_")
    assign(gamma1.est[s], filter(get(outputs[s]), grepl("^gamma1", param)))
    
  }
  
  #-------eps -------#
  for(s in 1:n.sims){
    eps.est[s]<- paste("eps.est", s, sep = "_")
    assign(eps.est[s], filter(get(outputs[s]), grepl("^e", param)))
    
    assign(eps.est[s], 
           cbind(get(eps.est[s]), cv = get(eps.est[s])$sd/get(eps.est[s])$mean
           ))  
  }
  
  #-------p -------#
  for(s in 1:n.sims){
    p.est[s]<- paste("p.est", s, sep = "_")
    assign(p.est[s], filter(get(outputs[s]), grepl("^p", param)))
    
    assign(p.est[s], 
           cbind(get(p.est[s]), cv = get(p.est[s])$sd/get(p.est[s])$mean
           ))  
    
  }
  
  #save annual data
  for(s in 1:n.sims){
    assign(final.states[s], 
           cbind(get(final.states[s]), year = year))
    
    assign(gamma0.est[s], 
           cbind(get(gamma0.est[s]), year = year))
    
    assign(gamma1.est[s], 
           cbind(get(gamma1.est[s]), year = year))
    
    assign(eps.est[s], 
           cbind(get(eps.est[s]), year = year))
    
    assign(p.est[s], 
           cbind(get(p.est[s]), year = year))
    
    all.final.states[s]<- paste("final.states.allsummary", s, sep = "_")
    all.gamma0.est[s]<- paste("gamma0.allsummary", s, sep = "_")
    all.gamma1.est[s]<- paste("gamma1.allsummary", s, sep = "_")
    all.eps.est[s]<- paste("eps.allsummary", s, sep = "_")
    all.p.est[s]<- paste("p.allsummary", s, sep = "_")

    
  }
  
  #If year 1 we set summary data frame to itself
  for(s in 1:n.sims){
    if(year == 1){
      assign(all.final.states[s], 
             get(final.states[s]))
      
      assign(all.gamma0.est[s], 
             get(gamma0.est[s]))
      
      assign(all.gamma1.est[s], 
             get(gamma1.est[s]))
      
      assign(all.eps.est[s], 
             get(eps.est[s]))
      
      assign(all.p.est[s], 
             get(p.est[s]))
     
  
      
    }else{ #if beyond first year, we append previous summary to new summary
      
      assign(all.final.states[s], 
             rbind(get(all.final.states[s]), get(final.states[s])))
      
      assign(all.gamma0.est[s], 
             rbind(get(all.gamma0.est[s]), get(gamma0.est[s])))
      
      assign(all.gamma1.est[s], 
             rbind(get(all.gamma1.est[s]), get(gamma1.est[s])))
      
      assign(all.eps.est[s],
             rbind(get(all.eps.est[s]), get(eps.est[s])))
      
      assign(all.p.est[s],
             rbind(get(all.p.est[s]), get(p.est[s])))
     
    }
  }
  
  ###### 3b. Make decision  #####
  #Removal decision: remove at 5 random segments
  #Extract final states from model:
  if(year < years){
    for(s in 1:n.sims){
      final.states.mean[,year,s] <- (get(final.states[s]))$mean
      
      site.rem.options[[s]] <- which(final.states.mean[,year,s] == 2)
      
    }
    
    
    
    for(s in 1:n.sims){
      if(length(site.rem.options[[s]]) < n.rem){
        
        site.rem[,year+1,s] <- c(site.rem.options[[s]], rep(NA,n.rem-length(site.rem.options[[s]])))
      }else{
        site.rem[,year+1,s] <- sample(site.rem.options[[s]], n.rem)
      }
      
      rem.vec[site.rem[,year+1,s],year+1,s] <- 1
    }
    
    
    #compare that with the truth:
    # site.rem[,year+1,] #what we are going to do
    # tail(which(State[,n.months,year,2] == 2),n.rem) #the truth
    
    #Monitoring decision: remove at 5 random sites
    
    
    for(s in 1:n.sims){
      
      available.obs.site[[s]] <- setdiff(seq(1,n.sites), site.rem[,year+1,s])
      site.obs[,year+1,s] <- sort(sample(available.obs.site[[s]], n.obs))
      
      
    } #ends s loop

  } #ends year < years loop

  
} #end adaptive management 

#################################################################################################
#### TIMING ####
end.time <- Sys.time()
time.taken <- end.time - start.time

file_name = paste(path, 'time.txt',sep = '/')
write.table(time.taken,file_name)

#### Save true states ####
State.dat <- adply(State, c(1,2,3,4))
colnames(State.dat) <- c("segment", "month", "year", "sim", "state")
file_name = paste(path, 'rrand5_orand10_truestates.csv',sep = '/')
write.csv(State.dat,file_name)

#### Save estimation data ####  

#Save estimated states
final.states.allsummary_1$sim <- 1
final.states.allsummary_2$sim <- 2
final.states.allsummary_3$sim <- 3
final.states.allsummary_4$sim <- 4
final.states.allsummary_5$sim <- 5


final.states.est <- rbind(final.states.allsummary_1, final.states.allsummary_2,
                          final.states.allsummary_3, final.states.allsummary_4,
                          final.states.allsummary_5)

file_name = paste(path, 'rrand5_orand10_estfinalstates.csv',sep = '/')
write.csv(final.states.est,file_name)

#Save estimated gamma
gamma0.allsummary_1$sim <- 1
gamma0.allsummary_2$sim <- 2
gamma0.allsummary_3$sim <- 3
gamma0.allsummary_4$sim <- 4
gamma0.allsummary_5$sim <- 5

gamma0.est <- rbind(gamma0.allsummary_1, gamma0.allsummary_2,
                    gamma0.allsummary_3, gamma0.allsummary_4,
                    gamma0.allsummary_5)

file_name = paste(path, 'rrand5_orand10_gamma0est.csv',sep = '/')
write.csv(gamma0.est,file_name)

gamma1.allsummary_1$sim <- 1
gamma1.allsummary_2$sim <- 2
gamma1.allsummary_3$sim <- 3
gamma1.allsummary_4$sim <- 4
gamma1.allsummary_5$sim <- 5

gamma1.est <- rbind(gamma1.allsummary_1, gamma1.allsummary_2,
                    gamma1.allsummary_3, gamma1.allsummary_4,
                    gamma1.allsummary_5)

file_name = paste(path, 'rrand5_orand10_gamma1est.csv',sep = '/')
write.csv(gamma1.est,file_name)

#Save estimated eps
eps.allsummary_1$sim <- 1
eps.allsummary_2$sim <- 2
eps.allsummary_3$sim <- 3
eps.allsummary_4$sim <- 4
eps.allsummary_5$sim <- 5

eps.est <- rbind(eps.allsummary_1, eps.allsummary_2,
                 eps.allsummary_3, eps.allsummary_4,
                 eps.allsummary_5)

file_name = paste(path, 'rrand5_orand10_epsest.csv',sep = '/')
write.csv(eps.est,file_name)

#Save estimated p
p.allsummary_1$sim <- 1
p.allsummary_2$sim <- 2
p.allsummary_3$sim <- 3
p.allsummary_4$sim <- 4
p.allsummary_5$sim <- 5

p.est <- rbind(p.allsummary_1, p.allsummary_2,
               p.allsummary_3, p.allsummary_4, 
               p.allsummary_5)

file_name = paste(path, 'rrand5_orand10_pest.csv',sep = '/')
write.csv(p.est,file_name)

#Save removal locations
site.rem.dat <- adply(site.rem, c(1,2,3))
colnames(site.rem.dat) <- c("n.rem", "year", "sim", "segment")
file_name = paste(path, 'rrand5_orand10_segrem.csv',sep = '/')
write.csv(site.rem.dat,file_name)

#Save observation locations
site.obs.dat <- adply(site.obs, c(1,2,3))
colnames(site.obs.dat) <- c("n.obs", "year", "sim", "segment")
file_name = paste(path, 'rrand5_orand10_segobs.csv',sep = '/')
write.csv(site.obs.dat,file_name)

#Save rhat values 
rhat.dat <- data.frame(rhat_vals)
colnames(rhat.dat) <- c("sim1", "sim2", "sim3", "sim4", "sim5")
file_name = paste(path, 'rrand5_orand10_rhatdat.csv',sep = '/')
write.csv(rhat.dat,file_name)
