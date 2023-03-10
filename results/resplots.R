library(tidyverse)
library(here)

#load files:
n.sites <- 50
#### Results  ####

##### Down no obs  #####
path <- here::here("results", "remdown5_noobs")
file_name = paste(path, 'rdown5_noobs_truestates.csv',sep = '/')
true_states_rdown_noobs1 <- read.csv(file_name)[-1]
unique(true_states_rdown_noobs1$sim)

path <- here::here("results", "remdown5_noobs2")
file_name = paste(path, 'rdown5_noobs_truestates.csv',sep = '/')
true_states_rdown_noobs2 <- read.csv(file_name)[-1]
unique(true_states_rdown_noobs2$sim)

true_states_rdown_noobs <- rbind(true_states_rdown_noobs1, true_states_rdown_noobs2)

path <- here::here("results", "remdown5_noobs")
file_name = paste(path, 'rdown5_noobs_estfinalstates.csv',sep = '/')
est_states_rdown_noobs1 <- read.csv(file_name)[-1]
unique(est_states_rdown_noobs1$sim)

path <- here::here("results", "remdown5_noobs2")
file_name = paste(path, 'rdown5_noobs_estfinalstates.csv',sep = '/')
est_states_rdown_noobs2 <- read.csv(file_name)[-1]
unique(est_states_rdown_noobs2$sim)

est_states_rdown_noobs <- rbind(est_states_rdown_noobs1, est_states_rdown_noobs2)

##### Down 5 obs  #####
path <- here::here("results", "remdown5_obsdown5")
file_name = paste(path, 'rdown5_odown5_truestates.csv',sep = '/')
true_states_rdown_odown51 <- read.csv(file_name)[-1]
unique(true_states_rdown_odown51$sim)

path <- here::here("results", "remdown5_obsdown52")
file_name = paste(path, 'rdown5_odown5_truestates.csv',sep = '/')
true_states_rdown_odown52 <- read.csv(file_name)[-1]
unique(true_states_rdown_odown52$sim)

true_states_rdown_odown5 <- rbind(true_states_rdown_odown51,true_states_rdown_odown52)

path <- here::here("results", "remdown5_obsdown5")
file_name = paste(path, 'rdown5_odown5_estfinalstates.csv',sep = '/')
est_states_rdown_odown51 <- read.csv(file_name)[-1]
unique(est_states_rdown_odown51$sim)

path <- here::here("results", "remdown5_obsdown52")
file_name = paste(path, 'rdown5_odown5_estfinalstates.csv',sep = '/')
est_states_rdown_odown52 <- read.csv(file_name)[-1]
unique(est_states_rdown_odown52$sim)

est_states_rdown_odown5 <- rbind(est_states_rdown_odown51,est_states_rdown_odown52)

##### Down 10 obs  #####
path <- here::here("results", "remdown5_obsdown10")
file_name = paste(path, 'rdown5_odown10_truestates.csv',sep = '/')
true_states_rdown_odown101 <- read.csv(file_name)[-1]
unique(true_states_rdown_odown101$sim)

path <- here::here("results", "remdown5_obsdown102")
file_name = paste(path, 'rdown5_odown10_truestates.csv',sep = '/')
true_states_rdown_odown102 <- read.csv(file_name)[-1]
unique(true_states_rdown_odown102$sim)

true_states_rdown_odown10 <- rbind(true_states_rdown_odown101, true_states_rdown_odown102)

path <- here::here("results", "remdown5_obsdown10")
file_name = paste(path, 'rdown5_odown10_estfinalstates.csv',sep = '/')
est_states_rdown_odown101 <- read.csv(file_name)[-1]
unique(est_states_rdown_odown101$sim)

path <- here::here("results", "remdown5_obsdown102")
file_name = paste(path, 'rdown5_odown10_estfinalstates.csv',sep = '/')
est_states_rdown_odown102 <- read.csv(file_name)[-1]
unique(est_states_rdown_odown102$sim)

est_states_rdown_odown10 <- rbind(est_states_rdown_odown101,est_states_rdown_odown102)

##### Rand no obs  #####
path <- here::here("results", "remrand5_noobs")
file_name = paste(path, 'rrand5_noobs_truestates.csv',sep = '/')
true_states_rrand_noobs1 <- read.csv(file_name)[-1]
unique(true_states_rrand_noobs1$sim)

path <- here::here("results", "remrand5_noobs2")
file_name = paste(path, 'rrand5_noobs_truestates.csv',sep = '/')
true_states_rrand_noobs2 <- read.csv(file_name)[-1]
unique(true_states_rrand_noobs2$sim)

true_states_rrand_noobs <- rbind(true_states_rrand_noobs1,true_states_rrand_noobs2)

path <- here::here("results", "remrand5_noobs")
file_name = paste(path, 'rrand5_noobs_estfinalstates.csv',sep = '/')
est_states_rrand_noobs1 <- read.csv(file_name)[-1]
unique(est_states_rrand_noobs1$sim)

path <- here::here("results", "remrand5_noobs2")
file_name = paste(path, 'rrand5_noobs_estfinalstates.csv',sep = '/')
est_states_rrand_noobs2 <- read.csv(file_name)[-1]
unique(est_states_rrand_noobs2$sim)

est_states_rrand_noobs <- rbind(est_states_rrand_noobs1,est_states_rrand_noobs2)

##### Rand 5 obs  #####
path <- here::here("results", "remrand5_obsrand5")
file_name = paste(path, 'rrand5_orand5_truestates.csv',sep = '/')
true_states_rrand_orand51 <- read.csv(file_name)[-1]
unique(true_states_rrand_orand51$sim)


path <- here::here("results", "remrand5_obsrand52")
file_name = paste(path, 'rrand5_orand5_truestates.csv',sep = '/')
true_states_rrand_orand52 <- read.csv(file_name)[-1]
unique(true_states_rrand_orand52$sim)

true_states_rrand_orand5 <- rbind(true_states_rrand_orand51,true_states_rrand_orand52)

path <- here::here("results", "remrand5_obsrand5")
file_name = paste(path, 'rrand5_orand5_estfinalstates.csv',sep = '/')
est_states_rrand_orand51 <- read.csv(file_name)[-1]
unique(est_states_rrand_orand51$sim)

path <- here::here("results", "remrand5_obsrand52")
file_name = paste(path, 'rrand5_orand5_estfinalstates.csv',sep = '/')
est_states_rrand_orand52 <- read.csv(file_name)[-1]
unique(est_states_rrand_orand52$sim)

est_states_rrand_orand5 <- rbind(est_states_rrand_orand51, est_states_rrand_orand52)

##### Rand 10 obs  #####
path <- here::here("results", "remrand5_obsrand10")
file_name = paste(path, 'rrand5_orand10_truestates.csv',sep = '/')
true_states_rrand_orand101 <- read.csv(file_name)[-1]
unique(true_states_rrand_orand101$sim)

path <- here::here("results", "remrand5_obsrand102")
file_name = paste(path, 'rrand5_orand10_truestates.csv',sep = '/')
true_states_rrand_orand102 <- read.csv(file_name)[-1]
unique(true_states_rrand_orand102$sim)

true_states_rrand_orand10 <- rbind(true_states_rrand_orand101,true_states_rrand_orand102)

path <- here::here("results", "remrand5_obsrand10")
file_name = paste(path, 'rrand5_orand10_estfinalstates.csv',sep = '/')
est_states_rrand_orand101 <- read.csv(file_name)[-1]
unique(est_states_rrand_orand101$sim)

path <- here::here("results", "remrand5_obsrand102")
file_name = paste(path, 'rrand5_orand10_estfinalstates.csv',sep = '/')
est_states_rrand_orand102 <- read.csv(file_name)[-1]
unique(est_states_rrand_orand102$sim)

est_states_rrand_orand10 <- rbind(est_states_rrand_orand101,est_states_rrand_orand102)

#### Truth results ####
true_states_rdown_noobs$dr <- c("down_no_obs")
true_states_rdown_odown5$dr <- c("down_5obs")
true_states_rdown_odown10$dr <- c("down_10obs")

true_states_rrand_noobs$dr <- c("rand_no_obs")
true_states_rrand_orand5$dr <- c("rand_5obs")
true_states_rrand_orand10$dr <- c("rand_10obs")

true_states <- rbind(true_states_rdown_noobs,
                     true_states_rdown_odown5,
                     true_states_rdown_odown10,
                     true_states_rrand_noobs,
                     true_states_rrand_orand5,
                     true_states_rrand_orand10)

true_states_final <- true_states %>% filter(month == 12, year == 10)

#Average final states across sims
# mapdat <- aggregate(state ~ segment + dr,
#                     data = as.data.frame(true_states_final), FUN = mean)

# path <- here::here("results")
# file_name = paste(path, 'map_true_finalstates.csv',sep = '/')
# write.csv(mapdat,file_name)

#### Estimation results ####
est_states_rdown_noobs$dr <- c("down_no_obs")
est_states_rdown_odown5$dr <- c("down_5obs")
est_states_rdown_odown10$dr <- c("down_10obs")
est_states_rrand_noobs$dr <- c("rand_no_obs")
est_states_rrand_orand5$dr <- c("rand_5obs")
est_states_rrand_orand10$dr <- c("rand_10obs")

est_states <- rbind(est_states_rdown_noobs,
                     est_states_rdown_odown5,
                     est_states_rdown_odown10,
                     est_states_rrand_noobs,
                     est_states_rrand_orand5,
                     est_states_rrand_orand10)

est_states_final <- est_states %>% filter(year == 10)

est_states_final$type <- c("estimate")


est_states_final <- est_states_final %>% select(segment, year,sim,mean,dr,type)
colnames(est_states_final)[4] <- c("state")

#what does the est_states_final tell us what is best?
est_states_final$state[est_states_final$state>1.5] <- 2
est_states_final$state[est_states_final$state<= 1.5] <- 1

#### Objective 1 results ####
#Objective 1 = minimize overall presence 
est_avgstates<- aggregate(state ~ dr,
          data = as.data.frame(est_states_final), FUN = mean)

true_avgstates <- aggregate(state ~ dr,
          data = as.data.frame(true_states_final), FUN = mean)

colnames(true_avgstates)[2] <- ("True state")


min_presence <- cbind(true_avgstates, est_avgstates$state)

colnames(min_presence)[3] <- ("Estimated state")

min_presence$dr <- c("Downstream, 10 observations", 
                      "Downstream, 5 observations", 
                      "Downstream, No observations",
                      "Random, 10 observations", 
                      "Random, 5 observations",
                      "Random, No observations")


colnames(min_presence)[1] <- c("Alternative")
min_presence

#### Objective 2 results ####
#Objective 2 = minimize downstream presence (across final 5km) 

est_states_final_last5 <- est_states_final %>% filter(segment %in% c(46,47,48,49,50))

est_finalstates<- aggregate(state ~ dr,
                          data = as.data.frame(est_states_final_last5), FUN = mean)


true_states_final_last5 <- true_states_final %>% filter(segment %in% seq(n.sites-4, n.sites))
true_finalstates <- aggregate(state ~ dr,
                            data = as.data.frame(true_states_final_last5), FUN = mean)

colnames(true_finalstates)[2] <- ("True state")


min_downstream <- cbind(true_finalstates, est_finalstates$state)


min_downstream$dr <- c("Downstream, 10 observations", 
                      "Downstream, 5 observations", 
                      "Downstream, No observations",
                      "Random, 10 observations", 
                      "Random, 5 observations",
                      "Random, No observations")

colnames(min_downstream)[3] <- ("Estimated state")
colnames(min_downstream)[1] <- c("Alternative")

min_downstream


#### Pareto optimal ####
PO_dat <- data.frame(min_downstream$Alternative, min_downstream$`True state`, c(10000,5000,0,10000,5000,0))
colnames(PO_dat) <- c("Alternative", "MinP", "MinC")
PO_dat$Alternative <- as.factor(c(1,2,3,4,5,6))
PO_dat$MinP <- PO_dat$MinP-1

PO_dat$col <- c("purple", "green", "green", "purple", "purple", "purple")
PO_dat$dom <- c("dominated", "not dominated", "not dominated", "dominated", "dominated", "dominated")

ggplot(PO_dat) +
  geom_point(mapping = aes(x = MinP, y = MinC, color = dom),size = 5) +
  scale_color_manual(values=c("purple", "green"))+
  xlab("Minimize downstream presence") +
  ylab("Minimize monitoring cost ($/year)")+
  theme_grey(base_size = 18)






