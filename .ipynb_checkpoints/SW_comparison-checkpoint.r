library(PracTools)

results <- as.data.frame(list(Variable = rep(NA,24),
                              Package = rep(NA,24),
                              Unit = rep(NA,24),
                              Units = rep(NA,24)))

load("pop.RData")
str(pop)

# Probabilities of inclusion I stage
pp <- as.numeric(table(pop$municipality))/nrow(pop)

bw <- BW2stagePPS(pop$income_hh, 
      pp,
      psuID=pop$municipality)
bw
des <- clusOpt2(C1=130,
               C2=1,
               delta=bw[6],
               unit.rv=bw[3],
               k=bw[5],
               CV0=0.02,
               tot.cost=NULL,
               cal.sw=2)
des
sample_size <- des$m.opt*des$n.opt
results[1,1] <- "income_hh"
results[1,2] <- "PractTools"
results[1,3] <- "PSU" 
results[1,4] <- round(des$m.opt)
results[2,1] <- "income_hh"
results[2,2] <- "PractTools"
results[2,3] <- "SSU" 
results[2,4] <- round(des$m.opt*des$n.opt)



bw <- BW2stagePPS(pop$active, 
                  pp,
                  psuID=pop$municipality)
bw
des <- clusOpt2(C1=180,
                C2=1,
                delta=bw[6],
                unit.rv=bw[3],
                k=bw[5],
                CV0=0.03,
                tot.cost=NULL,
                cal.sw=2)
des
sample_size <- des$m.opt*des$n.opt
sample_size
results[3,1] <- "active"
results[3,2] <- "PractTools"
results[3,3] <- "PSU" 
results[3,4] <- round(des$m.opt)
results[4,1] <- "active"
results[4,2] <- "PractTools"
results[4,3] <- "SSU" 
results[4,4] <- round(des$m.opt*des$n.opt)

# Third variable (inactive)
bw <- BW2stagePPS(pop$inactive, 
                  pp,
                  psuID=pop$municipality)
bw
des <- clusOpt2(C1=5,
                C2=1,
                delta=bw[6],
                unit.rv=bw[3],
                k=bw[5],
                CV0=0.03,
                tot.cost=NULL,
                cal.sw=2)
des
sample_size <- des$m.opt*des$n.opt
sample_size
results[5,1] <- "inactive"
results[5,2] <- "PractTools"
results[5,3] <- "PSU" 
results[5,4] <- round(des$m.opt)
results[6,1] <- "inactive"
results[6,2] <- "PractTools"
results[6,3] <- "SSU" 
results[6,4] <- round(des$m.opt*des$n.opt)

# Fourth variable (unemployed)
bw <- BW2stagePPS(pop$unemployed, 
                  pp,
                  psuID=pop$municipality)
bw
des <- clusOpt2(C1=350, # whatever value, m.opt always exceed number of municipalities
                C2=1,
                delta=bw[6],
                unit.rv=bw[3],
                k=bw[5],
                CV0=0.05,
                tot.cost=NULL,
                cal.sw=2)
des
sample_size <- des$m.opt*des$n.opt
sample_size
results[7,1] <- "unemployed"
results[7,2] <- "PractTools"
results[7,3] <- "PSU" 
results[7,4] <- round(des$m.opt)
results[8,1] <- "unemployed"
results[8,2] <- "PractTools"
results[8,3] <- "SSU" 
results[8,4] <- round(des$m.opt*des$n.opt)

library(R2BEAT)

# FIRST VARIABLE : income_hh
## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.02)))
## -----------------------------------------------------------
## Prepare inputs for allocation
samp_frame <- pop
samp_frame$one <- 1
id_PSU <- "municipality"  
id_SSU <- "id_ind"        
strata_var <- "one"   
target_vars <- c("income_hh") 
deff_var <- "stratum"     
domain_var <- "one"  
delta =  1       # households = survey units
minimum <- 50    # minimum number of SSUs to be interviewed in each selected PSU
f = 0.05         # suggestion for the sampling fraction 
deff_sugg <- 1.5 # suggestion for the deff value
inp1 <- prepareInputToAllocation1(samp_frame,
                                id_PSU,
                                id_SSU,
                                strata_var,
                                target_vars,
                                deff_var,
                                domain_var,
                                minimum,
                                delta,
                                f,
                                deff_sugg)
inp1$rho
## -----------------------------------------------------------
## Allocation
alloc1 <- beat.2st(stratif = inp1$strata, 
                  errors = cv, 
                  des_file = inp1$des_file, 
                  psu_file = inp1$psu_file, 
                  rho = inp1$rho, 
                  deft_start = NULL, 
                  effst = inp1$effst,
                  epsilon1 = 5, 
                  mmdiff_deft = 1,maxi = 15, 
                  epsilon = 10^(-11), 
                  minnumstrat = 2, 
                  maxiter = 200, 
                  maxiter1 = 25)
results[9,1] <- "income_hh"
results[9,2] <- "R2BEAT"
results[9,3] <- "PSU" 
results[9,4] <- alloc1$iterations[nrow(alloc1$iterations),4]
results[10,1] <- "income_hh"
results[10,2] <- "R2BEAT"
results[10,3] <- "SSU" 
results[10,4] <- alloc1$iterations[nrow(alloc1$iterations),5]

# SECOND VARIABLE : active

## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.03)))
## -----------------------------------------------------------
## Prepare inputs for allocation
samp_frame <- pop
samp_frame$one <- 1
id_PSU <- "municipality"  
id_SSU <- "id_ind"        
strata_var <- "one"   
# target_vars <- c("income_hh","active","inactive","unemployed")   
target_vars <- c("active") 
deff_var <- "stratum"     
domain_var <- "one"  
delta =  1       # households = survey units
minimum <- 50    # minimum number of SSUs to be interviewed in each selected PSU
f = 0.05         # suggestion for the sampling fraction 
deff_sugg <- 1.5 # suggestion for the deff value
inp2 <- prepareInputToAllocation1(samp_frame,
                                 id_PSU,
                                 id_SSU,
                                 strata_var,
                                 target_vars,
                                 deff_var,
                                 domain_var,
                                 minimum,
                                 delta,
                                 f,
                                 deff_sugg)
inp2$rho
## -----------------------------------------------------------
## Allocation
alloc2 <- beat.2st(stratif = inp2$strata, 
                  errors = cv, 
                  des_file = inp2$des_file, 
                  psu_file = inp2$psu_file, 
                  rho = inp2$rho, 
                  deft_start = NULL, 
                  effst = inp2$effst,
                  epsilon1 = 5, 
                  mmdiff_deft = 1,
                  maxi = 15, 
                  epsilon = 10^(-11), 
                  minnumstrat = 2, 
                  maxiter = 200, 
                  maxiter1 = 25)
results[11,1] <- "active"
results[11,2] <- "R2BEAT"
results[11,3] <- "PSU" 
results[11,4] <- alloc2$iterations[nrow(alloc2$iterations),4]
results[12,1] <- "active"
results[12,2] <- "R2BEAT"
results[12,3] <- "SSU" 
results[12,4] <- alloc2$iterations[nrow(alloc2$iterations),5]

## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.03)))
## -----------------------------------------------------------
## Prepare inputs for allocation
samp_frame <- pop
samp_frame$one <- 1
id_PSU <- "municipality"  
id_SSU <- "id_ind"        
strata_var <- "one"   
# target_vars <- c("income_hh","active","inactive","unemployed")   
target_vars <- c("inactive") 
deff_var <- "stratum"     
domain_var <- "one"  
delta =  1       # households = survey units
minimum <- 50    # minimum number of SSUs to be interviewed in each selected PSU
f = 0.05         # suggestion for the sampling fraction 
deff_sugg <- 1.5 # suggestion for the deff value
inp3 <- prepareInputToAllocation1(samp_frame,
                                 id_PSU,
                                 id_SSU,
                                 strata_var,
                                 target_vars,
                                 deff_var,
                                 domain_var,
                                 minimum,
                                 delta,
                                 f,
                                 deff_sugg)
inp3$rho
## -----------------------------------------------------------
## Allocation
alloc3 <- beat.2st(stratif = inp3$strata, 
                  errors = cv, 
                  des_file = inp3$des_file, 
                  psu_file = inp3$psu_file, 
                  rho = inp3$rho, 
                  deft_start = NULL, 
                  effst = inp3$effst,
                  epsilon1 = 5, 
                  mmdiff_deft = 1,maxi = 15, 
                  epsilon = 10^(-11), 
                  minnumstrat = 2, 
                  maxiter = 200, 
                  maxiter1 = 25)
results[13,1] <- "inactive"
results[13,2] <- "R2BEAT"
results[13,3] <- "PSU" 
results[13,4] <- alloc3$iterations[nrow(alloc3$iterations),4]
results[14,1] <- "inactive"
results[14,2] <- "R2BEAT"
results[14,3] <- "SSU" 
results[14,4] <- alloc3$iterations[nrow(alloc3$iterations),5]

## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.05)))
## -----------------------------------------------------------
## Prepare inputs for allocation
samp_frame <- pop
samp_frame$one <- 1
id_PSU <- "municipality"  
id_SSU <- "id_ind"        
strata_var <- "one"   
# target_vars <- c("income_hh","active","inactive","unemployed")   
target_vars <- c("unemployed") 
deff_var <- "stratum"     
domain_var <- "one"  
delta =  1       # households = survey units
minimum <- 50    # minimum number of SSUs to be interviewed in each selected PSU
f = 0.05         # suggestion for the sampling fraction 
deff_sugg <- 1.5 # suggestion for the deff value
inp4 <- prepareInputToAllocation1(samp_frame,
                                 id_PSU,
                                 id_SSU,
                                 strata_var,
                                 target_vars,
                                 deff_var,
                                 domain_var,
                                 minimum,
                                 delta,
                                 f,
                                 deff_sugg)
inp4$rho
## -----------------------------------------------------------
## Allocation
alloc4 <- beat.2st(stratif = inp4$strata, 
                  errors = cv, 
                  des_file = inp4$des_file, 
                  psu_file = inp4$psu_file, 
                  rho = inp4$rho, 
                  deft_start = NULL, 
                  effst = inp4$effst,
                  epsilon1 = 5, 
                  mmdiff_deft = 1,maxi = 15, 
                  epsilon = 10^(-11), 
                  minnumstrat = 2, 
                  maxiter = 200, 
                  maxiter1 = 25)
results[15,1] <- "unemployed"
results[15,2] <- "R2BEAT"
results[15,3] <- "PSU" 
results[15,4] <- alloc4$iterations[nrow(alloc4$iterations),4]
results[16,1] <- "unemployed"
results[16,2] <- "R2BEAT"
results[16,3] <- "SSU" 
results[16,4] <- alloc4$iterations[nrow(alloc4$iterations),5]

library("samplesize4surveys")

pop_strata <- as.numeric(table(pop$stratum))
PSU <- length(unique(pop$municipality))
rho_income_hh <- 0.04875369
rho_active <- 0.06566887
rho_inactive<- 0.002080643
rho_unmployed <- 0.1263824

# First variable (income_hh)
a1 <- ss2s4m(N = nrow(pop), 
       mu = mean(pop$income_hh), 
       sigma = sd(pop$income_hh),
       # conf = 0.95,
       delta = 0.02 * 1.96,
       M = PSU, 
       to = 50, 
       rho = rho_income_hh)
results[17,1] <- "income_hh"
results[17,2] <- "samplesize4surveys"
results[17,3] <- "PSU" 
results[17,4] <- a1[50,2]
results[18,1] <- "income_hh"
results[18,2] <- "samplesize4surveys"
results[18,3] <- "SSU" 
results[18,4] <- a1[50,4]

# Second variable (active)
a2 <- ss2s4p(N = nrow(pop), 
       P = as.numeric(table(pop$active))[2]/nrow(pop), 
       # conf = 0.95, 
       delta = 0.03 * 1.96, 
       M = PSU, 
       to = 50, 
       rho = rho_active)
results[19,1] <- "active"
results[19,2] <- "samplesize4surveys"
results[19,3] <- "PSU" 
results[19,4] <- a2[50,2]
results[20,1] <- "active"
results[20,2] <- "samplesize4surveys"
results[20,3] <- "SSU" 
results[20,4] <- a2[50,4]

a3 <- ss2s4p(N = nrow(pop), 
       P = as.numeric(table(pop$inactive))[2]/nrow(pop), 
       # conf = 0.95, 
       delta = 0.03 * 1.96, 
       M = PSU, 
       to = 50, 
       rho = rho_inactive)
results[21,1] <- "inactive"
results[21,2] <- "samplesize4surveys"
results[21,3] <- "PSU" 
results[21,4] <- a3[50,2]
results[22,1] <- "inactive"
results[22,2] <- "samplesize4surveys"
results[22,3] <- "SSU" 
results[22,4] <- a3[50,4]

# Fourth variable (unemployed)
a4 <- ss2s4p(N = nrow(pop), 
       P = as.numeric(table(pop$unemployed))[2]/nrow(pop), 
       # conf = 0.95, 
       delta = 0.05 * 1.96, 
       M = PSU, 
       to = 50, 
       rho = rho_unmployed)
results[23,1] <- "unemployed"
results[23,2] <- "samplesize4surveys"
results[23,3] <- "PSU" 
results[23,4] <- a4[50,2]
results[24,1] <- "unemployed"
results[24,2] <- "samplesize4surveys"
results[24,3] <- "SSU" 
results[24,4] <- a4[50,4]

library(ggplot2)

p <- ggplot(data=results[results$Unit=="PSU",], aes(x=Variable, y=Units, fill=Package)) +
   geom_bar(stat="identity", position=position_dodge(),)
p + labs(title="PSUs by variable and package", 
         x="Variable", y = "PSUs") 
p <- ggplot(data=results[results$Unit=="SSU",], aes(x=Variable, y=Units, fill=Package)) +
   geom_bar(stat="identity", position=position_dodge(),)
p + labs(title="SSUs by variable and package", 
         x="Variable", y = "SSUs") 
