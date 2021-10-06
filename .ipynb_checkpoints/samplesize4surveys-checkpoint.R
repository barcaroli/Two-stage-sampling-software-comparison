library(samplesize4surveys)

load("pop.RData")
PSU <- length(unique(pop$municipality))
pop_strata <- as.numeric(table(pop$stratum))

# Values taken from Scenario1_univariate.R
rho_income_hh <- 0.04875369
rho_active <- 0.06566887
rho_inactive<- 0.002080643
rho_unmployed <- 0.1263824

#------------------
# Error margin = delta : margin of error
# delta = CV / z


# First variable (income_hh)
ss2s4m(N = nrow(pop), 
       mu = mean(pop$income_hh), 
       sigma = sd(pop$income_hh),
       # conf = 0.95,
       delta = 0.02 * 1.96,
       M = PSU, 
       to = 50, 
       rho = rho_income_hh)
# 50 3.388931 142 50 7061

# Second variable (active)
ss2s4p(N = nrow(pop), 
       P = as.numeric(table(pop$active))[2]/nrow(pop), 
       # conf = 0.95, 
       delta = 0.03 * 1.96, 
       M = PSU, 
       to = 50, 
       rho = rho_active)
# 50 4.217775  49 50 2436

# Third variable (inactive)
ss2s4p(N = nrow(pop), 
       P = as.numeric(table(pop$inactive))[2]/nrow(pop), 
       # conf = 0.95, 
       delta = 0.03 * 1.96, 
       M = PSU, 
       to = 50, 
       rho = rho_inactive)
# 50 1.101952  88 50 4391

# Fourth variable (unemployed)
ss2s4p(N = nrow(pop), 
       P = as.numeric(table(pop$unemployed))[2]/nrow(pop), 
       # conf = 0.95, 
       delta = 0.05 * 1.96, 
       M = PSU, 
       to = 50, 
       rho = rho_unmployed)
# 50 7.192738 402 50 20058

