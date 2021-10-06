load("pop.RData")
length(unique(pop$municipality))
# [1] 513
library(PracTools)

# Probabilities of inclusion I stage
pp <- as.numeric(table(pop$municipality))/nrow(pop)

# pp <- NULL
# for (i in (1:length(unique(pop$stratum)))) {
#   pp1 <- as.numeric(table(pop$municipality[pop$stratum==levels(pop$stratum)[i]]))/ nrow(pop[pop$stratum==levels(pop$stratum)[i],])
#   pp <- c(pp,pp1)
# }
# sum(pp)
# pp <- pp/24

# First variable (income_hh)
bw <- BW2stagePPS(pop$income_hh, 
      pp,
      psuID=pop$municipality)
bw
#         B2          W2 unit relvar       B2+W2           k       delta 
# 0.04075893  0.79538674  0.83601766  0.83614567  1.00015312  0.04874621 

des <- clusOpt2(C1=130,
               C2=1,
               delta=bw[6],
               unit.rv=bw[3],
               k=bw[5],
               CV0=0.02,
               tot.cost=NULL,
               cal.sw=2)
des
# C1 = 130
# C2 = 1
# delta = 0.04874621
# unit relvar = 0.8360177
# k = 1.000153
# cost = 25499.72
# m.opt = 141.4
# n.opt = 50.4
# CV = 0.02
sample_size <- des$m.opt*des$n.opt
sample_size
# unit relvar 
# 7126.56

# Second variable (active)
table(pop$active)
bw <- BW2stagePPS(pop$active, 
                  pp,
                  psuID=pop$municipality)
bw
#         B2          W2 unit relvar       B2+W2           k       delta 
# 0.03416003  0.48613299  0.52018630  0.52029301  1.00020515  0.06565537 
des <- clusOpt2(C1=180,
                C2=1,
                delta=bw[6],
                unit.rv=bw[3],
                k=bw[5],
                CV0=0.03,
                tot.cost=NULL,
                cal.sw=2)
des
# C1 = 180
# C2 = 1
# delta = 0.06565537
# unit relvar = 0.5201863
# k = 1.000205
# cost = 11214.18
# m.opt = 48.6
# n.opt = 50.6
# CV = 0.03
sample_size <- des$m.opt*des$n.opt
sample_size
# 2459.16 

# Third variable (inactive)
table(pop$inactive)
bw <- BW2stagePPS(pop$inactive, 
                  pp,
                  psuID=pop$municipality)
bw
#          B2          W2 unit relvar       B2+W2           k       delta 
# 0.007475925 3.586427402 3.593086259 3.593903327 1.000227400 0.002080169 
des <- clusOpt2(C1=5,
                C2=1,
                delta=bw[6],
                unit.rv=bw[3],
                k=bw[5],
                CV0=0.03,
                tot.cost=NULL,
                cal.sw=2)
des
# C1 = 5
# C2 = 1
# delta = 0.002080169
# unit relvar = 3.593086
# k = 1.000227
# cost = 4840.099
# m.opt = 89.7
# n.opt = 49
# CV = 0.03
sample_size <- des$m.opt*des$n.opt
sample_size
# 4395.3 

# Fourth variable (unemployed)
table(pop$unemployed)
bw <- BW2stagePPS(pop$unemployed, 
                  pp,
                  psuID=pop$municipality)
bw
#       B2          W2  unit relvar       B2+W2           k       delta 
# 0.8890053   6.1465906   7.0342495   7.0355959   1.0001914   0.1263582  
des <- clusOpt2(C1=350, # whatever value, m.opt always exceed number of municipalities
                C2=1,
                delta=bw[6],
                unit.rv=bw[3],
                k=bw[5],
                CV0=0.05,
                tot.cost=NULL,
                cal.sw=2)
des
# C1 = 350
# C2 = 1
# delta = 0.1263582
# unit relvar = 7.034249
# k = 1.000191
# cost = 161905.3
# m.opt = 405.6
# n.opt = 49.2
# CV = 0.05
sample_size <- des$m.opt*des$n.opt
sample_size
# 19955.52
