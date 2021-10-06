#----------------------------
# Workflow example Scenario 1
# (univariate)
#----------------------------

# Install last version of R2BEAT
#devtools::install_github("barcaroli/R2BEAT)
library(R2BEAT)

## -----------------------------------------------------------
## Sampling frame
load("pop.RData")

## -----------------------------------------------------------
# FIRST VARIABLE : income_hh

## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.02)))
cv

## -----------------------------------------------------------
## Prepare inputs for allocation
samp_frame <- pop
samp_frame$one <- 1
id_PSU <- "municipality"  
id_SSU <- "id_ind"        
strata_var <- "one"   
# target_vars <- c("income_hh","active","inactive","unemployed")   
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
# STRATUM   RHO_AR1   RHO_NAR1
# 1       1       1 0.04875369

# inp$strata$DOM2 <- NULL
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
#   iterations PSU_SR PSU NSR PSU Total  SSU
# 1          0      0       0         0 2089
# 2          1      6      31        37 5835
# 3          2     14      74        88 5257
# 4          3     13      68        81 5301

## Simulation
allocat1 <- alloc1$alloc[-nrow(alloc1$alloc),]
set.seed(1234)
sample_2st <- StratSel(dataPop = inp1$psu_file,
                       idpsu = ~ PSU_ID, 
                       dom= ~ STRATUM, 
                       final_pop = ~ PSU_MOS, 
                       size = ~ PSU_MOS, 
                       PSUsamplestratum = 1, 
                       min_sample = minimum, 
                       min_sample_index = FALSE, 
                       dataAll = allocat1,
                       domAll = ~ factor(STRATUM), 
                       f_sample = ~ ALLOC, 
                       planned_min_sample = NULL, 
                       launch = F)
sample_2st[[2]]
as.numeric(sample_2st[[2]]$SRdom[2])+as.numeric(sample_2st[[2]]$nSRdom[2])
as.numeric(sample_2st[[2]]$SR_PSU_final_sample_unit[2])+as.numeric(sample_2st[[2]]$NSR_PSU_final_sample_unit[2])

selected_PSU <- sample_2st[[4]]
df=pop
df$one <- 1
PSU_code="municipality"
SSU_code="id_ind"
PSU_sampled=selected_PSU[selected_PSU$Sampled_PSU==1,]
target_vars <- c("income_hh")  
PSU_sampled <- selected_PSU[selected_PSU$PSU_final_sample_unit > 0,]
# Domain level = national
domain_var <- "one"
set.seed(1234)
eval1 <- eval_2stage(df,
                    PSU_code,
                    SSU_code,
                    domain_var,
                    target_vars,
                    PSU_sampled,
                    nsampl=50, 
                    writeFiles=FALSE,
                    progress=TRUE) 
eval1$coeff_var
#      CV1  dom
# 1 0.0131 DOM1


## -----------------------------------------------------------
# SECOND VARIABLE : active

## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.03)))
cv

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
# STRATUM   RHO_AR1   RHO_NAR1
# 1       1       1 0.06566887
# inp$strata$DOM2 <- NULL
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
#   iterations PSU_SR PSU NSR PSU Total  SSU
# 1          0      0       0         0  578
# 2          1      0      12        12 2436
# 3          2      7      36        43 1936
# 4          3      5      30        35 2023
## Simulation
allocat2 <- alloc2$alloc[-nrow(alloc2$alloc),]
set.seed(1234)
sample_2st <- StratSel(dataPop = inp2$psu_file,
                       idpsu = ~ PSU_ID, 
                       dom= ~ STRATUM, 
                       final_pop = ~ PSU_MOS, 
                       size = ~ PSU_MOS, 
                       PSUsamplestratum = 1, 
                       min_sample = minimum, 
                       min_sample_index = FALSE, 
                       dataAll = allocat2,
                       domAll = ~ factor(STRATUM), 
                       f_sample = ~ ALLOC, 
                       planned_min_sample = NULL, 
                       launch = F)
sample_2st[[2]]
as.numeric(sample_2st[[2]]$SRdom[2])+as.numeric(sample_2st[[2]]$nSRdom[2])
# [1] 36
as.numeric(sample_2st[[2]]$SR_PSU_final_sample_unit[2])+as.numeric(sample_2st[[2]]$NSR_PSU_final_sample_unit[2])
# [1] 2025



selected_PSU <- sample_2st[[4]]
df=pop
df$one <- 1
PSU_code="municipality"
SSU_code="id_ind"
PSU_sampled=selected_PSU[selected_PSU$Sampled_PSU==1,]
target_vars <- c("active")  
PSU_sampled <- selected_PSU[selected_PSU$PSU_final_sample_unit > 0,]
# Domain level = national
domain_var <- "one"
set.seed(1234)
eval2 <- eval_2stage(df,
                    PSU_code,
                    SSU_code,
                    domain_var,
                    target_vars,
                    PSU_sampled,
                    nsampl=50, 
                    writeFiles=FALSE,
                    progress=TRUE) 
eval2$coeff_var
#      CV1  dom
# 1 0.0147 DOM1

## -----------------------------------------------------------
# THIRD VARIABLE : inactive

## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.03)))
cv

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
#   STRATUM RHO_AR1    RHO_NAR1
# 1       1       1 0.002080643
# inp$strata$DOM2 <- NULL
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
#   iterations PSU_SR PSU NSR PSU Total  SSU
# 1          0      0       0         0 3986
# 2          1     12      52        64 4252
## Simulation
allocat3 <- alloc3$alloc[-nrow(alloc3$alloc),]
set.seed(1234)
sample_2st <- StratSel(dataPop = inp3$psu_file,
                       idpsu = ~ PSU_ID, 
                       dom= ~ STRATUM, 
                       final_pop = ~ PSU_MOS, 
                       size = ~ PSU_MOS, 
                       PSUsamplestratum = 1, 
                       min_sample = minimum, 
                       min_sample_index = FALSE, 
                       dataAll = allocat3,
                       domAll = ~ factor(STRATUM), 
                       f_sample = ~ ALLOC, 
                       planned_min_sample = NULL, 
                       launch = F)
sample_2st[[2]]
as.numeric(sample_2st[[2]]$SRdom[2])+as.numeric(sample_2st[[2]]$nSRdom[2])
# [1] 68
as.numeric(sample_2st[[2]]$SR_PSU_final_sample_unit[2])+as.numeric(sample_2st[[2]]$NSR_PSU_final_sample_unit[2])
# [1] 4251

selected_PSU <- sample_2st[[4]]
df=pop
df$one <- 1
PSU_code="municipality"
SSU_code="id_ind"
PSU_sampled=selected_PSU[selected_PSU$Sampled_PSU==1,]
target_vars <- c("inactive")  
PSU_sampled <- selected_PSU[selected_PSU$PSU_final_sample_unit > 0,]
# Domain level = national
domain_var <- "one"
set.seed(1234)
eval3 <- eval_2stage(df,
                    PSU_code,
                    SSU_code,
                    domain_var,
                    target_vars,
                    PSU_sampled,
                    nsampl=50, 
                    writeFiles=FALSE,
                    progress=TRUE) 
eval3$coeff_var
#      CV1  dom
# 1 0.0255 DOM1
## -----------------------------------------------------------
# FOURTH VARIABLE : unemployed

## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.05)))
cv

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
#   STRATUM RHO_AR1  RHO_NAR1
# 1       1       1 0.1263824
# inp$strata$DOM2 <- NULL
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
#   iterations PSU_SR PSU NSR PSU Total   SSU
# 1          0      0       0         0  2811
# 2          1      7      41        48 15442
# 3          2     50     133       183 10252
# 4          3     36     100       136 11243
# 5          4     42     103       145 10752

## Simulation
allocat4 <- alloc4$alloc[-nrow(alloc4$alloc),]
set.seed(1234)
sample_2st <- StratSel(dataPop = inp4$psu_file,
                       idpsu = ~ PSU_ID, 
                       dom= ~ STRATUM, 
                       final_pop = ~ PSU_MOS, 
                       size = ~ PSU_MOS, 
                       PSUsamplestratum = 1, 
                       min_sample = minimum, 
                       min_sample_index = FALSE, 
                       dataAll = allocat4,
                       domAll = ~ factor(STRATUM), 
                       f_sample = ~ ALLOC, 
                       planned_min_sample = NULL, 
                       launch = F)
sample_2st[[2]]
as.numeric(sample_2st[[2]]$SRdom[2])+as.numeric(sample_2st[[2]]$nSRdom[2])
# [1] 141
as.numeric(sample_2st[[2]]$SR_PSU_final_sample_unit[2])+as.numeric(sample_2st[[2]]$NSR_PSU_final_sample_unit[2])
# [1] 10753
selected_PSU <- sample_2st[[4]]
df=pop
df$one <- 1
PSU_code="municipality"
SSU_code="id_ind"
PSU_sampled=selected_PSU[selected_PSU$Sampled_PSU==1,]
target_vars <- c("unemployed")  
PSU_sampled <- selected_PSU[selected_PSU$PSU_final_sample_unit > 0,]
# Domain level = national
domain_var <- "one"
set.seed(1234)
eval4 <- eval_2stage(df,
                    PSU_code,
                    SSU_code,
                    domain_var,
                    target_vars,
                    PSU_sampled,
                    nsampl=50, 
                    writeFiles=FALSE,
                    progress=TRUE) 
eval4$coeff_var
#     CV1  dom
# 1 0.026 DOM1

save.image("Scenario1_univariate.RData")
