### Data set up ###
setwd("")
clpop <- read_csv("nlsy_gcomp_clpop.csv")

##### data notes #####
# MH_binary is binary MHI-5, MH_Scale is continuous MHI-5
# arrest, charge, incar, probation are first adult criminal legal sentences
# age is age prior to first adult criminal legal encounter
# race_black, race_hispanic, race_white, race_other are race/ethnicity 
# assault is having an assault conviction at first adult criminal legal encounter
# sex is sex collected at baseline
# parent_edu is parental education collected at baseline
# cl_juv is indicator for having had a juvenile criminal legal encounter
# edu is educational attainment prior to first adult criminal legal encounter
# emp is employment prior to first adult criminal legal encounter
# marital is marital status prior to first adult criminal legal encounter
# child is having children prior to first adult criminal legal encounter
# health is self-rated health prior to first adult criminal legal encounter
# drug is reported drug use prior to first adult criminal legal encounter
# stressor is reporting stressful life events 
# region_neast, region_ncentral, region_west, region_south are region lived in prior to first adult criminal legal encounter

# BINARY OUTCOME 
#### calculate point estimates ####
### (0 of 2) natural course ###
# regression 
reg_full <- glm(MH_binary ~ arrest + charge + incar + age + 
                  race_black + race_hispanic + arrest:race_black + arrest:race_hispanic +
                  charge:race_black + charge:race_hispanic + incar:race_black + incar:race_hispanic +
                  assault + sex + parent_edu + cl_juv + edu + emp + marital + child + 
                  health + drug + stressor + region_neast + region_ncentral + region_west, data = clpop[!is.na(clpop$MH_binary)&clpop$race_other!=1,], family = binomial(link="logit"))
# predicted Y, natural course 
risk_nc_full <- predict(reg_full, newdata=clpop, weights = baseline_wt, type="response")
mean(risk_nc_full)

### (1 of 2) intervention incarceration -> probation ###
# generate data sets with incarceration set to 0 -> gives everyone probation that was incarcerated
noincar_full <- clpop %>% mutate(probation = ifelse(incar==1,1,probation), incar = 0)
# Create predicted outcome setting incarceration to 0
risk1_full <- predict(reg_full, newdata=noincar_full, weights = baseline_wt, type="response")
# calculate mean differences
diff1_all_full <- mean(risk1_full - risk_nc_full)
diff1_full_black <- mean(risk1_full[clpop$race_black==1] - risk_nc_full[clpop$race_black==1])
diff1_full_white <- mean(risk1_full[clpop$race_white==1] - risk_nc_full[clpop$race_white==1])
diff1_sub_full <- mean(risk1_full[clpop$incar==1] - risk_nc_full[clpop$incar==1])
diff1_sub_black <- mean(risk1_full[clpop$incar==1 & clpop$race_black==1] - risk_nc_full[clpop$incar==1 & clpop$race_black==1])
diff1_sub_white <- mean(risk1_full[clpop$incar==1 & clpop$race_white==1] - risk_nc_full[clpop$incar==1 & clpop$race_white==1])

### (2 of 2) intervention probation -> charge ###
nopr_full <- clpop %>% mutate(charge = ifelse(probation==1, 1, charge),probation = 0)
# predict
risk2_full <- predict(reg_full, newdata=nopr_full, weights = baseline_wt, type="response")
# calculate mean difference
diff2_all_full <- mean(risk2_full - risk_nc_full)
diff2_full_black <- mean(risk2_full[clpop$race_black==1] - risk_nc_full[clpop$race_black==1])
diff2_full_white <- mean(risk2_full[clpop$race_white==1] - risk_nc_full[clpop$race_white==1])
diff2_sub_full <- mean(risk2_full[clpop$probation==1] - risk_nc_full[clpop$probation==1])
diff2_sub_black <- mean(risk2_full[clpop$probation==1 & clpop$race_black==1] - risk_nc_full[clpop$probation==1 & clpop$race_black==1])
diff2_sub_white <- mean(risk2_full[clpop$probation==1 & clpop$race_white==1] - risk_nc_full[clpop$probation==1 & clpop$race_white==1])
# save point estimates
Intervention <- tibble(mean(risk_nc_full), mean(risk1_full), diff1_all_full, diff1_full_black, diff1_full_white, diff1_sub_full, diff1_sub_black, diff1_sub_white,
                       mean(risk2_full), diff2_all_full, diff2_full_black, diff2_full_white, diff2_sub_full, diff2_sub_black, diff2_sub_white)

#### Bootstrap #### 
### (0 of 2) natural course ###
my_gcomp <- function(gcomp_data){
# regression 
reg_full <- glm(MH_binary ~ arrest + charge + incar + age + 
                  race_black + race_hispanic + arrest:race_black + arrest:race_hispanic +
                  charge:race_black + charge:race_hispanic + incar:race_black + incar:race_hispanic +
                  assault + sex + parent_edu + cl_juv + edu + emp + marital + child + 
                  health + drug + stressor + region_neast + region_ncentral + region_west, data = gcomp_data[!is.na(gcomp_data$MH_binary)&gcomp_data$race_other!=1,], family = binomial(link="logit"))
# create predicted Y, natural course
risk_nc_full <- predict(reg_full, newdata=gcomp_data, weights = baseline_wt, type="response")

### (1 of 2) intervention incarceration -> probation ###
# generate data sets with incarceration set to 0 -> gives everyone probation that was incarcerated
noincar_full <- gcomp_data %>% mutate(probation = ifelse(incar==1,1, probation), incar = 0)
# Create predicted outcome setting incarceration to 0
risk1_full <- predict(reg_full, newdata=noincar_full, weights = baseline_wt, type="response")
# calculate mean difference
diff1_all_full <- mean(risk1_full - risk_nc_full)
diff1_full_black <- mean(risk1_full[gcomp_data$race_black==1] - risk_nc_full[gcomp_data$race_black==1])
diff1_full_white <- mean(risk1_full[gcomp_data$race_white==1] - risk_nc_full[gcomp_data$race_white==1])
diff1_sub_full <- mean(risk1_full[gcomp_data$incar==1] - risk_nc_full[gcomp_data$incar==1])
diff1_sub_black <- mean(risk1_full[gcomp_data$incar==1 & gcomp_data$race_black==1] - risk_nc_full[gcomp_data$incar==1 & gcomp_data$race_black==1])
diff1_sub_white <- mean(risk1_full[gcomp_data$incar==1 & gcomp_data$race_white==1] - risk_nc_full[gcomp_data$incar==1 & gcomp_data$race_white==1])

### (2 of 2) intervention probation -> charge for those w/ CL involvement ###
nopr_full <- gcomp_data %>% mutate(charge = ifelse(probation==1, 1, charge),probation = 0)
# predict
risk2_full <- predict(reg_full, newdata=nopr_full, weights = baseline_wt, type="response")
# calculate mean difference
diff2_all_full <- mean(risk2_full - risk_nc_full)
diff2_full_black <- mean(risk2_full[gcomp_data$race_black==1] - risk_nc_full[gcomp_data$race_black==1])
diff2_full_white <- mean(risk2_full[gcomp_data$race_white==1] - risk_nc_full[gcomp_data$race_white==1])
diff2_sub_full <- mean(risk2_full[gcomp_data$probation==1] - risk_nc_full[gcomp_data$probation==1])
diff2_sub_black <- mean(risk2_full[gcomp_data$probation==1 & gcomp_data$race_black==1] - risk_nc_full[gcomp_data$probation==1 & gcomp_data$race_black==1])
diff2_sub_white <- mean(risk2_full[gcomp_data$probation==1 & gcomp_data$race_white==1] - risk_nc_full[gcomp_data$probation==1 & gcomp_data$race_white==1])
# save 
Intervention <- tibble(mean(risk_nc_full), mean(risk1_full), diff1_all_full, diff1_full_black, diff1_full_white, diff1_sub_full, diff1_sub_black, diff1_sub_white,
                       mean(risk2_full), diff2_all_full, diff2_full_black, diff2_full_white, diff2_sub_full, diff2_sub_black, diff2_sub_white)
return(Intervention)
}

# get n rows
n_ = nrow(clpop)
# Set number of bootstrap samples
B <- 500
# bootstrap
boot=tibble(rep = 1:B, data=replicate(n=B, expr=clpop[sample(1:n_, n_, replace=TRUE), ],simplify=FALSE)) %>%
  mutate(estimate=lapply(data, my_gcomp))
# get SE of MCC across bootstrapped samples               
boot_gcomp <- boot %>% select(-data) %>% unnest(estimate) %>% summarise_if(is.numeric, sd)
boot_gcomp <- boot_gcomp %>% select(-c(rep))
colnames(boot_gcomp) <- paste(colnames(boot_gcomp), "SE", sep = "_")

# join bootstrap SE with original estimates to calculate confidence intervals
boot_se <-merge(x=Intervention,y=boot_gcomp,all.x=TRUE)
uci_data <- (boot_se[,1:15] + (1.96*boot_se[,16:30]))
uci_data$type = "UCI"
lci_data <- (boot_se[,1:15] - (1.96*boot_se[,16:30]))
lci_data$type = "LCI"
Intervention$type = "EST"
Binary_Bootstrap <- rbind(Intervention,lci_data,uci_data)

# write out file
write_csv(Binary_Bootstrap, "Binary_5.25.23.csv")

# SCALED OUTCOME
#### calculate point estimates ####
### (0 of 2) natural course ###
# regression 
reg_scale_full <- glm(MH_scale ~ arrest + charge + incar + age + 
                        race_black + race_hispanic + arrest:race_black + arrest:race_hispanic +
                        charge:race_black + charge:race_hispanic + incar:race_black + incar:race_hispanic +
                        assault + sex + parent_edu + cl_juv + edu + emp + marital + child + 
                        health + drug + stressor + region_neast + region_ncentral + region_west, data = clpop[!is.na(clpop$MH_scale)&clpop$race_other!=1,,], family = gaussian)
# create predicted Y, natural course
risk_nc_full <- predict(reg_scale_full, newdata=clpop, weights=baseline_wt)

### (1 of 2) intervention incarceration -> probation for those w/ CL involvement ###
# generate data sets with incarceration set to 0 -> gives everyone probation that was incarcerated
noincar_full <- clpop %>% mutate(probation = ifelse(incar==1, 1, probation), incar = 0)
# Create predicted outcome setting incarceration to 0
risk1_full <- predict(reg_scale_full, newdata=noincar_full)
# calculate mean difference
diff1_all_full <- mean(risk1_full - risk_nc_full)
diff1_full_black <- mean(risk1_full[clpop$race_black==1] - risk_nc_full[clpop$race_black==1])
diff1_full_white <- mean(risk1_full[clpop$race_white==1] - risk_nc_full[clpop$race_white==1])
diff1_sub_full <- mean(risk1_full[clpop$incar==1] - risk_nc_full[clpop$incar==1])
diff1_sub_black <- mean(risk1_full[clpop$incar==1 & clpop$race_black==1] - risk_nc_full[clpop$incar==1 & clpop$race_black==1])
diff1_sub_white <- mean(risk1_full[clpop$incar==1 & clpop$race_white==1] - risk_nc_full[clpop$incar==1 & clpop$race_white==1])

### (2 of 2) intervention probation -> charge for those w/ CL involvement ###
nopr_full <- clpop %>% mutate(charge = ifelse(probation==1, 1, charge),probation = 0)
# predict
risk2_full <- predict(reg_scale_full, newdata=nopr_full)
# calculate mean difference
diff2_all_full <- mean(risk2_full - risk_nc_full)
diff2_full_black <- mean(risk2_full[clpop$race_black==1] - risk_nc_full[clpop$race_black==1])
diff2_full_white <- mean(risk2_full[clpop$race_white==1] - risk_nc_full[clpop$race_white==1])

diff2_sub_full <- mean(risk2_full[clpop$probation==1] - risk_nc_full[clpop$probation==1])
diff2_sub_black <- mean(risk2_full[clpop$probation==1 & clpop$race_black==1] - risk_nc_full[clpop$probation==1 & clpop$race_black==1])
diff2_sub_white <- mean(risk2_full[clpop$probation==1 & clpop$race_white==1] - risk_nc_full[clpop$probation==1 & clpop$race_white==1])
# save
Intervention_Scale_Full <- tibble(mean(risk_nc_full), mean(risk1_full), diff1_all_full, diff1_full_black, diff1_full_white, diff1_sub_full, diff1_sub_black, diff1_sub_white,
                                  mean(risk2_full), diff2_all_full, diff2_full_black, diff2_full_white, diff2_sub_full, diff2_sub_black, diff2_sub_white)

#### Bootstrap #### 
my_gcomp_scale <- function(gcomp_data_scale){
  
### MH scale ###
reg_scale_full <- glm(MH_scale ~ arrest + charge + incar + age + 
                        race_black + race_hispanic + arrest:race_black + arrest:race_hispanic +
                        charge:race_black + charge:race_hispanic + incar:race_black + incar:race_hispanic +
                        assault + sex + parent_edu + cl_juv + edu + emp + marital + child + 
                        health + drug + stressor + region_neast + region_ncentral + region_west, data = gcomp_data_scale[!is.na(gcomp_data_scale$MH_scale)&gcomp_data_scale$race_other!=1,], family = gaussian)

# create predicted Y, natural course
risk_nc_full <- predict(reg_scale_full, newdata=gcomp_data_scale, weights=baseline_wt)
mean(risk_nc_full)
  
### (1 of 2) intervention incarceration -> probation for those w/ CL involvement ###
# generate data sets with incarceration set to 0 -> gives everyone probation that was incarcerated
noincar_full <- gcomp_data_scale %>% mutate(probation = ifelse(incar==1,1, probation), incar = 0)
  
# Create predicted outcome setting incarceration to 0
risk1_full <- predict(reg_scale_full, newdata=noincar_full)
  
# calculate mean difference
diff1_all_full <- mean(risk1_full - risk_nc_full)
diff1_full_black <- mean(risk1_full[gcomp_data_scale$race_black==1] - risk_nc_full[gcomp_data_scale$race_black==1])
diff1_full_white <- mean(risk1_full[gcomp_data_scale$race_white==1] - risk_nc_full[gcomp_data_scale$race_white==1])
diff1_sub_full <- mean(risk1_full[gcomp_data_scale$incar==1] - risk_nc_full[gcomp_data_scale$incar==1])
diff1_sub_black <- mean(risk1_full[gcomp_data_scale$incar==1 & clpop$race_black==1] - risk_nc_full[gcomp_data_scale$incar==1 & gcomp_data_scale$race_black==1])
diff1_sub_white <- mean(risk1_full[gcomp_data_scale$incar==1 & clpop$race_white==1] - risk_nc_full[gcomp_data_scale$incar==1 & gcomp_data_scale$race_white==1])
  
### (2 of 2) intervention probation -> charge for those w/ CL involvement ###
nopr_full <- gcomp_data_scale %>% mutate(charge = ifelse(probation==1, 1, charge), probation = 0)
# predict
risk2_full <- predict(reg_scale_full, newdata=nopr_full)
# calculate mean difference
diff2_all_full <- mean(risk2_full - risk_nc_full)
diff2_full_black <- mean(risk2_full[gcomp_data_scale$race_black==1] - risk_nc_full[gcomp_data_scale$race_black==1])
diff2_full_white <- mean(risk2_full[gcomp_data_scale$race_white==1] - risk_nc_full[gcomp_data_scale$race_white==1])
  
diff2_sub_full <- mean(risk2_full[gcomp_data_scale$probation==1] - risk_nc_full[gcomp_data_scale$probation==1])
diff2_sub_black <- mean(risk2_full[gcomp_data_scale$probation==1 & gcomp_data_scale$race_black==1] - risk_nc_full[gcomp_data_scale$probation==1 & gcomp_data_scale$race_black==1])
diff2_sub_white <- mean(risk2_full[gcomp_data_scale$probation==1 & gcomp_data_scale$race_white==1] - risk_nc_full[gcomp_data_scale$probation==1 & gcomp_data_scale$race_white==1])

Intervention_Scale_Full <- tibble(mean(risk_nc_full), mean(risk1_full), diff1_all_full, diff1_full_black, diff1_full_white, diff1_sub_full, diff1_sub_black, diff1_sub_white,
                                    mean(risk2_full), diff2_all_full, diff2_full_black, diff2_full_white, diff2_sub_full, diff2_sub_black, diff2_sub_white)
                                    
return(Intervention_Scale_Full)
  
}

# get n rows
n_ = nrow(clpop)
# Set number of bootstrap samples
B <- 500
# bootstrap
boot_scale=tibble(rep = 1:B, data=replicate(n=B, expr=clpop[sample(1:n_, n_, replace=TRUE), ],simplify=FALSE)) %>%
  mutate(estimate=lapply(data, my_gcomp_scale))
# get SE of MCC across bootstrapped samples               
boot_scale_gcomp <- boot_scale %>% select(-data) %>% unnest(estimate) %>% summarise_if(is.numeric, sd)
boot_scale_gcomp <- boot_scale_gcomp %>% select(-c(rep))
colnames(boot_scale_gcomp) <- paste(colnames(boot_scale_gcomp), "SE", sep = "_")
# join bootstrap SE with original estimates to calculate confidence intervals
boot_scale_se <-merge(x=Intervention_Scale_Full,y=boot_scale_gcomp,all.x=TRUE)
uci_data <- (boot_scale_se[,1:15] + (1.96*boot_scale_se[,16:30]))
uci_data$type = "UCI"
lci_data <- (boot_scale_se[,1:15] - (1.96*boot_scale_se[,16:30]))
lci_data$type = "LCI"
Intervention_Scale_Full$type = "EST"
Scale_Bootstrap <- rbind(Intervention_Scale_Full,lci_data, uci_data)

# write out files
write_csv(Scale_Bootstrap, "Scale_5.25.23.csv")


