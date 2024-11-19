## ---------------------------
##
## Script name: sample_synth_data.R
##
## Purpose of script: Create sample of synthetic dataset for use in documentation
##                    and anonymization training in Perugia
## Author: Thijs Benschop
##
## Date Created: 2023-04-06
## Date updated: 2023-05-05 - recalibrate weights within strata
##                          - add variable labels
## ---------------------------
##
## Notes:
##   - Draw stratified sample of 8,000 households
##   - Two-stage sample: first stage stratified by geo1 and urbrur, second stage fixed number of households at random
##   - Input:  Synthetic household and individual census datasets 
##   - Output: Household and individual level datasets
## ---------------------------

rm(list = ls())

# Set wd
setwd("C:/Users/DOC_ANO_TRAINING/Datasets")

## Load packages
library(haven)
library(data.table)
library(sampling)
library(dplyr)
#library(reldist)
#library(survey)

#### Function for 2-stage sampling, first stage stratified, second stage fixed number n2  ####
# size      - sample size to be drawn (if size in [0,100] -> size is p %, if size > 100, size is n)
# ea_var    - variable specifying units to sample in stage 1 
# strat_var - variable for stratifying first stage (needs to be 1 variable) - sample is drawn proportionally 
# n2        - number of households sampled in each ea
# dat       - data.table with variable hid  
# seed      - seed for random number generation to replicate samples

# for testing:
# size = 8000
# ea_var = "ea" 
# strat_var = "stratvar" # stratify by both urbrur and geo1
# n2 = 25
# dat = synth_data_hh 
# seed = 123

two_stage_sample <- function(size, ea_var, strat_var, n2, dat, seed = 123){
  set.seed(seed) # see for replicability
  
  # Calculate number of ea to be sampled based on size and n2
  if(size <= 0){ 
    break("Size cannot be negative or 0")
  }else if (size <= 100){
    size <- round(size * nrow(dat))
  }
  number_of_ea <- round(size / n2) # note that size should be a multiple of n2 to have exact results
  
  # List of eas
  dat_ea <- dat[, c(ea_var, strat_var), with = FALSE]
  dat_ea <- subset(unique(dat_ea)) # all records within same ea have no variation in strat_var
  
  # Size of sample per strata (proportional to size of strata)
  dat_ea <- dat_ea[order(dat_ea[, strat_var, with=FALSE])] # order list of eas by strata
  number_of_ea_by_strata <- round(number_of_ea * as.numeric(table(dat[, strat_var, with=FALSE]))/nrow(dat))
  
  # Correction: if total in number_of_ea_by_strata doesn't add up to number_of_ea due to rounding
  # add/substract from largest strata difference between sum(number_of_ea_by_strata) and number_of_ea
  if(sum(number_of_ea_by_strata) != number_of_ea){
    pos_to_update <- which(number_of_ea_by_strata == max(number_of_ea_by_strata))[1] # first of largest strata
    number_of_ea_by_strata[1] <- number_of_ea_by_strata[1] + (number_of_ea - sum(number_of_ea_by_strata))
  }
  
  # Sample eas
  sample_1 <- sampling::strata(data = dat_ea, 
                     stratanames = strat_var,
                     size = number_of_ea_by_strata, 
                     method = "srswor", 
                     description = TRUE)  

  sample_1 <- cbind(dat_ea[sample_1$ID_unit, ea_var, with = FALSE], sample_1$Prob)
  colnames(sample_1) <- c(colnames(sample_1)[1], "eaweight")
  
  # Merge sample_1 and dat, selecting only selected eas
  dat_selected <- merge.data.table(dat, sample_1, by = ea_var, all.x = FALSE, all.y = TRUE)
  
  # Sample n2 households in each ea from dat_selected
  #dat_selected
  
  sample_2 <- sampling::strata(data = dat_selected, 
                     stratanames = ea_var,
                     size = rep(n2, sum(number_of_ea_by_strata)), 
                     method = "srswor", 
                     description = TRUE)
  
  sample_2 <- cbind(dat_selected[sample_2$ID_unit,], sample_2$Prob)
  
  # Calculate weight
  sample_2[, hhweight := 1/(eaweight * V2)]  
  sample_2[, V2 := NULL]
  sample_2[, eaweight := NULL]
  
  # Final weight adjustment within strata
  num_obs_pop_by_strata <- dat %>% count(by = eval(stratvar))
  colnames(num_obs_pop_by_strata) <- c(strat_var, "n")
  num_obs_sample_by_strata <- sample_2 %>% group_by(eval(stratvar)) %>% summarise((sum = sum(hhweight)))
  colnames(num_obs_sample_by_strata) <- c(strat_var, "sum_w")
  num_obs_comb <- merge(num_obs_pop_by_strata, num_obs_sample_by_strata, by = strat_var)
  num_obs_comb[, weight_factor := n/sum_w] # adjustment factor by strata
  
  sample_2 <- merge(sample_2, num_obs_comb[, c(strat_var, "weight_factor"), with = FALSE], by = strat_var)
  sample_2[, hhweight := hhweight * weight_factor]
  #sample_2[, hhweight := hhweight *  (nrow(dat) / sum(hhweight))]

  sample_2[, .(hid, hhweight)] # Return hid and hhweight  
}

#### Read in census data ####
synth_data_hh  <- as.data.table(read_dta("./training_data_household_census.dta"))
synth_data_ind <- as.data.table(read_dta("./training_data_individual_census.dta"))

dim(synth_data_hh) # 2,501,755 hhs
length(unique(synth_data_hh$geo1)) # 10 geo1
length(unique(synth_data_hh$geo2)) # 61 geo2
length(unique(synth_data_hh$ea))   # 5,940 eas

dim(synth_data_ind) # 10,003,891 individuals

colnames_hh  <- colnames(synth_data_hh)
colnames_ind <- colnames(synth_data_ind)

##### Draw hh sample #####
## Merge hh and ind level population files
synth_population <- merge(synth_data_hh, synth_data_ind, by = "hid")
rm(synth_data_ind) # remove ind data, as in synth_population

# Create stratification variable for both geo1 and urbrur
synth_data_hh[, stratvar := geo1 * 10 + urbrur]
table(synth_data_hh$stratvar) 
# 20 strata, smallest strata only 14,036 hhs

## Sample 1: Two-stage sample, n = 8,000, 25 in each ea
sample_1 <- two_stage_sample(size = 8000, 
                             ea_var = "ea", 
                             strat_var = "stratvar", # stratify by both urbrur and geo1
                             n2 = 25, 
                             dat = synth_data_hh, 
                             seed = 123)

# sample_1 only contains selected hids and weight
length(unique(sample_1$hid))

# save selected hid
saveRDS(sample_1, file = "sampled_hhs.rds")

# select sample from pop
sample_1_dat  <- right_join(synth_population, sample_1, by = "hid") 

# #### Replace hid with numeric hid ####
# problem with precisionnof float numbers > 16,777,215
# # hid includes information on geo2 -> intentionally
# setorder(sample_1_dat, cols = "geo1", "geo2")
# sample_1_dat[, hid_numeric_ea := rleid(hid), by = c("geo1", "geo2")]
# max(sample_1_dat$hid_numeric_ea)
# sample_1_dat[, hid_numeric    := 1000 * geo2 + hid_numeric_ea]
# 
# #View(sample_1_dat[, .(geo1, geo2, hid, hid_numeric, hid_numeric_ea)])
# 
# # save mapping hid and hid_numeric (idno in both pop and sample the same)
# saveRDS(unique(sample_1_dat[,.(hid, hid_numeric )]), "hid_mapping.rds")
# 
# # keep only numeric hid
# sample_1_dat[, hid := hid_numeric]
# sample_1_dat[, ':='(hid_numeric = NULL, hid_numeric_ea = NULL)]

#### Checks on variables ####
colnames(sample_1_dat)

## Geo areas
# Not all geo2 areas are sampled
table(sample_1_dat$geo2) #  missing 9, 21, 32

# Proportionate in geo1
round(100 * table(sample_1_dat$geo1) / table(synth_population$geo1), digits = 2)

## Weights
# Sum of weights by hh -> 2,501,755 == number of hhs in pop
sample_1_dat[, .SD[1,], by = hid][, sum(hhweight)]

# Sum of weights at pop level -> 10,092,120 != pop size (10,003,891)
sample_1_dat[, sum(hhweight)]

# Weighted number of ind and hh by geo1
setorder(synth_population, cols = "geo1", "geo2")

geo1_dist <- cbind(sample_1_dat[, sum(hhweight), by=.(geo1)],
                   synth_population[, .N, by=.(geo1)]) #ind
geo1_dist$prop <- geo1_dist$V1 / geo1_dist$N
geo1_dist

geo1_dist_hh <- cbind(sample_1_dat %>% distinct(hid, .keep_all = T) %>% 
  group_by(geo1) %>% summarise(sum(hhweight)),
synth_data_hh  %>% 
  group_by(geo1) %>% summarise(n()))

geo1_dist_hh$prop <- geo1_dist_hh$`sum(hhweight)` / geo1_dist_hh$`n()`
geo1_dist_hh

# Weighted number of ind and hh by geo2 -> not stratified/representative at geo2 level
# setorder(synth_population, cols = "geo1", "geo2")
# 
# geo2_dist <- cbind(sample_1_dat[, sum(hhweight), by=.(geo2)],
#                    synth_population[, .N, by=.(geo2)]) #ind
# geo2_dist$prop <- geo2_dist$V1 / geo2_dist$N
# geo2_dist
# 
# sample_1_dat %>% distinct(hid, .keep_all = T) %>% 
#   group_by(geo1) %>% summarise(sum(hhweight))
# synth_data_hh  %>% 
#   group_by(geo1) %>% summarise(n())
# 
# geo1_dist_hh <- cbind(sample_1_dat %>% distinct(hid, .keep_all = T) %>% 
#                         group_by(geo1) %>% summarise(sum(hhweight)),
#                       synth_data_hh  %>% 
#                         group_by(geo1) %>% summarise(n()))
# 
# geo1_dist_hh$prop <- geo1_dist_hh$`sum(hhweight)` / geo1_dist_hh$`n()`
# geo1_dist_hh

# Compare hhsize
#sample_1_dat[, ]

#### Recalculate sample quintiles ####
# Need to weigh by hhweight, but can leave out hhsize when done on sample_1_dat

# 1) Sort by pc_exp (per capita expenditures)
setorder(sample_1_dat, cols = "pc_exp")

# 2) Generate cumulative sum of all weights
sample_1_dat[, cum_weight        := cumsum(hhweight)]
sample_1_dat[, cum_weight_urbrur := cumsum(hhweight), by = urbrur] # urbrur
sample_1_dat[, cum_weight_urb    := cum_weight_urbrur] # urb
sample_1_dat[, cum_weight_rur    := cum_weight_urbrur] # rur
sample_1_dat[urbrur == 1, cum_weight_urb    := NA] # set urban to NA if rural
sample_1_dat[urbrur == 2, cum_weight_rur    := NA] # set rural to NA if urban

# 3) Quintile cut off points and pc_exp values
cut_offs <- 1:4 * (max(sample_1_dat$cum_weight) / 5) # cut off points
cut_offs

cut_offs_urb <- 1:4 * (max(sample_1_dat %>% filter(urbrur == 2) %>% select(cum_weight_urb)) / 5) # cut off points urban
#cut_offs_urb <- 1:4 * (max(sample_1_dat$cum_weight_urb, na.rm = T) / 5) # cut off points urban
cut_offs_urb
cut_offs_rur <- 1:4 * (max(sample_1_dat %>% filter(urbrur == 1) %>% select(cum_weight_rur)) / 5) # cut off points rural
#cut_offs_rur <- 1:4 * (max(sample_1_dat$cum_weight_rur, na.rm = T) / 5) # cut off points urban
cut_offs_rur

# national
pc_exp_vals <- c(sample_1_dat[min(which(sample_1_dat$cum_weight > cut_offs[1])), pc_exp],
                 sample_1_dat[min(which(sample_1_dat$cum_weight > cut_offs[2])), pc_exp],
                 sample_1_dat[min(which(sample_1_dat$cum_weight > cut_offs[3])), pc_exp],
                 sample_1_dat[min(which(sample_1_dat$cum_weight > cut_offs[4])), pc_exp])
pc_exp_vals
sample_1_dat[, quint_nat_new := fcase(pc_exp < pc_exp_vals[1], 1,
                                      pc_exp < pc_exp_vals[2], 2,
                                      pc_exp < pc_exp_vals[3], 3,
                                      pc_exp < pc_exp_vals[4], 4,
                                      pc_exp >= pc_exp_vals[4], 5)]

table(sample_1_dat$quint_nat_new)
table(sample_1_dat$quint_nat_new, sample_1_dat$quint_nat)

# rural (value 1)
pc_exp_vals_rur <- c(sample_1_dat[min(which(sample_1_dat$cum_weight_urbrur > cut_offs_rur[1] & sample_1_dat$urbrur == 1)), pc_exp],
                     sample_1_dat[min(which(sample_1_dat$cum_weight_urbrur > cut_offs_rur[2] & sample_1_dat$urbrur == 1)), pc_exp],
                     sample_1_dat[min(which(sample_1_dat$cum_weight_urbrur > cut_offs_rur[3] & sample_1_dat$urbrur == 1)), pc_exp],
                     sample_1_dat[min(which(sample_1_dat$cum_weight_urbrur > cut_offs_rur[4] & sample_1_dat$urbrur == 1)), pc_exp])
pc_exp_vals_rur

sample_1_dat[, quint_rur_new := fcase(pc_exp < pc_exp_vals_rur[1], 1,
                                      pc_exp < pc_exp_vals_rur[2], 2,
                                      pc_exp < pc_exp_vals_rur[3], 3,
                                      pc_exp < pc_exp_vals_rur[4], 4,
                                      pc_exp >= pc_exp_vals_rur[4], 5)]

sample_1_dat[urbrur == 2, quint_rur_new := NA] # set all urban to missing (NA)

table(sample_1_dat$quint_rur_new, useNA = "always")
table(sample_1_dat$quint_rur_new, sample_1_dat$quint_urb, useNA = "always")

# urban (value 2)
pc_exp_vals_urb <- c(sample_1_dat[min(which(sample_1_dat$cum_weight_urb > cut_offs_urb[1] & sample_1_dat$urbrur == 2)), pc_exp],
                     sample_1_dat[min(which(sample_1_dat$cum_weight_urb > cut_offs_urb[2] & sample_1_dat$urbrur == 2)), pc_exp],
                     sample_1_dat[min(which(sample_1_dat$cum_weight_urb > cut_offs_urb[3] & sample_1_dat$urbrur == 2)), pc_exp],
                     sample_1_dat[min(which(sample_1_dat$cum_weight_urb > cut_offs_urb[4] & sample_1_dat$urbrur == 2)), pc_exp])
pc_exp_vals_urb

sample_1_dat[, quint_urb_new := fcase(pc_exp < pc_exp_vals_urb[1], 1,
                                      pc_exp < pc_exp_vals_urb[2], 2,
                                      pc_exp < pc_exp_vals_urb[3], 3,
                                      pc_exp < pc_exp_vals_urb[4], 4,
                                      pc_exp >= pc_exp_vals_urb[4], 5)]

sample_1_dat[urbrur == 1, quint_urb_new := NA] # set all rural to missing (NA)

table(sample_1_dat$quint_urb_new, useNA = "always")
table(sample_1_dat$quint_urb_new, sample_1_dat$quint_urb, useNA = "always")

# Check quintiles in ind sample (summing weights)
sample_1_dat[, sum(hhweight), by = quint_nat_new]
sample_1_dat[, sum(hhweight), by = quint_urb_new]
sample_1_dat[, sum(hhweight), by = quint_rur_new]
sample_1_dat[, sum(hhweight), by = urbrur]

# Replace old values with new values and drop newly created vars
# Cut in two as to not replace attributes
sample_1_dat[1:nrow(sample_1_dat), quint_nat := quint_nat_new]
sample_1_dat[1:nrow(sample_1_dat), quint_urb := quint_urb_new]
sample_1_dat[1:nrow(sample_1_dat), quint_rur := quint_rur_new]

sample_1_dat[ ,`:=`(quint_nat_new = NULL, 
                    quint_urb_new = NULL,
                    quint_rur_new = NULL,
                    cum_weight = NULL,
                    cum_weight_urbrur = NULL,
                    cum_weight_rur = NULL,
                    cum_weight_urb = NULL)]
# Reorder data
setorderv(sample_1_dat, cols = c("geo2", "hid", "idno"))

#### Export data ####
# subset vars for distribution
# hh file
keepvars_h <- colnames_hh[!colnames_hh == "stratvar"]
keepvars_h <- c(keepvars_h, "hhweight") # add hhweight
sample_1_dat_hh <- sample_1_dat[,.SD[1], by = "hid"][, keepvars_h, with = FALSE]
sample_1_dat_hh[, popweight := hhsize * hhweight]

# Add variable names
attributes(sample_1_dat_hh$hid) <- list(label = "Unique household identifier",
                                        format.stata = "%12.0g")
attributes(sample_1_dat_hh$hhweight) <- list(label = "Household weight",
                                             format.stata = "%12.0g")
attributes(sample_1_dat_hh$popweight) <- list(label = "Population weight",
                                             format.stata = "%12.0g")
dim(sample_1_dat_hh)
colnames(sample_1_dat_hh)
write_dta(sample_1_dat_hh, "training_survey_data_hh.dta")

# ind file
keepvars_i <- c(colnames_ind, "hhweight")
sample_1_dat_ind <- sample_1_dat[, keepvars_i, with = FALSE]

# Add variable names
attributes(sample_1_dat_ind$hid) <- list(label = "Unique household identifier",
                                         format.stata = "%12.0g")
attributes(sample_1_dat_ind$hhweight) <- list(label = "Household weight",
                                             format.stata = "%12.0g")

dim(sample_1_dat_ind)
colnames(sample_1_dat_ind)
write_dta(sample_1_dat_ind, "training_survey_data_ind.dta")

# Check quintiles in hh sample
sample_1_dat_hh[, sum(popweight), by = quint_nat]
sample_1_dat_hh[, sum(popweight), by = quint_urb]
sample_1_dat_hh[, sum(popweight), by = quint_rur]

# Check min and max of quintiles
sample_1_dat_hh[, min(pc_exp), by = quint_nat]
sample_1_dat_hh[, max(pc_exp), by = quint_nat]
sample_1_dat_hh[, min(pc_exp), by = quint_urb]
sample_1_dat_hh[, max(pc_exp), by = quint_urb]
sample_1_dat_hh[, min(pc_exp), by = quint_rur]
sample_1_dat_hh[, max(pc_exp), by = quint_rur]

