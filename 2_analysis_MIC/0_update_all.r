rm(list=ls())

print("step 1...")
source("1_find_MIC_quantiles.r")

print("step 2...")
source("2_find_scaling_parameters.r")

print("step 3...")
source("3_find_MIClowest.r")
