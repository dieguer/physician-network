library(ff)
library(data.table)
library(tidyverse)
library(csvread)
library(bit64)

#find doctors 
data=fread('~/Documents/health_replication/shared_pp_data_2014.csv', header = T, sep = ',')

#keep orthpeadic surgeons 700k
data=subset(data, grepl('^207X', t_code) | grepl('^207X', t_code_npi2) )

#keep general physicians (roughly) 130k
data=subset(data,  grepl('^207R|^208D|^207Q|^208D',t_code) | grepl('^207R|^208D|^207Q|^208D',t_code))


