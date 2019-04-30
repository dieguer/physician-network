library(ff)
library(data.table)
library(tidyverse)
library(csvread)
library(bit64)
#require(doParallel)
#library(Rmpi)

#script to join information

wdata=fread('~/Documents/health_replication/npi_wdata.csv',header = T, sep=',')

#data=fread('~/Documents/health_replication/pspp2014_30.csv', header = T, sep = ',')
data=fread('~/Documents/health_replication/pspd2016/root_npi_graph_2016.csv', header = T, sep = ',')
names(data)=c('npi1', 'npi2', 'pat_count')
enum=fread('~/Documents/health_replication/core.csv', header = T, sep = ',')
enum$npi=as.int64(enum$npi)

credential=subset(enum,entity==1,select = c('npi', 'pcredential'))
rm(enum)

gc()

credential$pcredential= gsub(" ", "", credential$pcredential)


matched=inner_join(data, credential,by=c("npi1" = "npi"),suffix = c("",".1"))
matched=inner_join(matched, credential,by=c("npi2" = "npi"),suffix = c("",".2"))
matched=subset(matched,grepl("^M.D.$|^MD.$|^MD$|^M.D$",pcredential))
matched=subset(matched,grepl("^M.D.$|^MD.$|^MD$|^M.D$",pcredential.2))
rm(credential)
rm(data)

#colnames(data)[colnames(data)=="old_name"] <- "new_name"
matched=left_join(matched, wdata,by=c("npi1" = "npi"),suffix = c("","_npi1"))
matched=left_join(matched, wdata,by=c("npi2" = "npi"),suffix = c("","_npi2"))


drops <- c("year","days","begdate","enddate","pcredential","pcredential.2")
matched=matched[ , !(names(matched) %in% drops)]


fwrite(matched, file = '~/Documents/health_replication/shared_pp_data_2016.csv',row.names = F)



