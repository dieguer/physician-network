library('data.table')
library(gdata)
library('tidyverse')
library(csvread)
library(bit64)
library(sf)
library(spdep)
library(doParallel)
library(foreach)
#library(ggplot2)
#install.packages('spdep', dependencies = T)


##################################################################
#           PREPARE CROSSWALK DATA
##################################################################
cw15=read.xls('~/Documents/health_replication/ZipHsaHrr15 (1).xls')
cw16=read.xls('~/Documents/health_replication/ZipHsaHrr16.xls')

cw15$zipcode15=as.character( cw15$zipcode15)

i=0
while(i<length(cw15$zipcode15)){
  d=nchar(cw15$zipcode15)==5
  cw15$zipcode15[!d]= paste0('0', cw15$zipcode15[!d] )
  s=table(d)
  i=s[length(s)]
}                      

cw16$zipcode16=as.character( cw16$zipcode16)

i=0
while(i<length(cw16$zipcode16)){
  d=nchar(cw16$zipcode16)==5
  cw16$zipcode16[!d]= paste0('0', cw16$zipcode16[!d] )
  s=table(d)
  i=s[length(s)]
  
}                           


##################################################################
#           Physcian Compare etc
##################################################################


d15=fread('~/Documents/health_replication/Physician_Compare_2015.csv',header = T, sep=',')
d16=fread('~/Documents/health_replication/Physician_Compare_2016.csv',header = T, sep=',')

zip_shp15 <- st_read( "~/Documents/health_replication/shps/2015/HRR_Bdry.SHP")

#plot(zip_shp15)

id=as.data.frame(zip_shp15)$HRR_BDRY_I
hrrnum=as.data.frame(zip_shp15)$HRRNUM
hrrdb=cbind.data.frame(id, hrrnum)
Wmat=poly2nb(zip_shp15, queen = F)


Wmat=lapply(Wmat, function(x) {
  x=unlist( 
    lapply( x, function(y) {
      y=hrrdb$hrrnum[which(hrrdb$id==y)]
    })
    
  )
})





namo=names(d15)
namo=gsub('\\s', '_', namo)
names(d15)=namo
namo=names(d16)
namo=gsub('\\s', '_', namo)
names(d16)=namo





#subset(d15,  rownames(d15)==c(1,2,3,4),select = c('NPI', 'Last_Name', 'First_Name','Organization_legal_name','Zip_Code') )



interest=fread('~/Documents/health_replication/npi_wdata.csv',header = T, sep=',')

d15$NPI=as.integer64(d15$NPI)
d15=left_join(d15,interest,by=c("NPI" = "npi"),suffix = c("",".1"))
d15$zip5=substr(d15$zip,1,5)
d15$Zip_Code=substr(d15$Zip_Code,1,5)



d16$NPI=as.integer64(d16$NPI)
d16=left_join(d16,interest,by=c("NPI" = "npi"),suffix = c("",".1"))
d16$zip5=substr(d16$zip,1,5)
d16$Zip_Code=substr(d16$Zip_Code,1,5)




d15=left_join(d16,subset(cw15,select = c('zipcode15','hrrnum')),by=c("zip5" = "zipcode15"),suffix = c("","_15"))
d16=left_join(d16,subset(cw16,select = c('zipcode16','hrrnum')),by=c("zip5" = "zipcode16"),suffix = c("","_16"))


d15=subset(d15,grepl('^207R|^208D|^207Q|^208D',t_code) | grepl('^207X', t_code)) 
d16=subset(d16,grepl('^207R|^208D|^207Q|^208D',t_code) | grepl('^207X', t_code)) 


########################################################
#
#       Uncomment to run diagnostiques on uniqueness
#
#####################################################
# d15$hrrnum_f=as.factor(d15$hrrnum)
# count(unique(d15['NPI']))
# 
# 
# u=unique(d15[c('NPI')])
# 
# raros=integer64(l[1])
# 
# cl=makeCluster(5)
# registerDoParallel(cl)
# 
# raros15=foreach(i=1:l[1]) %dopar% {
#  samp=d15[d15$NPI==u$NPI[i],47]
#   all(samp[1] == samp) 
# }
# stopCluster(cl)
# 
# raros15=unlist(raros15)
# u=unique(d16[c('NPI')])
# 
# 
# cl=makeCluster(5)
# registerDoParallel(cl)
# 
# raros16=foreach(i=1:l[1]) %dopar% {
#   samp=d16[d16$NPI==u$NPI[i],47]
#   raros16[i]= all(samp[1] == samp) 
# }
# 
# stopCluster(cl)
# 
# 
# raros16=unlist(raros16)
# 
# table(raros16)

u15=unique(d15[c('NPI' ,'hrrnum')])
u16=unique(d16[c('NPI' ,'hrrnum')])

dbase=inner_join(u15,u16, by=c("NPI" = "NPI"),suffix = c("_16","_15"))

dbase$movers= dbase$hrrnum_15 != dbase$hrrnum_16



names(Wmat)=hrrdb$hrrnum

dbase$movers_nc= if_else(dbase$movers, true = !is.element(dbase$hrrnum_15,Wmat[as.character(dbase$hrrnum_16)]),FALSE  )


fwrite(dbase, file = '~/Documents/health_replication/ph_comp_2016.csv',row.names = F)

rm(list = ls())

data=fread( file = '~/Documents/health_replication/ph_comp_2016.csv')


