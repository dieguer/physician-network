library('data.table')
library('tidyverse')
library(bit64)
library(sf)
library(spdep)

#library(ggplot2)
#install.packages('spdep', dependencies = T)

d16=fread('~/Documents/health_replication/newdata/Physician Compare Data/Refresh_Data_Archive_12_2016.csv',header = F, sep=',')
d15=fread('~/Documents/health_replication/newdata/Physician Compare Data/Refresh_Data_Archive_12_2015.csv',header = T, sep=',')
d14=read.csv('~/Documents/health_replication/newdata/Physician Compare Data/Refresh_Data_Archive_12_2014.csv',header = T, sep=',', stringsAsFactors = F)


d16_o=fread('~/Documents/health_replication/Physician_Compare_2016.csv',header = T, sep=',')

# Clean 2016 data
namo=names(d16_o)
namo=gsub('\\s', '_', namo)
names(d16_o)=namo

#keep variables that we know are the same for certain

d16 =  dplyr::select(d16,-(39:42))
d16_o= dplyr::select(d16_o,-(39:41)) 


#rename variables
namo=names(d16_o)
names(d16) = namo

namo=names(d15) 
namo=gsub('\\s', '_', namo)
names(d15)=namo

namo=names(d14) 
namo=gsub('\\.', '_', namo)
names(d14)=namo


#adjust to drop objects
#ob=ls()
#rm(list =ob[-:-4] )


#create working data sets

test1=subset(d16,select=c('NPI','Group_Practice_PAC_ID', 'City', 'State', 'Zip_Code'))
test2=subset(d15,select=c('NPI','Group_Practice_PAC_ID', 'City', 'State', 'Zip_Code'))
test3=subset(d14,select=c('NPI','Group_Practice_PAC_ID', 'City', 'State', 'Zip_Code'))

test2$NPI=as.integer(test2$NPI)
test3$NPI=as.integer(test3$NPI)


#set index for merge between 14-16 and 15-16

setDT(test3)
setkeyv(test1,cols =c('NPI'))
setkeyv(test2,cols =c('NPI'))
setkeyv(test3,cols =c('NPI'))



res1=merge(test1,test2,all=T, nodups=F, allow.cartesian=T) #15-16
res2=merge(test1,test3,all=T, nodups=F, allow.cartesian=T) #14-16

####  IMPOrtatn ALL datasets indexed 1 are related to 15-16 info. 
####  IMPOrtatn ALL datasets indexed 2 are related to 14-16 info. 

res1$Zip_Code.x=substr(res1$Zip_Code.x,1,5)
res1$Zip_Code.y=substr(res1$Zip_Code.y,1,5)

res2$Zip_Code.x=substr(res2$Zip_Code.x,1,5)
res2$Zip_Code.y=substr(res2$Zip_Code.y,1,5)

#################
# READ CROSSWALKFILE
################

cw14=read.xls('~/Documents/health_replication/ZipHsaHrr14.xls')
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

cw14$zipcode14=as.character( cw14$zipcode14)

i=0
while(i<length(cw14$zipcode14)){
  d=nchar(cw14$zipcode14)==5
  cw14$zipcode14[!d]= paste0('0', cw14$zipcode14[!d] )
  s=table(d)
  i=s[length(s)]
  
}                           


res1=left_join(res1,subset(cw16,select = c('zipcode16','hrrnum')),by=c("Zip_Code.x" = "zipcode16"),suffix = c("","_16"))
res1=left_join(res1,subset(cw15,select = c('zipcode15','hrrnum')),by=c("Zip_Code.y" = "zipcode15"),suffix = c("","_15"))

res2=left_join(res2,subset(cw16,select = c('zipcode16','hrrnum')),by=c("Zip_Code.x" = "zipcode16"),suffix = c("","_16"))
res2=left_join(res2,subset(cw14,select = c('zipcode14','hrrnum')),by=c("Zip_Code.y" = "zipcode14"),suffix = c("","_14"))


#########################
# Back to analysis
########################



# group observations 
res1=mutate(res1, ID=group_indices_(res1, .dots=c('NPI', 'hrrnum')))

res2=mutate(res2, ID=group_indices_(res2, .dots=c('NPI', 'hrrnum')))


# create variable for change in hrr
res1 = res1 %>% mutate(change= hrrnum==hrrnum_15)
res2 = res2 %>% mutate(change= hrrnum==hrrnum_14)

res1= res1 %>%filter(!is.na(change))
res2= res2 %>%filter(!is.na(change))


#establish number of coincidences
res1= res1 %>% group_by(ID) %>% mutate(n=sum(change, na.rm = T)) 
res2= res2 %>% group_by(ID) %>% mutate(n=sum(change, na.rm = T)) 


# build flags

res1=res1 %>% 
  group_by(NPI, hrrnum_15) %>% 
  mutate(dum=any(change== 1)  )

res2=res2 %>% 
  group_by(NPI, hrrnum_14) %>% 
  mutate(dum=any(change== 1)  )


res1= res1 %>% group_by(NPI) %>% mutate(nn=sum(n, na.rm = T)>0) 
res2= res2 %>% group_by(NPI) %>% mutate(nn=sum(n, na.rm = T)>0) 




res1= res1 %>% filter(n==0 )
res2= res2 %>% filter(n==0)


resu1=res1 %>% dplyr::select(NPI, n, nn)  %>% group_by(NPI) %>% summarize(moves=max(n), overlap=max(nn)) %>%mutate(moves=moves==0,overlap=overlap==1)  

resu2=res2 %>% dplyr::select(NPI, n, nn)  %>% group_by(NPI) %>% summarize(moves=max(n), overlap=max(nn)) %>%mutate(moves=moves==0,overlap=overlap==1)  
  

lista=ls()
lista=lista[!lista %in% c('res1', 'res2', 'resu1', 'resu2') ]
rm(list=lista)


resu1$NPI=as.integer64(resu1$NPI)
resu2$NPI=as.integer64(resu2$NPI)

#bring data from the web scrappin results

interest=fread('~/Documents/health_replication/npi_wdata.csv',header = T, sep=',')


interest1=left_join(interest, subset(resu1,select = c('NPI','moves', 'overlap')),by=c("npi" = "NPI"))
interest2=left_join(interest, subset(resu2,select = c('NPI','moves', 'overlap')),by=c("npi" = "NPI"))

#filter general physicians and OP surgeons
interest1=interest1 %>% filter(moves) %>% 
  filter(grepl('^207R|^208D|^207Q|^208D',t_code) | grepl('^207X', t_code))

interest2=interest2 %>% filter(moves) %>% 
  filter(grepl('^207R|^208D|^207Q|^208D',t_code) | grepl('^207X', t_code))

#write data into tables

fwrite(interest1, file = '~/Documents/health_replication/movers2014.csv',row.names = F)
fwrite(interest2, file = '~/Documents/health_replication/movers2015.csv',row.names = F)
