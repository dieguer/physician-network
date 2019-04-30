library('data.table')
library(gdata)
library('tidyverse')
library(csvread)
library(bit64)
library(sf)
library(spdep)
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

plot(zip_shp15)

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


#d15=arrange(d15,NPI)


#subset(d15,  rownames(d15)==c(1,2,3,4),select = c('NPI', 'Last_Name', 'First_Name','Organization_legal_name','Zip_Code') )




setsel=c("NPI", "Zip_Code", "Primary_specialty"                                                
         ,"Secondary_specialty_1"                                            
        ,"Secondary_specialty_2"                                            
        ,"Secondary_specialty_3"                                            
       ,"Secondary_specialty_4" )

dbase=inner_join(subset(d15,select=c("NPI", "Zip_Code")),subset( d16,select=setsel),by=c("NPI" = "NPI"),suffix = c("",".1"))
rm(d15)
rm(d16)
gc()

#dbase=subset(dbase, Zip_Code!=Zip_Code.1)

interest=fread('~/Documents/health_replication/npi_wdata.csv',header = T, sep=',')

dbase$NPI=as.integer64(dbase$NPI)
dbase=left_join(dbase,interest,by=c("NPI" = "npi"),suffix = c("",".1"))
dbase$zip5=substr(dbase$zip,1,5)
dbase$Zip_Code=substr(dbase$Zip_Code,1,5)
dbase$Zip_Code.1=substr(dbase$Zip_Code.1,1,5)

dbase=left_join(dbase,subset(cw15,select = c('zipcode15','hrrnum')),by=c("Zip_Code" = "zipcode15"),suffix = c("","_15"))
dbase=left_join(dbase,subset(cw16,select = c('zipcode16','hrrnum')),by=c("Zip_Code.1" = "zipcode16"),suffix = c("","_16"))

dbase$movers= dbase$hrrnum != dbase$hrrnum_16

names(Wmat)=hrrdb$hrrnum

dbase$movers_nc= if_else(dbase$movers, true = !is.element(dbase$hrrnum,Wmat[as.character(dbase$hrrnum_16)]),FALSE  )


fwrite(dbase, file = '~/Documents/health_replication/ph_comp_2016.csv',row.names = F)

data=fread(dbase, file = '~/Documents/health_replication/ph_comp_2016.csv',row.names = F)

data=subset(dbase,  grepl('^207R|^208D|^207Q|^208D',t_code) | grepl('^207R|^208D|^207Q|^208D',t_code))




