


library(data.table)
library(csvread)
library(bit64)

#######################################
#script to clean data from scrapping 
#######################################


# load data from ian_gatech script
load('~/Documents/health_replication/npi_webinfo.Rdata')


# clean data with regular expresions adn build data set
npi=gsub('^.*enumeration_type\\s*|\\s*taxonomies.*$', '', a)
npi=gsub('^.*number\\s*|\\s*other_names.*$', '', npi)
npi= gsub("[^0-9\\.]", "", npi) 
npi=as.int64(npi)
npi_wdata=as.data.frame(npi)
zip=gsub('^.*address_1\\s*|\\s*basic.*$', '', a)
state=gsub('^.*state\\s*|\\s*telephone_number.*$', '', zip)
zip=gsub('^.*postal_code\\s*|\\s*state.*$', '', zip)
state=gsub("[^[:alpha:]\\.]", "", state)
zip= gsub("[^0-9\\.]", "", zip) 
taxo=gsub('^.*taxonomies\\s*|\\s*license.*$', '', a)
desc_tax= gsub('^.*desc\\s*', '', taxo)
taxo=gsub('^.*code\\s*|\\s*desc.*$', '', taxo)
taxo= gsub("[^[:alnum:]\\.]", "", taxo) 
desc_tax=gsub("[^[:alpha:][:space:]|/\\.]", "", desc_tax)
desc_tax=gsub("^ *|(?<= ) | *$", "", desc_tax, perl=T)
#populate data frame
npi_wdata$zip=zip
npi_wdata$state=state
npi_wdata$t_code=taxo
npi_wdata$desc_tax=desc_tax
colnames(npi_wdata)[colnames(npi_wdata)=="x"] <- "npi"
#(optional) remove all objects created
rm( desc_tax,  taxo, zip, state, npi)

#remove large web object

rm(a)

write.csv(npi_wdata, file = '~/Documents/health_replication/npi_wdata.csv',row.names = F)

