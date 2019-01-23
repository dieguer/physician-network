rm(list=ls())
library(rvest)
library('data.table')
install.packages('stringr')
library('RCurl')
library(parallel)
library(doParallel)
library('snow')
library(httr)
library('XML')
i


docs2=c("1003000100" ,"1003000126", "1003000134" ,"1003000142", "1003000159", "1003000167")


docss=credential$npi

getwd()

setwd("//Client/D$/health_replication")

cl<-makeCluster(40)
clusterEvalQ(cl, {require(stringr)})
clusterEvalQ(cl, {require(RCurl)})
clusterExport(cl,'docss')
registerDoParallel(cl)
gp=Sys.time()
a<-foreach(i=1:length(docss)) %dopar% {
  
  str_squish( getURL(paste0('https://npiregistry.cms.hhs.gov/api/?number=',docss[i],'&enumeration_type=&taxonomy_description=&first_name=&last_name=&organization_name=&address_purpose=&city=&state=&postal_code=&country_code=&limit=&skip=&pretty=on')))
}
gp=Sys.time()-gp
stopCluster(cl)

save(a,file='npi_webinfo')


#  y=gsub('.*\"taxonomies\"',"",getURL(paste0('https://npiregistry.cms.hhs.gov/api/?number=',docss[i],'&enumeration_type=&taxonomy_description=&first_name=&last_name=&organization_name=&address_purpose=&city=&state=&postal_code=&country_code=&limit=&skip=&pretty=on')) ) 


cl<-makeCluster(40)
clusterEvalQ(cl, {require(XML)})
clusterEvalQ(cl, {require(rvest)})
clusterEvalQ(cl, {require(httr)})
clusterEvalQ(cl, {require(RCurl)})
clusterExport(cl,'docss')
gp=Sys.time()
dats<-parLapply(cl, docss, function(j){
getURL(paste0('https://npiregistry.cms.hhs.gov/api/?number=',j,'&enumeration_type=&taxonomy_description=&first_name=&last_name=&organization_name=&address_purpose=&city=&state=&postal_code=&country_code=&limit=&skip=&pretty=on'))
})
gp=gp-Sys.time()
gp
stopCluster(cl)



a=dats[[100]]
a=htmlTreeParse(a, asText = T)


size=unlist(strsplit(unlist(size),'\n'))
a=unlist(strsplit(a,'\\:'))
a=gsub('\\\"','',a)
a=unlist(strsplit(a,'\\\\(.*?)\\\\'))
html_children(a)







ss=s[613]





b=gregexpr('\n', s)  

c=c()
for (i in 1:1000) {
c[i]=  length(b[[i]])
}



gregexpr('\n', a[2])[[1]]

gregexpr("\n", size)[[1]][3]


library('doParallel')


library(doParallel)  

getq=function(x){
  require(rvest)
  h=read_html(paste('https://npiregistry.cms.hhs.gov/api/?number=',x,'&enumeration_type=&taxonomy_description=&first_name=&last_name=&organization_name=&address_purpose=&city=&state=&postal_code=&country_code=&limit=&skip=&pretty=on',sep=''))
  h$children$html
  return(h)
  }

cl <- makeCluster(30)  
registerDoParallel(cl)  
result <- foreach(i=1:100) %dopar% getq(docss[i])
stopCluster(cl)
















read_

ss=lapply(dats,function(s) { 
  read_html(s)
})


table= html_node(dat[1],xpath = '//*[@id="top"]/div/div[2]/div[3]')

html_text(dat[[1]])

for(i in 1:length(docs)){
  Sys.sleep(0.5)
  first_page <- read_html(paste0('https://npiregistry.cms.hhs.gov/registry/provider-view/',docs[i]))
  
  table=html_node(first_page,xpath = '//*[@id="top"]/div/div[2]/div[3]')
  
  table=htmlTreeParse(table,useInternalNodes = T)
  
  A=xpathSApply(table,'//tr',xmlValue)
  
  
  
  g=function(x){ 
    y=gsub('\t',"", x)
    y=gsub('\n'," ", y)
    y=gsub('\\s+'," ", y)
  } 
  B=unlist(lapply(A, g ))
  
  B=unlist(lapply(B, trimws ))
  
  store[[i]]=B
  
}
