library(bigmemory)
library(biganalytics)
library(bigtabulate)
library(ff)
library(data.table)
library(regex)
library(tidyverse)
library(dplyr)
library(igraph)
library(rgexf)



getwd()

setwd('D:/health_replication')
setwd('//Client/D$/health_replication')
rm(data)
data=fread('pspp2014_30.csv', header = T, sep = ',')
enum=fread('core.csv', header = T, sep = ',')

taxonomy=fread('ptaxcode20188.csv', header = T, sep = ',')
taxonomy$ptaxcode= gsub(" ", "", taxonomy$ptaxcode)
taxonomy$doctor=substr(taxonomy$ptaxcode, 1, 2)
taxonomy$doctors=substr(taxonomy$ptaxcode, 1, 2)=='20'
taxonomy=taxonomy[order(npi),]
taxonomy=subset(taxonomy,pprimtax=='Y'  )



credential=subset(enum,entity==1,select = c('npi', 'pcredential','pcredentialoth', 'replacement_npi','npideactdate', 'npireactdate'))
rm(enum)


credential$pcredential= gsub(" ", "", credential$pcredential)

#provitional filter
credential$dummy=grepl("M.D.|MD.|MD|M.D",credential$pcredential)
credential$dummy2=grepl("M",credential$pcredential)
credential$dummy3=grepl("D.M.D.|D.M.D|DMD.|DMD|DM.D|D.MD",credential$pcredential) 
credential$dummy4=grepl("(PHARMA|PH)",credential$pcredential) 
credential$dummy5= credential$dummy & !credential$dummy3 & !credential$dummy4

credential=subset(credential, select = c("npi", 'dummy5'))



matched=left_join(data, credential,by=c("npi1" = "npi"),suffix = c("",".1"))
matched=left_join(matched, credential,by=c("npi2" = "npi"),suffix = c("",".2"))

matched2=subset(matched, dummy5==T & dummy5.2==T, select = c(npi1,npi2) )

matched2=data.table(matched2)

set.seed(1234)

sampled=matched2[sample(.N, 10000)]

sm_mat=as.matrix(sampled)
net=graph_from_edgelist(sm_mat, directed = F)

#install.packages('intergraph')
#install.packages('network')
#install.packages('statnet')
library(network)
library('intergraph')
library(statnet)
summary(net)



net=graph_from_data_frame(sampled, directed = F)

neto=asNetwork(net)

plot(neto)
plot(net)


summary(neto)

nety=network(neto)
nety %v% 'vertex.names'

table(degree(nety , gmode='graph'))
table(closeness(nety , gmode='graph'))
table(betweenness(nety , gmode='graph'))

cliques(net,min=3)

net_gef=igraph.to.gexf(net, position=NULL)

# construct the nodes and edges data for gexf conversion
nodes <- data.frame(cbind(V(net), as.character(V(net))))
edges <- t(Vectorize(get.edge, vectorize.args='id')(net, 1:ecount(net)))

# do the conversion
write.gexf(nodes, edges) 





