object.size(npi)
g="\is"

gsub("\\\\", "", g)


d=a[1:3]

nchar(r[1000000])



###########################
#FAILED parallel
###################

# setup parallel
cores <- 4
system <- Sys.info()['sysname']

cl <- NULL
if (system == 'Windows') {
  cl <- makeCluster(getOption('cl.cores', cores))
  registerDoParallel(cl)
  registerDoSEQ()
  on.exit(stopCluster(cl))
} else {
  options('mc.cores' = cores)
  registerDoParallel(cores)
}




'^.*enumeration_type\\s*|\\s*taxonomies.*$'
rempar=function(lista,bs,nucleo,palabra,swap){
  blocksize <- bs
  blocks <- floor((length(lista)) / blocksize)+1
  res <- character(length(lista))
  for (l in 1:blocks)
  {
    
    if (l==1)
    {
      print(paste("Processing ", blocksize, "  batch ", l, " of ", blocks, sep=""))
      time1=Sys.time()
      res[1:(blocksize*l)] <- mclapply(lista[1:(blocksize*l)],
                                       function(x) gsub(palabra, swap, x)
                                       ,mc.cores = nucleo )
    }
    
    # final block - will be executed just once
    if (l==blocks)
    {
      print(paste("Processing final batch ", l, " of ", blocks, sep=""))
      print(paste("Index1: ", (1+(blocksize*(l-1))), "; ", "Index2: ", length(a), sep=""))
      res[(1+(blocksize*(l-1))):length(lista)] <- mclapply(lista[(1+(blocksize*(l-1))):length(lista)], 
                                                           function(x) gsub(palabra, swap, x)
                                                           ,mc.cores = nucleo)
      print(Sys.time()-time1) 
      return(res)
    }
    
    # any other blocks - executed any number of times between first and final block
    if (l>1 && l<blocks)
    {
      #if (l%%30==0){ print(paste("Processing ", blocksize, " formulae, batch ", l, " of ", blocks, sep=""))}
      #if (l%%30==0){ print(paste("Index1: ", (1+(blocksize*(l-1))), "; ", "Index2: ", (blocksize*l), sep=""))}
      res[(1+(blocksize*(l-1))):(blocksize*l)] <- mclapply(lista[(1+(blocksize*(l-1))):(blocksize*l)], function(x) gsub('^.*enumeration_type\\s*|\\s*taxonomies.*$', '', x)
                                                           ,mc.cores = nucleo)
      
    }
    
  }
}


