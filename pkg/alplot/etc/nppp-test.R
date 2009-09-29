library(nicholsonppp)
L <- sim.drift.selection()
e <- list(freq=list(L$sim,c("locus","population","generation")))
df <- L$s[,6:17]
e <- c(e,list(color=list(ff(unlist(df),dim=dim(df)),c("locus","population"))))
for(N in names(L$s)[1:5])e <- c(e,structure(list(list(L$s[,N],"locus")),names=N))
## describe this crazy object we have created:
sapply(e,function(l){x <- l[[1]];list(length(x),dim(x),mode(x),class(x))})
str(e)
## now make a function that constructs a minimal lattice df, with ids,
## appropriate prepanel,panel,etc. functions, etc.

## need to find an R function for doing this programmatically [,,foo,]
