library(nicholsonppp)
library(ff)
L <- sim.drift.selection(array=ff,loci=1,p.neutral=1/2)
e <- list(freq=list(L$sim,c("locus","population","generation")))
df <- L$s[,6:17]
e <- c(e,list(color=list(ff(unlist(df),dim=dim(df)),c("locus","population"))))
for(N in c("s","type","ancestral","S"))e <- c(e,structure(list(list(L$s[,N],"locus")),names=N))
## describe this crazy object we have created:
sapply(e,function(l){x <- l[[1]];list(length(x),dim(x),mode(x),class(x))})
str(e)
sapply(e,function(l)l[[2]])
## now make a function that constructs a minimal lattice df, with ids,
## appropriate prepanel,panel,etc. functions, etc.

## need to find an R function for doing this programmatically [,,foo,]


## all populations for a given locus
col <- gd(e,"color",locus=5)
## Example: return all frequencies for a given locus
freq <- gd(e,"freq",locus=5)
## This should generate an allele frequency over time plot:
myplot(freq~generation|locus,e,groups=population)
