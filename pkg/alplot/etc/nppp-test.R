library(nicholsonppp)
library(ff)
L <- sim.drift.selection(array=ff,loci=1,p.neutral=1/2)
df <- L$s[,6:17]
colors <- ff(unlist(df),dim=dim(df),dimnames=list(locus=NULL,population=NULL))
e <- list(freq=L$sim,color=colors)
for(N in c("s","type","ancestral","S"))e <- c(e,structure(list(array(L$s[,N],dim=nrow(L$s),dimnames=list(locus=NULL))),names=N))
## describe this crazy object we have created:
sapply(e,function(x)list(length(x),dim(x),mode(x),class(x)))
str(e)
str(lapply(e,dimnames))
## now make a function that constructs a minimal lattice df, with ids,
## appropriate prepanel,panel,etc. functions, etc.

## need to find an R function for doing this programmatically [,,foo,]


## all populations for a given locus
col <- gd(e,"color",list(locus=5))
## Example: return all frequencies for a given locus
freq <- gd(e,"freq",list(locus=5))
## This should generate an allele frequency over time plot:
myplot(freq~generation|locus,e,groups=population,type="l")
